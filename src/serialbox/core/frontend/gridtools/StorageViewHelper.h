//===-- serialbox/core/frontend/gridtools/StorageViewHelper.h -----------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains helper functions to create StorageViews of gridtools::storages.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_STORAGE_VIEW_HELPER_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_STORAGE_VIEW_HELPER_H

#include <boost/mpl/max_element.hpp>
#include <utility>
#include <vector>

namespace serialbox {

namespace gridtools {

namespace internal {

//===------------------------------------------------------------------------------------------===//
//     Strides
//===------------------------------------------------------------------------------------------===//

template <typename LayoutMap, typename Container>
int get_stride_helper(int coord, const Container& container) noexcept {
  int max_value = boost::mpl::deref<
      typename boost::mpl::max_element<typename LayoutMap::layout_vector_t>::type>::type::value;

  const int value = coord < (int)LayoutMap::length ? LayoutMap::layout_vector[coord] : -1;
  return (max_value < 0) ? 0 : (value == max_value ? 1 : container[value + 1]);
}

template <typename MetaDataType, typename StorageType>
std::vector<int> get_strides(const StorageType&, const MetaDataType& meta_data) {
  const auto& strides_array = meta_data.m_strides;
  const int n_dimensions = meta_data.dims().n_dimensions;

  std::vector<int> strides_vec(n_dimensions);
  for(int i = 0; i < n_dimensions; ++i)
    strides_vec[i] = internal::get_stride_helper<typename StorageType::storage_info_type::layout>(
        i, strides_array);

  return strides_vec;
}

//===------------------------------------------------------------------------------------------===//
//     Dimensions
//===------------------------------------------------------------------------------------------===//

template <typename MetaDataType>
std::vector<int> get_dims(const MetaDataType& meta_data) noexcept {
  const auto& unaligned_dims_array = meta_data.m_unaligned_dims;
  const int n_dimensions = meta_data.dims().n_dimensions;

  std::vector<int> dims_vec(n_dimensions);
  for(int i = 0; i < n_dimensions; ++i)
    dims_vec[i] = unaligned_dims_array[i];

  return dims_vec;
}

//===------------------------------------------------------------------------------------------===//
//     OriginPtr
//===------------------------------------------------------------------------------------------===//

template <typename LayoutMap>
bool has_stride_one(unsigned int coord) noexcept {
  int max_value = boost::mpl::deref<
      typename boost::mpl::max_element<typename LayoutMap::layout_vector_t>::type>::type::value;
  return (LayoutMap::layout_vector[coord] == max_value);
}

template <typename LayoutMap, unsigned int Alignment, typename Halo>
int left_padding_helper(unsigned int coord) noexcept {
  unsigned int lpad = Halo::get(coord);
  return (Alignment && has_stride_one<LayoutMap>(coord)) ? (Alignment - lpad) % Alignment : 0;
}

template <typename StorageType, class MetaDataType>
void* get_origin_ptr(const StorageType& storage, const MetaDataType& meta_data,
                     unsigned int field_idx) noexcept {
  const int n_dimensions = meta_data.dims().n_dimensions;
  const auto& strides_array = meta_data.m_strides;

  auto* data_ptr = storage.fields()[field_idx].get();

  for(int i = 0; i < n_dimensions; ++i) {
    int lpad = left_padding_helper<typename StorageType::storage_info_type::layout,
                                   StorageType::storage_info_type::s_alignment,
                                   typename StorageType::storage_info_type::halo_t>(i);
    int stride = internal::get_stride_helper<typename StorageType::storage_info_type::layout>(
        i, strides_array);
    data_ptr += lpad * stride;
  }

  return static_cast<void*>(data_ptr);
}

} // namespace internal

} // namespace gridtools

} // namespace serialbox

#endif
