//===-- serialbox/Core/Frontend/gridtools/StorageViewHelper.h -----------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_STORAGEVIEWHELPER_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_STORAGEVIEWHELPER_H

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

  const int value = coord < (int) LayoutMap::length ? LayoutMap::layout_vector[coord] : -1;
  return (max_value < 0) ? 0 : (value == max_value ? 1 : container[value + 1]);
}

template <typename Storage>
std::vector<int> get_strides(const Storage& storage) {
  auto strides_array = storage.meta_data().m_strides;
  const int n_dimensions = storage.meta_data().dims().n_dimensions;

  std::vector<int> strides_vec;
  for(int i = 0; i < n_dimensions; ++i)
    strides_vec.push_back(
        internal::get_stride_helper<typename Storage::storage_info_type::layout>(i, strides_array));

  return strides_vec;
}

//===------------------------------------------------------------------------------------------===//
//     Dimensions
//===------------------------------------------------------------------------------------------===//

template <typename Storage>
std::vector<int> get_dims(const Storage& storage) noexcept {
  auto unaligned_dims_array = storage.meta_data().m_unaligned_dims;
  const int n_dimensions = storage.meta_data().dims().n_dimensions;

  std::vector<int> dims_vec;
  for(int i = 0; i < n_dimensions; ++i)
    dims_vec.push_back(unaligned_dims_array[i]);

  return dims_vec;
}

//===------------------------------------------------------------------------------------------===//
//     Padding
//===------------------------------------------------------------------------------------------===//

template <typename LayoutMap>
bool has_stride_one(unsigned int coord) noexcept {
  int max_value = boost::mpl::deref<
      typename boost::mpl::max_element<typename LayoutMap::layout_vector_t>::type>::type::value;
  return (LayoutMap::layout_vector[coord] == max_value);
}

template <typename LayoutMap, unsigned int Alignment, typename Halo>
int left_padding_helper(unsigned int coord) noexcept {
  unsigned int lpad = Halo::get_halo_vector()[coord];
  return (Alignment && has_stride_one<LayoutMap>(coord)) ? (Alignment - lpad) % Alignment : 0;
}

template <typename LayoutMap, unsigned int Alignment, typename Padding>
int right_padding_helper(unsigned int coord, unsigned int unaligned_dimension) noexcept {
  unsigned int rpad = Padding::get_halo_vector()[coord];
  return (Alignment && has_stride_one<LayoutMap>(coord))
             ? (Alignment - (unaligned_dimension + rpad) % Alignment)
             : 0;
}

template <typename Storage>
std::vector<std::pair<int, int>> get_padding(const Storage& storage) noexcept {
  const int n_dimensions = storage.meta_data().dims().n_dimensions;
  auto unaligned_dims = storage.meta_data().m_unaligned_dims;

  std::vector<std::pair<int, int>> padding_vec;

  for(int i = 0; i < n_dimensions; ++i) {
    int lpad = left_padding_helper<typename Storage::storage_info_type::layout,
                                   Storage::storage_info_type::s_alignment,
                                   typename Storage::storage_info_type::halo_t>(i);
    int rpad =
        right_padding_helper<typename Storage::storage_info_type::layout,
                             Storage::storage_info_type::s_alignment,
                             typename Storage::storage_info_type::padding_t>(i, unaligned_dims[i]);
    padding_vec.emplace_back(std::pair<int, int>(lpad, rpad));
  }

  return padding_vec;
}

//===------------------------------------------------------------------------------------------===//
//     Data
//===------------------------------------------------------------------------------------------===//

template <typename Storage>
void* get_data_pointer(const Storage& storage, unsigned int field_idx) noexcept {
  return static_cast<void*>(storage.fields()[field_idx].get());
}

} // namespace internal

} // namespace gridtools

} // namespace serialbox

#endif
