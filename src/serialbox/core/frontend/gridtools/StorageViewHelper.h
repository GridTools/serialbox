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
template <typename MetaDataType, typename StorageType>
std::vector<int> get_strides(const StorageType& storage, const MetaDataType& meta_data) {
  // TODO FIXME Should be unsigned int in Serialbox
  std::vector<int> v;
  for(const auto elem : storage.strides()) {
    v.push_back(elem);
  }
  return v;
}

template <typename MetaDataType>
std::vector<int> get_dims(const MetaDataType& meta_data) noexcept {
  // TODO FIXME Should be unsigned int in Serialbox
  std::vector<int> v;
  for(const auto elem : ::gridtools::to_vector(meta_data.m_dims)) {
    v.push_back(elem);
  }
  return v;
}

template <typename StorageType, class MetaDataType>
void* get_origin_ptr(const StorageType& storage, const MetaDataType& meta_data,
                     unsigned int field_idx) noexcept {
  auto* data_ptr = storage.get_storage_ptr()->get_cpu_ptr();
  auto index = meta_data.index({}); // http://en.cppreference.com/w/cpp/language/zero_initialization
  return static_cast<void*>(data_ptr + index);
}

} // namespace internal

} // namespace gridtools

} // namespace serialbox

#endif
