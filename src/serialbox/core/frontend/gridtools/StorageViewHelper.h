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

#include <utility>
#include <vector>

namespace serialbox {

namespace gridtools {

namespace internal {

/*
 * @brief convert any Container of integer type which has operator[] and .size() to std::vector<int>
 */
template <typename Container>
std::vector<int> to_vector(Container const& a) {
  std::vector<int> v(a.size());
  for(int i = 0; i < a.size(); ++i) {
    v.at(i) = a[i];
  }
  return v;
}

template <typename StorageType>
std::vector<int> get_strides(const StorageType& storage) {
  return to_vector(storage.strides());
}

/*
 * @brief gets the total lengths from gridtools (including padding, excluding alignment)
 */
template <typename StorageType>
std::vector<int> get_dims(const StorageType& storage) noexcept {
  return to_vector(storage.dims());
}

template <typename StorageType>
void* get_origin_ptr(const StorageType& storage, unsigned int field_idx) noexcept {
  auto* data_ptr = storage.get_storage_ptr()->get_cpu_ptr();
  auto index = storage.get_storage_info_ptr()->index(
      {}); // http://en.cppreference.com/w/cpp/language/zero_initialization
  return static_cast<void*>(data_ptr + index);
}

} // namespace internal

} // namespace gridtools

} // namespace serialbox

#endif
