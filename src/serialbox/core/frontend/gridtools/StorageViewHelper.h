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
#include <storage/common/storage_info_rt.hpp>
#include <utility>
#include <vector>

namespace serialbox {

namespace gridtools {

namespace internal {

inline std::vector<int> unsigned_to_int(const std::vector<unsigned int>& in) {
  std::vector<int> out(in.size());
  std::copy(in.begin(), in.end(), out.begin());
  return out;
}

template <typename StorageType>
std::vector<int> get_strides(const StorageType& storage) {
  return unsigned_to_int(::gridtools::to_vector(storage.strides()));
}

/*
 * @brief gets the unaligned dims from gridtools (not including alignment)
 */
template <typename StorageType>
std::vector<int> get_dims(const StorageType& storage) noexcept {
  return unsigned_to_int(::gridtools::to_vector(
      ::gridtools::make_unaligned_dims_array(*storage.get_storage_info_ptr())));
}

template <typename StorageType>
void* get_origin_ptr(const StorageType& storage, unsigned int field_idx) noexcept {
  // TODO test data_fields
  auto* data_ptr = storage.get_storage_ptr()->get_cpu_ptr();
  auto index = storage.get_storage_info_ptr()->index(
      {}); // http://en.cppreference.com/w/cpp/language/zero_initialization
  return static_cast<void*>(data_ptr + index);
}

} // namespace internal

} // namespace gridtools

} // namespace serialbox

#endif
