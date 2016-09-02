//===-- serialbox/Serializer/StorageView.h ------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the StorageView which represent a mutable view to a multi-dimensional
/// storage.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_SERIALIZER_STORAGEVIEW_H
#define SERIALBOX_SERIALIZER_STORAGEVIEW_H

#include "serialbox/Support/Logging.h"
#include "serialbox/Support/Type.h"
#include <algorithm>
#include <vector>

namespace serialbox {

/// \brief Represent a mutable view to a multi-dimensional storage
class StorageView {
public:
  /// \name Constructors
  /// @{

  /// \brief Construct StorageView from pointer
  template <class T>
  StorageView(T* data, const std::vector<int>& dims, const std::vector<int>& strides)
      : data_(static_cast<void*>(data)), type_(toTypeID<T>::value), dims_(dims), strides_(strides) {
    CHECK_NE(data_, nullptr);
    CHECK(!dims_.empty() && !strides_.empty()) << "empty dimension or strides";
    CHECK(dims_.size() == strides_.size()) << "dimension mismatch";
  }

  /// \brief Copy constructor
  StorageView(const StorageView& other) = default;

  /// \brief Move constructor
  StorageView(const StorageView&& other) = delete;

  /// @}
  /// \name Iterator
  /// @{

  /// @}
  /// \name Getter
  /// @{

  /// \brief Get data pointer as type T
  template <class T>
  T* data() noexcept {
    CHECK(toTypeID<T>::value == type_);
    return static_cast<T>(data_);
  }
  template <class T>
  const T* data() const noexcept {
    CHECK(toTypeID<T>::value == type_);
    return static_cast<T>(data_);
  }

  /// \brief Get raw data pointer
  void* data() noexcept { return data_; }
  const void* data() const noexcept { return data_; }

  /// \brief Get type-id
  TypeID type() const noexcept { return type_; }

  /// \brief Get dimensions
  const std::vector<int>& dims() const noexcept { return dims_; }

  /// \brief Get strides
  const std::vector<int>& strides() const noexcept { return strides_; }

  /// @}
  /// \name Operator
  /// @{

  /// \brief Copy assignment
  StorageView& operator=(StorageView other) noexcept {
    swap(other);
    return *this;
  }

  /// \brief Swap with \c other
  void swap(StorageView& other) noexcept {
    std::swap(data_, other.data_);
    std::swap(type_, other.type_);
    dims_.swap(other.dims_);
    strides_.swap(other.strides_);
  }

  /// @}

private:
  void* data_;               ///< Data pointer
  TypeID type_;              ///< Type-id
  std::vector<int> dims_;    ///< Dimensions
  std::vector<int> strides_; ///< Strides
};

/// \fn swap
/// \brief Swap StorageView \c a with \c b
void swap(StorageView& a, StorageView& b) noexcept { a.swap(b); }

/// \fn operator==
/// \brief Compare \c a equal \c b
bool operator==(const StorageView& a, const StorageView& b) noexcept {
  return a.data() == b.data() && a.dims() == b.dims() && a.strides() == b.strides();
}

} // namespace serialbox

#endif
