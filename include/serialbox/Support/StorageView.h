//===-- serialbox/Support/StorageView.h ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information.
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the StorageView which represent a mutable view to a multi-dimensional
/// storage.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_SUPPORT_STORAGEVIEW_H
#define SERIALBOX_SUPPORT_STORAGEVIEW_H

#include "serialbox/Support/Logging.h"
#include "serialbox/Support/StorageViewIterator.h"
#include "serialbox/Support/Type.h"
#include <vector>

namespace serialbox {

/// \brief Represent a mutable view to a multi-dimensional storage
class StorageView {
public:
  /// \name Constructors
  /// @{

  /// \brief Construct StorageView from pointer
  StorageView(void* data, TypeID type, const std::vector<int>& dims,
              const std::vector<int>& strides, const std::vector<std::pair<int, int>>& padding);

  /// \brief Copy constructor
  StorageView(const StorageView& other) = default;

  /// \brief Move constructor
  StorageView(const StorageView&& other) = delete;

  /// @}
  /// \name Iterator
  /// @{

  /// \brief Iterator to the beginning of the data
  StorageViewIterator begin() noexcept { return StorageViewIterator(this, true); }

  /// \brief Iterator to the end of the data
  StorageViewIterator end() noexcept { return StorageViewIterator(this, false); }

  /// @}
  /// \name Getter
  /// @{

  /// \brief Get data pointer as type T
  template <class T>
  T* data() noexcept {
    CHECK(ToTypeID<T>::value == type_);
    return static_cast<T>(data_);
  }
  template <class T>
  const T* data() const noexcept {
    CHECK(ToTypeID<T>::value == type_);
    return static_cast<T>(data_);
  }

  /// \brief Get raw data pointer
  Byte* data() noexcept { return data_; }
  const Byte* data() const noexcept { return data_; }

  /// \brief Get bytes per element
  int bytesPerElement() const noexcept { return TypeUtil::sizeOf(type_); }

  /// \brief Get dimensions
  const std::vector<int>& dims() const noexcept { return dims_; }

  /// \brief Get strides
  const std::vector<int>& strides() const noexcept { return strides_; }

  /// \brief Get strides
  const std::vector<std::pair<int, int>>& padding() const noexcept { return padding_; }

  /// @}
  /// \name Operators
  /// @{

  /// \brief Copy assignment
  StorageView& operator=(StorageView other) noexcept {
    swap(other);
    return *this;
  }

  /// \brief Swap with \c other
  void swap(StorageView& other) noexcept;

  /// \brief Test for equality
  bool operator==(const StorageView& right) const noexcept;
  
  /// \brief Test for inequality
  bool operator!=(const StorageView& right) const noexcept { return (!(*this == right)); }
  
  /// \brief Convert to stream  
  friend std::ostream& operator<<(std::ostream& stream, const StorageView& s);

  /// @}

private:
  Byte* data_;
  TypeID type_;
  std::vector<int> dims_;
  std::vector<int> strides_;
  std::vector<std::pair<int, int>> padding_;
};

/// \fn swap
/// \brief Swap StorageView \c a with \c b
void swap(StorageView& a, StorageView& b) noexcept;

} // namespace serialbox

#endif
