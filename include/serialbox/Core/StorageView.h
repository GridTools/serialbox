//===-- serialbox/Core/StorageView.h ------------------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_STORAGEVIEW_H
#define SERIALBOX_CORE_STORAGEVIEW_H

#include "serialbox/Core/Logging.h"
#include "serialbox/Core/StorageViewIterator.h"
#include "serialbox/Core/Type.h"
#include <vector>
#include <utility>

namespace serialbox {

/// \brief Represent a mutable view to a multi-dimensional storage
class StorageView {
public:
  /// \name Constructors
  /// @{

  /// \brief Construct StorageView
  StorageView(void* data, TypeID type, const std::vector<int>& dims,
              const std::vector<int>& strides, const std::vector<std::pair<int, int>>& padding);

  /// \brief Construct StorageView (move version)
  StorageView(void* data, TypeID type, std::vector<int>&& dims, std::vector<int>&& strides,
              std::vector<std::pair<int, int>>&& padding);

  /// \brief Copy constructor
  StorageView(const StorageView& other) = default;

  /// \brief Move constructor
  StorageView(StorageView&& other) = default;

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
  StorageView& operator=(StorageView other) noexcept;
  
  /// \brief Move assignment
  StorageView& operator=(StorageView&& other) noexcept;

  /// \brief Swap with \c other
  void swap(StorageView& other) noexcept;

  /// \brief Test for equality
  bool operator==(const StorageView& right) const noexcept;

  /// \brief Test for inequality
  bool operator!=(const StorageView& right) const noexcept { return (!(*this == right)); }

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const StorageView& s);

  /// @}

  /// \brief Return true if the storage is contiguous in memory (i.e no padding) and is column-major
  /// ordered
  bool isMemCopyable() const noexcept;

private:
  Byte* data_;
  TypeID type_;

  std::vector<int> dims_;                    ///< Unaligned dimensions (including the halos)
  std::vector<int> strides_;                 ///< Total strides including all padding
  std::vector<std::pair<int, int>> padding_; ///< Left and right padding in each dimension
};

/// \fn swap
/// \brief Swap StorageView \c a with \c b
void swap(StorageView& a, StorageView& b) noexcept;

} // namespace serialbox

#endif
