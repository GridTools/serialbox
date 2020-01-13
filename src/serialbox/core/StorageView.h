//===-- serialbox/core/StorageView.h ------------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/Logging.h"
#include "serialbox/core/STLExtras.h"
#include "serialbox/core/Slice.h"
#include "serialbox/core/StorageViewIterator.h"
#include "serialbox/core/Type.h"
#include <utility>
#include <vector>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Represent a mutable view to a multi-dimensional storage
class StorageView {
public:
  /// \name Constructors
  /// @{

  /// \brief Construct StorageView
  StorageView(void* originPtr, TypeID type, const std::vector<int>& dims,
              const std::vector<int>& strides);

  /// \brief Construct StorageView (move version)
  StorageView(void* originPtr, TypeID type, std::vector<int>&& dims, std::vector<int>&& strides);

  /// \brief Copy constructor
  StorageView(const StorageView& other) = default;

  /// \brief Move constructor
  StorageView(StorageView&& other) = default;

  /// @}
  /// \name Iterator
  /// @{

  /// \brief Iterator to the beginning of the data
  StorageViewIterator begin() noexcept {
    return StorageViewIterator(originPtr_, bytesPerElement(), dims_, strides_, slice_, true);
  }
  ConstStorageViewIterator begin() const noexcept {
    return ConstStorageViewIterator(originPtr_, bytesPerElement(), dims_, strides_, slice_, true);
  }

  /// \brief Iterator to the end of the data
  StorageViewIterator end() noexcept {
    return StorageViewIterator(originPtr_, bytesPerElement(), dims_, strides_, slice_, false);
  }
  ConstStorageViewIterator end() const noexcept {
    return ConstStorageViewIterator(originPtr_, bytesPerElement(), dims_, strides_, slice_, false);
  }

  /// @}
  /// \name Getter
  /// @{

  /// \brief Get data pointer as type `T` of the origin of the data
  template <class T>
  T* originPtrAs() noexcept {
    return (T*)(originPtr_);
  }
  template <class T>
  const T* originPtrAs() const noexcept {
    return (T*)(originPtr_);
  }

  /// \brief Get raw data pointer
  Byte* originPtr() noexcept { return originPtr_; }
  const Byte* originPtr() const noexcept { return originPtr_; }

  /// \brief Get type
  TypeID type() const noexcept { return type_; }

  /// \brief Get bytes per element
  int bytesPerElement() const noexcept { return TypeUtil::sizeOf(type_); }

  /// \brief Get dimensions
  const std::vector<int>& dims() const noexcept { return dims_; }

  /// \brief Get strides
  const std::vector<int>& strides() const noexcept { return strides_; }

  /// @}
  /// \name Operators
  /// @{

  /// \brief Copy assignment
  StorageView& operator=(const StorageView& other) = default;

  /// \brief Move assignment
  StorageView& operator=(StorageView&& other) = default;

  /// \brief Swap with other
  void swap(StorageView& other) noexcept;

  /// \brief Test for equality
  bool operator==(const StorageView& right) const noexcept;

  /// \brief Test for inequality
  bool operator!=(const StorageView& right) const noexcept { return (!(*this == right)); }

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const StorageView& s);

  /// @}

  /// \brief Set the slice of the `StorageView`
  void setSlice(Slice slice);

  /// \brief Get the slice of the `StorageView`
  Slice& getSlice() noexcept { return slice_; }
  const Slice& getSlice() const noexcept { return slice_; }

  /// \brief Return true if the storage is contiguous in memory (i.e no padding) and is column-major
  /// ordered
  bool isMemCopyable() const noexcept;

  /// \brief Size of the allocated, sliced data (without padding)
  std::size_t size() const noexcept;

  /// \brief Size of the allocated, sliced data (without padding) in Bytes
  std::size_t sizeInBytes() const noexcept;

private:
  Byte* originPtr_;          ///< Pointer to the origin of the data (i.e skipping initial padding)
  TypeID type_;              ///< TypeID of the storage
  std::vector<int> dims_;    ///< Unaligned dimensions (including the halos)
  std::vector<int> strides_; ///< Total strides including all padding
  Slice slice_;              ///< Slicing of the data
};

/// \fn swap
/// \brief Swap StorageView \c a with \c b
void swap(StorageView& a, StorageView& b) noexcept;

/// @}

} // namespace serialbox

#endif
