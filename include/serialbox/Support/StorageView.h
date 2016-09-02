//===-- serialbox/Serializer/StorageView.h ------------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_SERIALIZER_STORAGEVIEW_H
#define SERIALBOX_SERIALIZER_STORAGEVIEW_H

#include "serialbox/Support/Logging.h"
#include "serialbox/Support/Type.h"
#include <algorithm>
#include <iterator>
#include <vector>

namespace serialbox {

class StorageView;

/// \brief Forward Iterator access to the data of a StorageView
class StorageViewIterator {
public:
  /// \name Typedef
  /// @{

  using value_type = char;
  using diffrence_type = std::ptrdiff_t;
  using pointer = char*;
  using refrence = char&;
  using iterator = StorageViewIterator;
  using iterator_category = std::forward_iterator_tag;

  /// @}
  /// \name Constructor
  /// @{
  
  /// \brief Copy constructor
  StorageViewIterator(const StorageViewIterator&) = default;

  /// \brief Construct iterator at specific location in the data
  StorageViewIterator(char* curData, std::vector<int>&& dimIndices, int bytesPerElement,
                      StorageView* storageView);

  /// @}
  /// \name Operators
  /// @{
   
  /// \brief Copy assignment
  StorageViewIterator& operator=(const StorageViewIterator&) = default;
  
  /// \brief Move assignment
  StorageViewIterator& operator=(StorageViewIterator&&) = default;
  
  /// \brief Test for equality
  bool operator==(const iterator& right) const noexcept { return (curData_ == right.curData_); }

  /// \brief Test for inequality
  bool operator!=(const iterator& right) const noexcept { return (!(*this == right)); }
  
  /// \brief Pre-increment
  iterator& operator++() noexcept;

  /// \brief Post-increment
  iterator operator++(int) noexcept;
  
  /// \brief Swap with other
  void swap(StorageViewIterator& other) noexcept;
  
  /// @}

  /// \brief Return true if the storage is contiguous in memory
  bool isContiguous() noexcept { return isContiguous_; }
  
  /// \brief Get bytes per element
  int bytesPerElement() noexcept { return bytesPerElement_; }

  /// \brief Get current data location
  char* curData() noexcept { return curData_; }
  
private:
  char* curData_;               ///< Current position
  std::vector<int> dimIndices_; ///< Current dimensional index
  int bytesPerElement_;
  StorageView* storageView_;
  bool isContiguous_;
};

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
  StorageViewIterator begin() noexcept;

  /// \brief Iterator to the end of the data
  StorageViewIterator end() noexcept;

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
  char* data() noexcept { return data_; }
  const char* data() const noexcept { return data_; }

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

  /// @}

private:
  char* data_;
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
