//===-- serialbox/Support/StorageViewIterator.h -------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information.
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file provides an Iterator interface for the StorageView.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_SUPPORT_STORAGEVIEWITERATOR_H
#define SERIALBOX_SUPPORT_STORAGEVIEWITERATOR_H

#include "serialbox/Support/Type.h"
#include <iterator>
#include <vector>

namespace serialbox {

class StorageView;

/// \brief Mutable forward iterator to access the data of a StorageView
/// 
/// The data is accessed in column-major order.
class StorageViewIterator {
public:
  /// \name Typedef
  /// @{
  /// 
  using value_type = Byte;
  using diffrence_type = std::ptrdiff_t;
  using pointer = Byte*;
  using refrence = Byte&;
  using iterator = StorageViewIterator;
  using iterator_category = std::forward_iterator_tag;
  
  /// @}
  /// \name Constructor
  /// @{

  /// \brief Copy constructor
  StorageViewIterator(const StorageViewIterator&) = default;

  /// \brief Construct iterator at specific location in the data
  /// 
  /// \param storageView  Associated StorageView
  /// \param beginning    Create a pointer to the beginning of the data or the end 
  StorageViewIterator(StorageView* storageView, bool beginning);

  /// @}
  /// \name Operators
  /// @{

  /// \brief Copy assignment
  StorageViewIterator& operator=(const StorageViewIterator&) = default;

  /// \brief Test for equality
  bool operator==(const iterator& right) const noexcept;

  /// \brief Test for inequality
  bool operator!=(const iterator& right) const noexcept { return (!(*this == right)); }

  /// \brief Pre-increment
  iterator& operator++() noexcept;

  /// \brief Post-increment
  iterator operator++(int)noexcept;
  
  /// \brief Derefrence
  refrence operator*() noexcept { return *ptr_; }
  
  /// \brief Swap with other
  void swap(StorageViewIterator& other) noexcept;
  
  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const StorageViewIterator& it); 

  /// @}

  /// \brief Get bytes per element
  int bytesPerElement() noexcept { return bytesPerElement_; }

  /// \brief Get current data pointer
  Byte* ptr() noexcept { return ptr_; } 

private:
  /// \brief Compute the current linear index in the data according to the \c index vector
  int computeCurrentIndex() const noexcept;
  
private:
  // Position in the data
  Byte* ptr_;
  bool end_;  
  std::vector<int> index_;

  // Associated StorageView
  int bytesPerElement_;
  StorageView* storageView_;
};

} // namespace serialbox

#endif 
