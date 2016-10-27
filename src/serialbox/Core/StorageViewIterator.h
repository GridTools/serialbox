//===-- serialbox/Core/StorageViewIterator.h ----------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_STORAGEVIEWITERATOR_H
#define SERIALBOX_CORE_STORAGEVIEWITERATOR_H

#include "serialbox/Core/Compiler.h"
#include "serialbox/Core/Slice.h"
#include "serialbox/Core/Type.h"
#include <iterator>
#include <type_traits>
#include <vector>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Forward iterator to access the data of a StorageView
///
/// The data is accessed in column-major order using operator++ (pre-increment).
template <class ValueType>
class StorageViewIteratorBase {
public:
  /// \name Typedefs
  /// @{
  ///
  using value_type = ValueType;
  using diffrence_type = std::ptrdiff_t;
  using pointer = value_type*;
  using refrence = value_type&;
  using iterator = StorageViewIteratorBase<ValueType>;
  using iterator_category = std::forward_iterator_tag;

  /// @}
  /// \name Constructors
  /// @{

  /// \brief Copy constructor
  StorageViewIteratorBase(const iterator&) = default;

  /// \brief Move constructor
  StorageViewIteratorBase(iterator&&) = default;

  /// \brief Construct iterator at specific location in the data
  ///
  /// \param originPtr        Origin pointer of the associated StorageView
  /// \param bytesPerElement  Number of bytes of the underlying data-type
  /// \param dims             Vector of dimensions
  /// \param strides          Vector of strides
  /// \param slice            Slice of the data
  /// \param beginning        Indicate whether the iterator has reached the end
  StorageViewIteratorBase(value_type* originPtr, int bytesPerElement, const std::vector<int>& dims,
                          const std::vector<int>& strides, const Slice& slice, bool beginning)
      : dims_(dims), strides_(strides), bytesPerElement_(bytesPerElement), slice_(slice) {

    if(!(end_ = !beginning)) {

      if(slice_.empty())
        index_.resize(dims_.size(), 0);
      else
        for(const auto& triple : slice.sliceTriples())
          index_.push_back(triple.start);

      originPtr_ = originPtr;
      curPtr_ = originPtr_ + computeCurrentIndex();
    }
  }

  /// @}
  /// \name Operators
  /// @{

  /// \brief Copy assignment
  iterator& operator=(const iterator&) = default;

  /// \brief Move assignment
  iterator& operator=(iterator&&) = default;

  /// \brief Test for equality
  bool operator==(const iterator& right) const noexcept {
    return (curPtr_ == right.curPtr_ || (end_ == true && end_ == right.end_));
  }

  /// \brief Test for inequality
  bool operator!=(const iterator& right) const noexcept { return (!(*this == right)); }

  /// \brief Pre-increment
  iterator& operator++() noexcept {
    if(!end_) {

      //
      // Unsliced increment
      //
      if(slice_.empty()) {

        // Consecutively increment the dimensions (column-major order)
        int size = index_.size();
        for(int i = 0; i < size; ++i)
          if(SERIALBOX_BUILTIN_LIKELY(++index_[i] < dims_[i]))
            break;
          else {
            index_[i] = 0;
            // If we overflow in the last dimension we reached the end
            if(SERIALBOX_BUILTIN_UNLIKELY(i == size - 1))
              end_ = true;
          }

        //
        // Sliced increment
        //
      } else {

        // Consecutively increment the dimensions (column-major order) with associated step
        int size = index_.size();
        const auto& triples = slice_.sliceTriples();

        for(int i = 0; i < size; ++i)
          if((index_[i] += triples[i].step) < triples[i].stop)
            break;
          else {
            index_[i] = triples[i].start;
            // If we overflow in the last dimension we reached the end
            if(SERIALBOX_BUILTIN_UNLIKELY(i == size - 1))
              end_ = true;
          }
      }

      // Compute the current data pointer
      curPtr_ = originPtr_ + computeCurrentIndex();
    }
    return (*this);
  }

  /// \brief Derefrence
  refrence operator*() noexcept { return *curPtr_; }

  /// \brief Swap with other
  void swap(iterator& other) noexcept {
    std::swap(curPtr_, other.curPtr_);
    index_.swap(other.index_);

    std::swap(originPtr_, other.originPtr_);
    dims_.swap(other.dims_);
    strides_.swap(other.strides_);
    slice_.swap(other.slice_);
    std::swap(bytesPerElement_, other.bytesPerElement_);
  }

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const iterator& it) {
    stream << "StorageViewIterator = {\n";
    stream << "  curPtr: " << static_cast<void*>(it.curPtr_) << "\n";
    stream << "  index: [";
    for(auto i : it.index_)
      stream << " " << i;
    stream << " ]\n  end: " << std::boolalpha << it.end_ << "\n";
    stream << "  originPtr: " << static_cast<void*>(it.originPtr_) << "\n";
    stream << "  dims: [";
    for(auto i : it.dims_)
      stream << " " << i;
    stream << " ]\n  strides: [";
    for(auto i : it.strides_)
      stream << " " << i;
    stream << " ]\n  bytesPerElement: " << it.bytesPerElement_ << "\n";
    stream << "}\n";
    return stream;
  }

  /// @}

  /// \brief Get bytes per element
  int bytesPerElement() noexcept { return bytesPerElement_; }

  /// \brief Get current data pointer
  value_type* ptr() noexcept { return curPtr_; }

  /// \brief Interpret current data pointer as type ´T´
  template <class T>
  typename match_cv_qualifier<value_type, T>::type& as() noexcept {
    return *((T*)(curPtr_));
  }
  template <class T>
  typename match_cv_qualifier<value_type, T>::type& as() const noexcept {
    return *((T*)(curPtr_));
  }

  /// \brief Get current index position in the data
  const std::vector<int>& index() const noexcept { return index_; }

protected:
  /// \brief Compute the current linear index in the data according to the index vector
  int computeCurrentIndex() const noexcept {
    int pos = 0;
    const int size = index_.size();
    for(int i = 0; i < size; ++i)
      pos += bytesPerElement_ * (strides_[i] * index_[i]);
    return pos;
  }

protected:
  // Position in the data
  value_type* curPtr_;
  std::vector<int> index_;
  bool end_;

  // Associated StorageView
  value_type* originPtr_;
  std::vector<int> dims_;
  std::vector<int> strides_;
  int bytesPerElement_;
  Slice slice_;
};

/// \brief Mutable forward iterator to access the data of a StorageView
///
/// The data is accessed in column-major order using operator++ (pre-increment).
class StorageViewIterator : public StorageViewIteratorBase<Byte> {
public:
  using Base = StorageViewIteratorBase<Byte>;

  /// \brief Copy constructor
  StorageViewIterator(const StorageViewIterator&) = default;

  /// \brief Move constructor
  StorageViewIterator(StorageViewIterator&&) = default;

  /// \brief Construct iterator at specific location in the data
  ///
  /// \param originPtr        Origin pointer of the associated StorageView
  /// \param bytesPerElement  Number of bytes of the underlying data-type
  /// \param dims             Vector of dimensions
  /// \param strides          Vector of strides
  /// \param slice            Slice of the data
  /// \param beginning        Indicate whether the iterator has reached the end
  StorageViewIterator(Base::value_type* originPtr, int bytesPerElement,
                      const std::vector<int>& dims, const std::vector<int>& strides,
                      const Slice& slice, bool beginning)
      : Base(originPtr, bytesPerElement, dims, strides, slice, beginning) {}
};

/// \brief Non-mutable forward iterator to access the data of a StorageView
///
/// The data is accessed in column-major order using operator++ (pre-increment).
class ConstStorageViewIterator : public StorageViewIteratorBase<const Byte> {
public:
  using Base = StorageViewIteratorBase<const Byte>;

  /// \brief Copy constructor
  ConstStorageViewIterator(const ConstStorageViewIterator&) = default;

  /// \brief Move constructor
  ConstStorageViewIterator(ConstStorageViewIterator&&) = default;

  /// \brief Construct iterator at specific location in the data
  ///
  /// \param originPtr        Origin pointer of the associated StorageView
  /// \param bytesPerElement  Number of bytes of the underlying data-type
  /// \param dims             Vector of dimensions
  /// \param strides          Vector of strides
  /// \param slice            Slice of the data
  /// \param beginning        Indicate whether the iterator has reached the end
  ConstStorageViewIterator(Base::value_type* originPtr, int bytesPerElement,
                           const std::vector<int>& dims, const std::vector<int>& strides,
                           const Slice& slice, bool beginning)
      : Base(originPtr, bytesPerElement, dims, strides, slice, beginning) {}
};

/// @}

} // namespace serialbox

#endif
