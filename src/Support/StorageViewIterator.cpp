//===-- Support/StorageViewIterator.cpp ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file provides an Iterator interface for the StorageView.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Support/Compiler.h"
#include "serialbox/Support/StorageView.h"
#include "serialbox/Support/StorageViewIterator.h"

namespace serialbox {

StorageViewIterator::StorageViewIterator(StorageView* storageView, bool beginning)
    : bytesPerElement_(storageView->bytesPerElement()), storageView_(storageView) {

  if(!(end_ = !beginning)) {
    index_.resize(storageView_->dims().size(), 0);
    ptr_ = storageView_->data() + computeCurrentIndex();
  }
}

StorageViewIterator::iterator& StorageViewIterator::operator++() noexcept {
  if(!end_) {
    // Consecutively increment the dimensions (column-major order)
    int size = index_.size();
    for(int i = 0; i < size; ++i)
      if(SERIALBOX_BUILTIN_LIKELY(++index_[i] < storageView_->dims()[i]))
        break;
      else {
        index_[i] = 0;
        // If we overflow in the last dimension we reached the end
        if(SERIALBOX_BUILTIN_UNLIKELY(i == size - 1))
          end_ = true;
      }

    // Compute the current data pointer
    ptr_ = storageView_->data() + computeCurrentIndex();
  }
  return (*this);
}

bool StorageViewIterator::operator==(const iterator& right) const noexcept { 
  return (ptr_ == right.ptr_ || (end_ == true && end_ == right.end_)); 
}

StorageViewIterator::iterator StorageViewIterator::operator++(int)noexcept {
  LOG(INFO) << "usage of this operator is discouraged, user pre-increment instead";
  iterator tmp = *this;
  ++*this;
  return (tmp);
}

void StorageViewIterator::swap(StorageViewIterator& other) noexcept {
  std::swap(ptr_, other.ptr_);
  index_.swap(other.index_);
  std::swap(bytesPerElement_, other.bytesPerElement_);
  std::swap(storageView_, other.storageView_);
}

std::ostream& operator<<(std::ostream& stream, const StorageViewIterator& it) {
  stream << "StorageViewIterator [\n";
  stream << "  ptr = " << static_cast<void*>(it.ptr_) << "\n";
  stream << "  end = " << it.end_ << "\n";  
  stream << "  index = {";
  for(auto i : it.index_)
    stream << " " << i;
  stream << " }\n  bytesPerElement = " << it.bytesPerElement_ << "\n";
  stream << "  storageView = " << std::hex << it.storageView_ << "\n";
  stream << "]\n";
  return stream;
}

//                    stride
//   <-------------------------------------------------->
//   <--------><------------------------------><-------->
//   pad.first              dim                pad.second
// 


int StorageViewIterator::computeCurrentIndex() const noexcept {
  int pos = 0;
  const int size = index_.size();
  for(int i = 0; i < size; ++i)
    pos += bytesPerElement_ *
           (storageView_->strides()[i] * (storageView_->padding()[i].first + index_[i]));
  return pos;
}

} // namespace serialbox
