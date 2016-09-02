//===-- Support/Type.cpp ------------------------------------------------------------*- C++ -*-===//
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

#include "serialbox/Support/StorageView.h"


//
//                    stride
//   <---------------------------------------->
//   <---><------------------------------><--->
//    pad              dim                 pad
//

static inline int computeIndex(const std::vector<int>& indices,
                               const std::vector<int>& strides,
                               const std::vector<std::pair<int, int>>& padding) noexcept {
  int pos = 0;
  const int size = indices.size();
  for(int i = 0; i < size; ++i) {
    pos += strides[i] * indices[i];
  }
  return pos;
}

namespace serialbox {

StorageViewIterator::StorageViewIterator(char* curData, std::vector<int>&& dimIndices,
                                         int bytesPerElement, StorageView* storageView)
    : curData_(curData), dimIndices_(dimIndices), bytesPerElement_(bytesPerElement),
      storageView_(storageView) {
  
  //TODO: do the check
  isContiguous_ = false;
}


StorageViewIterator::iterator& StorageViewIterator::operator++() noexcept {
  for(int i = 0; i < dimIndices_.size(); ++i)
    if(++dimIndices_[i] < storageView_->dims()[i])
      break;
    else
      dimIndices_[i] = 0;

  curData_ = storageView_->data() +
             computeIndex(dimIndices_, storageView_->strides(), storageView_->padding());

  return (*this);
}

StorageViewIterator::iterator StorageViewIterator::operator++(int) noexcept {
  iterator tmp = *this;
  ++*this;
  return (tmp);
}

void StorageViewIterator::swap(StorageViewIterator& other) noexcept {
  std::swap(curData_, other.curData_);
  dimIndices_.swap(other.dimIndices_);
  std::swap(bytesPerElement_, other.bytesPerElement_);
  std::swap(storageView_, other.storageView_);
}

StorageView::StorageView(void* data, TypeID type, const std::vector<int>& dims,
                         const std::vector<int>& strides,
                         const std::vector<std::pair<int, int>>& padding)
    : data_(reinterpret_cast<char*>(data)), type_(type), dims_(dims), strides_(strides),
      padding_(padding) {
  CHECK(data_ != nullptr) << "invalid data";
  CHECK(!dims_.empty()) << "empty dimension";
  CHECK(dims_.size() == strides_.size() && dims_.size() == padding_.size()) << "dimension mismatch";
}

StorageViewIterator StorageView::begin() noexcept {
  int bytesPerElement = TypeUtil::sizeOf(type_);
  char* curData = data_;
  std::vector<int> dimIndices(dims_.size(), 0);
  return StorageViewIterator(curData, std::move(dimIndices), bytesPerElement, this);
}

StorageViewIterator StorageView::end() noexcept {
  int bytesPerElement = TypeUtil::sizeOf(type_);
  char* curData = data_ + bytesPerElement * (strides_.back() * dims_.back());
  std::vector<int> dimIndices(dims_);
  return StorageViewIterator(curData, std::move(dimIndices), bytesPerElement, this);
}

void StorageView::swap(StorageView& other) noexcept {
  std::swap(data_, other.data_);
  std::swap(type_, other.type_);
  dims_.swap(other.dims_);
  strides_.swap(other.strides_);
}

void swap(StorageView& a, StorageView& b) noexcept { a.swap(b); }

} // namespace serialbox
