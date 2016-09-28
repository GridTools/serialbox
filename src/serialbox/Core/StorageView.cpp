//===-- serialbox/Core/StorageView.cpp ----------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/StorageView.h"
#include "serialbox/Core/Logging.h"
#include "serialbox/Core/StorageViewIterator.h"
#include <algorithm>

namespace serialbox {

StorageView::StorageView(void* originPtr, TypeID type, const std::vector<int>& dims,
                         const std::vector<int>& strides)
    : originPtr_(reinterpret_cast<Byte*>(originPtr)), type_(type), dims_(dims), strides_(strides) {
  CHECK(!dims_.empty()) << "empty dimension";
  CHECK(dims_.size() == strides_.size()) << "dimension mismatch";
}

StorageView::StorageView(void* originPtr, TypeID type, std::vector<int>&& dims,
                         std::vector<int>&& strides)
    : originPtr_(reinterpret_cast<Byte*>(originPtr)), type_(type), dims_(dims), strides_(strides) {
  CHECK(!dims_.empty()) << "empty dimension";
  CHECK(dims_.size() == strides_.size()) << "dimension mismatch";
}

void StorageView::swap(StorageView& other) noexcept {
  std::swap(originPtr_, other.originPtr_);
  std::swap(type_, other.type_);
  dims_.swap(other.dims_);
  strides_.swap(other.strides_);
}

bool StorageView::operator==(const StorageView& right) const noexcept {
  return (originPtr_ == right.originPtr_ && type_ == right.type_ && dims_ == right.dims_);
}

std::ostream& operator<<(std::ostream& stream, const StorageView& s) {
  stream << "StorageView = {\n";
  stream << "  originPtr: " << static_cast<void*>(s.originPtr_) << "\n";
  stream << "  type: " << s.type_ << "\n";

  stream << "  dims: [";
  for(auto i : s.dims_)
    stream << " " << i;

  stream << " ]\n  strides: [";
  for(auto i : s.strides_)
    stream << " " << i;

  stream << " ]\n}\n";
  return stream;
}

void swap(StorageView& a, StorageView& b) noexcept { a.swap(b); }

bool StorageView::isMemCopyable() const noexcept {
  // Check if data is col-major
  int stride = 1;
  if(strides_[0] != 1)
    return false;

  // Check that there is no padding
  for(std::size_t i = 1; i < dims_.size(); ++i) {
    stride *= dims_[i - 1];
    if(strides_[i] != stride)
      return false;
  }
  return true;
}

std::size_t StorageView::size() const noexcept {
  std::size_t size = 1;
  for(std::size_t i = 0; i < dims_.size(); ++i)
    size *= (dims_[i] == 0 ? 1 : dims_[i]);
  return size;
}

std::size_t StorageView::sizeInBytes() const noexcept { return size() * bytesPerElement(); }

} // namespace serialbox
