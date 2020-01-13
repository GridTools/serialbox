//===-- serialbox/core/StorageView.cpp ----------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/StorageView.h"
#include "serialbox/core/Exception.h"
#include "serialbox/core/Logging.h"
#include "serialbox/core/StorageViewIterator.h"
#include <algorithm>
#include <cassert>

namespace serialbox {

StorageView::StorageView(void* originPtr, TypeID type, const std::vector<int>& dims,
                         const std::vector<int>& strides)
    : originPtr_(reinterpret_cast<Byte*>(originPtr)), type_(type), dims_(dims), strides_(strides),
      slice_((Slice::Empty())) {
  assert(!dims_.empty() && "empty dimension");
  assert(dims_.size() == strides_.size() && "dimension mismatch");
  assert(slice_.empty());
}

StorageView::StorageView(void* originPtr, TypeID type, std::vector<int>&& dims,
                         std::vector<int>&& strides)
    : originPtr_(reinterpret_cast<Byte*>(originPtr)), type_(type), dims_(dims), strides_(strides),
      slice_((Slice::Empty())) {
  assert(!dims_.empty() && "empty dimension");
  assert(dims_.size() == strides_.size() && "dimension mismatch");
  assert(slice_.empty());
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

void StorageView::setSlice(Slice slice) {
  if(slice.sliceTriples().size() > dims_.size())
    throw Exception("number of slices (%i) exceeds number of dimensions (%i)",
                    slice.sliceTriples().size(), dims_.size());

  // Append un-sliced dimensions
  while(slice.sliceTriples().size() != dims_.size())
    slice();

  // Expand negative Slice.stop
  for(std::size_t i = 0; i < slice.sliceTriples().size(); ++i) {

    slice.sliceTriples()[i].stop =
        (slice.sliceTriples()[i].stop < 0 ? dims_[i] + 1 + slice.sliceTriples()[i].stop
                                          : slice.sliceTriples()[i].stop);
  }
  assert(slice.sliceTriples().size() == dims_.size());
  slice_ = std::move(slice);
}

bool StorageView::isMemCopyable() const noexcept {
  // Check if data is sliced
  if(!slice_.empty())
    return false;

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

void swap(StorageView& a, StorageView& b) noexcept { a.swap(b); }

} // namespace serialbox
