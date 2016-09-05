//===-- Support/StorageView.cpp -----------------------------------------------------*- C++ -*-===//
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

#include "serialbox/Support/Logging.h"
#include "serialbox/Support/StorageView.h"
#include "serialbox/Support/StorageViewIterator.h"
#include <algorithm>

namespace serialbox {

StorageView::StorageView(void* data, TypeID type, const std::vector<int>& dims,
                         const std::vector<int>& strides,
                         const std::vector<std::pair<int, int>>& padding)
    : data_(reinterpret_cast<Byte*>(data)), type_(type), dims_(dims), strides_(strides),
      padding_(padding) {
  CHECK(!dims_.empty()) << "empty dimension";
  CHECK(dims_.size() == strides_.size() && dims_.size() == padding_.size()) << "dimension mismatch";
}

void StorageView::swap(StorageView& other) noexcept {
  std::swap(data_, other.data_);
  std::swap(type_, other.type_);
  dims_.swap(other.dims_);
  strides_.swap(other.strides_);
  padding_.swap(other.padding_);
}

bool StorageView::operator==(const StorageView& right) const noexcept {
  return (data_ == right.data_ && type_ == right.type_ && dims_ == right.dims_ &&
          strides_ == right.strides_ && padding_ == right.padding_);
}

std::ostream& operator<<(std::ostream& stream, const StorageView& s) {
  stream << "StorageView [\n";
  stream << "  data = " << static_cast<void*>(s.data_) << "\n";
  stream << "  type = " << TypeUtil::toString(s.type_) << "\n";
  
  stream << "  dims = {";
  for(auto i : s.dims_)
    stream << " " << i;
  
  stream << " }\n  strides = {";
  for(auto i : s.strides_)
    stream << " " << i;
  
  stream << " }\n  padding = {";
  for(auto i : s.padding_)
    stream << " [" << i.first << "," << i.second << "]";
  
  stream << " }\n]\n";
  return stream;
}

void swap(StorageView& a, StorageView& b) noexcept { a.swap(b); }

} // namespace serialbox
