//===-- serialbox/core/FieldMetainfoImplImpl.h ------------------------------------------*- C++
//-*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the meta information of a field.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/FieldMetainfoImpl.h"
#include <algorithm>
#include <memory>

namespace serialbox {

FieldMetainfoImpl& FieldMetainfoImpl::operator=(const FieldMetainfoImpl& other) {
  type_ = other.type_;
  dims_ = other.dims_;
  metaInfo_ = std::make_shared<MetainfoMapImpl>(*other.metaInfo_);
  return (*this);
}

void FieldMetainfoImpl::swap(FieldMetainfoImpl& other) noexcept {
  std::swap(type_, other.type_);
  dims_.swap(other.dims_);
  metaInfo_->swap(*other.metaInfo_);
}

bool FieldMetainfoImpl::operator==(const FieldMetainfoImpl& right) const noexcept {
  if(type_ != right.type_)
    return false;

  if(dims_.size() != right.dims_.size() ||
     !std::equal(dims_.begin(), dims_.end(), right.dims_.begin()))
    return false;

  return (*metaInfo_ == *right.metaInfo_);
}

std::string FieldMetainfoImpl::toString() const {
  std::stringstream ss;
  ss << "type = " << TypeUtil::toString(type_) << ", dims = [" << ArrayUtil::toString(dims_)
     << "], "
     << "metainfo = " << *metaInfo_;
  return ss.str();
}

std::ostream& operator<<(std::ostream& stream, const FieldMetainfoImpl& f) {
  return (stream << f.toString());
}

} // namespace serialbox
