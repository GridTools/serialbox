//===-- serialbox/core/SavepointImpl.cpp --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the shared implementation the Savepoint.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/SavepointImpl.h"
#include "serialbox/core/Logging.h"
#include <sstream>

namespace serialbox {

SavepointImpl& SavepointImpl::operator=(const SavepointImpl& other) {
  name_ = other.name_;
  metaInfo_ = std::make_shared<MetainfoMapImpl>(*other.metaInfo_);
  return (*this);
}

std::string SavepointImpl::toString() const {
  std::stringstream ss;
  ss << *this;
  return ss.str();
}

std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s) {
  return (stream << s.name_ << " " << (*s.metaInfo_));
}

} // namespace serialbox
