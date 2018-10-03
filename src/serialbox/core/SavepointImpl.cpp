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
#include "MetainfoMapImplSerializer.h" // TODO remove after refactoring
#include "serialbox/core/Logging.h"
#include <sstream>

namespace serialbox {

SavepointImpl& SavepointImpl::operator=(const SavepointImpl& other) {
  name_ = other.name_;
  metaInfo_ = std::make_shared<MetainfoMapImpl>(*other.metaInfo_);
  return (*this);
}

json::json SavepointImpl::toJSON() const {
  json::json jsonNode;
  jsonNode["name"] = name_;
  jsonNode["meta_info"] = *metaInfo_;
  return jsonNode;
}

void SavepointImpl::fromJSON(const json::json& jsonNode) {
  if(!metaInfo_)
    metaInfo_ = std::make_shared<MetainfoMapImpl>();

  name_.clear();
  metaInfo_->clear();

  if(jsonNode.is_null() || jsonNode.empty())
    throw Exception("node is empty");

  name_ = jsonNode.at("name");

  if(jsonNode.count("meta_info"))
    *metaInfo_ = jsonNode["meta_info"];
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
