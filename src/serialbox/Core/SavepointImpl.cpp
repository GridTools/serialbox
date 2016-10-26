//===-- serialbox/Core/SavepointImpl.cpp --------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/SavepointImpl.h"
#include "serialbox/Core/Logging.h"
#include <sstream>

namespace serialbox {

SavepointImpl& SavepointImpl::operator=(const SavepointImpl& other) {
  name_ = other.name_;
  metaInfo_ = std::make_shared<MetaInfoMap>(*other.metaInfo_);
  return (*this);
}

json::json SavepointImpl::toJSON() const {
  json::json jsonNode;
  jsonNode["name"] = name_;
  jsonNode["meta_info"] = metaInfo_->toJSON();
  return jsonNode;
}

void SavepointImpl::fromJSON(const json::json& jsonNode) {
  if(!metaInfo_)
    metaInfo_ = std::make_shared<MetaInfoMap>();

  name_.clear();
  metaInfo_->clear();

  if(jsonNode.is_null() || jsonNode.empty())
    throw Exception("node is empty");

  if(!jsonNode.count("name"))
    throw Exception("no node 'name'");
  name_ = jsonNode["name"];

  if(jsonNode.count("meta_info"))
    metaInfo_->fromJSON(jsonNode["meta_info"]);
}

std::string SavepointImpl::toString() const {
  std::stringstream ss;
  ss << *this;
  return ss.str();
}

std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s) {
  std::stringstream ss;
  ss << s.name_;
  ss << " [ ";
  for(auto it = s.metaInfo_->begin(), end = s.metaInfo_->end(); it != end; ++it)
    ss << it->first << " = " << it->second.toString() << ", ";
  std::string str = ss.str();
  str[str.size() - 2] = ' ';
  str[str.size() - 1] = ']';
  return (stream << str);
}

} // namespace serialbox
