//===-- Core/SavepointImpl.cpp ------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the shared implementation of all Savepoints.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Logging.h"
#include "serialbox/Core/SavepointImpl.h"

namespace serialbox {

void SavepointImpl::swap(SavepointImpl& other) noexcept {
  name_.swap(other.name_);
  metaInfo_.swap(other.metaInfo_);
  fields_.swap(other.fields_);
}

json::json SavepointImpl::toJSON() const {
  json::json jsonNode;
  jsonNode["name"] = name_;
  jsonNode["meta_info"] = metaInfo_.toJSON();
  for(const auto fieldID : fields_)
    jsonNode["fields"][fieldID.name] = fieldID.id;
  return jsonNode;
}

void SavepointImpl::fromJSON(const json::json& jsonNode) {
  name_.clear();
  metaInfo_.clear();
  fields_.clear();
  
  if(jsonNode.is_null() || jsonNode.empty())
    throw Exception("node is empty");

  if(!jsonNode.count("name"))
    throw Exception("no node 'name'");
  name_ = jsonNode["name"];

  if(!jsonNode.count("fields"))
    throw Exception("no node 'fields'");
  for(auto it = jsonNode["fields"].begin(), end = jsonNode["fields"].end(); it != end; ++it)
    fields_.push_back(FieldID{it.key(), static_cast<unsigned int>(it.value())});

  if(!jsonNode.count("meta_info"))
    throw Exception("no node 'meta_info'");
  metaInfo_.fromJSON(jsonNode["meta_info"]);    
}

void SavepointImpl::registerField(FieldID fieldID) {
  LOG(INFO) << "Registering field \"" << fieldID.name << "\" within savepoint \"" << name_ << "\"";
  if(hasField(fieldID.name))
    throw Exception("savepoint '%s' already has a field regsistered as '%s", name_, fieldID.name);
  fields_.push_back(fieldID);
}

const FieldID& SavepointImpl::getFieldID(const std::string& name) const {
  for(const auto& fieldID : fields_)
    if(fieldID.name == name)
      return fieldID;
  throw Exception("savepoint '%s' has no field '%s'", name_, name);
}

std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s) {
  return (stream << s.toJSON().dump(4));
}

} // namespace serialbox
