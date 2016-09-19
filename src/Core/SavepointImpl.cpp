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

json::json SavepointImpl::toJSON() const {
  json::json jsonNode;

  return jsonNode;
}

void SavepointImpl::fromJSON(const json::json& jsonNode) {}

void SavepointImpl::registerField(FieldID fieldID) {
  LOG(INFO) << "Registering field \"" << fieldID.name << "\" within savepoint \"" << name_ << "\"";
  if(hasField(fieldID.name))
    throw Exception("savepoint '%s' already has a field regsistered as '%s");
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
