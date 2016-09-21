//===-- Core/FieldMap.cpp -----------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the field map which stores the meta-information of each field.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/FieldMap.h"

namespace serialbox {

json::json FieldMap::toJSON() const {
  json::json jsonNode;

  if(empty())
    return jsonNode;

  for(const_iterator it = this->begin(), end = this->end(); it != end; ++it)
    jsonNode[it->first] = it->second.toJSON();

  return jsonNode;
}

void FieldMap::fromJSON(const json::json& jsonNode) {
  this->clear();

  if(jsonNode.is_null() || jsonNode.empty())
    return;

  for(auto it = jsonNode.begin(), end = jsonNode.end(); it != end; ++it) {
    try {
      insert(it.key(), it.value());
    } catch(Exception& e) {
      throw Exception("cannot insert node '%s' in FieldMap: JSON node ill-formed: %s", it.key(),
                      e.what());
    }
  }
}

std::ostream& operator<<(std::ostream& stream, const FieldMap& s) {
  return (stream << "FieldMap = " << s.toJSON().dump(4));
}

} // namespace serialbox
