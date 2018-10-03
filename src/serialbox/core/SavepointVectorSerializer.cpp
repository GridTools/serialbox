//===-- serialbox/core/SavepointVector.cpp ------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/SavepointVectorSerializer.h"
#include "serialbox/core/SavepointImplSerializer.h"

namespace serialbox {

void to_json(json::json& jsonNode, SavepointVector const& v) {
  assert(v.size() == v.fields().size());

  for(std::size_t i = 0; i < v.size(); ++i)
    jsonNode["savepoints"].push_back(*(v.savepoints()[i]));

  for(std::size_t i = 0; i < v.fields().size(); ++i) {
    const std::string& savepoint = v.savepoints()[i]->name();
    json::json fieldNode;

    if(v.fields()[i].empty())
      fieldNode[savepoint] = nullptr;

    for(auto it = v.fields()[i].begin(), end = v.fields()[i].end(); it != end; ++it)
      fieldNode[savepoint][it->first] = it->second;

    jsonNode["fields_per_savepoint"].push_back(fieldNode);
  }
}

void from_json(json::json const& jsonNode, SavepointVector& v) {
  v.clear();

  if(jsonNode.is_null() || jsonNode.empty())
    return;

  // Add savepoints
  if(jsonNode.count("savepoints")) {
    for(auto it = jsonNode["savepoints"].begin(), end = jsonNode["savepoints"].end(); it != end;
        ++it) {
      SavepointImpl sp = *it;
      v.insert(sp);
    }
  }

  // Eeach savepoint needs an entry in the fields array (it can be null though)
  if(jsonNode.count("fields_per_savepoint") &&
     jsonNode["fields_per_savepoint"].size() != v.fields().size())
    throw Exception("inconsistent number of 'fields_per_savepoint' and 'savepoints'");

  for(std::size_t i = 0; i < v.fields().size(); ++i) {
    const json::json& fieldNode = jsonNode["fields_per_savepoint"][i][v.savepoints()[i]->name()];

    // Savepoint has no fields
    if(fieldNode.is_null() || fieldNode.empty())
      break;

    // Add fields
    for(auto it = fieldNode.begin(), end = fieldNode.end(); it != end; ++it)
      v.fields()[i].insert({it.key(), static_cast<unsigned int>(it.value())});
  }
}

} // namespace serialbox
