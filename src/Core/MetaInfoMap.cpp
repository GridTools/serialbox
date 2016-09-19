//===-- serialbox/Core/MetaInfoMap.cpp ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the meta-information map.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/MetaInfoMap.h"
#include "serialbox/Core/Unreachable.h"
#include <iostream>

namespace serialbox {

namespace {

struct InsertHelper {
  // Capture environment
  MetaInfoMap& map;
  const std::string& key;
  const json::json& node;

  // Read value from JSON node (and check if the types match) and insert it into the MetaInfoMap
  // (this is just a fancy way to avoid ugly macros)
  template <class T, class CheckFunction>
  void insert(CheckFunction&& checkFunction, const char* valueStr) {
    if(!(node["value"].*checkFunction)())
      throw Exception("sub-node '%s' not regconized as %s", key, valueStr);
    T value = node["value"];
    map.insert(key, value);
  }
};

} // anonymous namespace

MetaInfoMap::mapped_type& MetaInfoMap::at(const MetaInfoMap::key_type& key) {
  try {
    return map_.at(key);
  } catch(std::out_of_range&) {
    throw Exception("no key '%s' exists in MetaInfoMap", key);
  }
}

const MetaInfoMap::mapped_type& MetaInfoMap::at(const MetaInfoMap::key_type& key) const {
  try {
    return map_.at(key);
  } catch(std::out_of_range&) {
    throw Exception("no key '%s' exists in MetaInfoMap", key);
  }
}

json::json MetaInfoMap::toJSON() const {
  json::json j;

  if(map_.empty())
    return j;

  for(auto it = map_.cbegin(), end = map_.cend(); it != end; ++it) {
    const MetaInfoValue& value = it->second;
    const std::string& key = it->first;

    j[key]["type_id"] = static_cast<int>(value.type());
    switch(value.type()) {
    case TypeID::Boolean:
      j[key]["value"] = value.as<bool>();
      break;
    case TypeID::Int32:
      j[key]["value"] = value.as<int>();
      break;
    case TypeID::Int64:
      j[key]["value"] = value.as<std::int64_t>();
      break;
    case TypeID::Float32:
      j[key]["value"] = value.as<float>();
      break;
    case TypeID::Float64:
      j[key]["value"] = value.as<double>();
      break;
    case TypeID::String:
      j[key]["value"] = value.as<std::string>();
      break;
    default:
      serialbox_unreachable("Invalid TypeID");
    }
  }
  return j;
}

void MetaInfoMap::fromJSON(const json::json& jsonNode) {
  map_.clear();

  if(jsonNode.is_null() || jsonNode.empty())
    return;

  for(auto it = jsonNode.begin(), end = jsonNode.end(); it != end; ++it) {

    if(!it->count("type_id"))
      throw Exception("sub-node '%s' has no node 'type_id'", it.key());

    if(!it->count("value"))
      throw Exception("sub-node '%s' has no node 'value'", it.key());

    const json::json& node = it.value();
    int typeAsInt = node["type_id"];

    InsertHelper insertHelper{*this, it.key(), node};
    switch(static_cast<TypeID>(typeAsInt)) {
    case TypeID::Boolean:
      insertHelper.insert<bool>(&json::json::is_boolean, "boolean");
      break;
    case TypeID::Int32:
      insertHelper.insert<int>(&json::json::is_number_integer, "integer");
      break;
    case TypeID::Int64:
      insertHelper.insert<std::int64_t>(&json::json::is_number_integer, "integer");
      break;
    case TypeID::Float32:
      insertHelper.insert<float>(&json::json::is_number, "floating pointer number");
      break;
    case TypeID::Float64:
      insertHelper.insert<double>(&json::json::is_number, "floating pointer number");
      break;
    case TypeID::String:
      insertHelper.insert<std::string>(&json::json::is_string, "string");
      break;
    default:
      serialbox_unreachable("Invalid TypeID");
    }
  }
}

std::ostream& operator<<(std::ostream& stream, const MetaInfoMap& s) {
  json::json j(s.toJSON());
  json::json jDump;
  for(auto it = j.begin(), end = j.end(); it != end; ++it)
    jDump[it.key()] = it.value()["value"];
  stream << "MetaInfoMap = " << jDump.dump(4);
  return stream;
}

} // namespace serialbox
