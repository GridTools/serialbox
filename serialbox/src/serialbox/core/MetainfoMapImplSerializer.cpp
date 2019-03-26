//===-- serialbox/core/MetainfoMapImplSerializer.cpp --------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//

#include "MetainfoMapImplSerializer.h"
#include "serialbox/core/Unreachable.h"

namespace serialbox {

namespace {

struct InsertHelper {
  // Capture environment
  MetainfoMapImpl& map;
  const std::string& key;
  const json::json& node;

  // Read value from JSON node (and check if the types match) and insert it into the MetainfoMapImpl
  // as
  // type ´T´
  template <class T, class CheckFunction>
  void insertAs(CheckFunction&& checkFunction, const char* valueStr) {
    if(!(node["value"].*checkFunction)())
      throw Exception("sub-node '%s' not regconized as %s", key, valueStr);
    T value = node["value"];
    map.insert(key, value);
  }

  // Read value from JSON node as Array of type ´T´ and insert as array of type ´T´ into the
  // MetainfoMapImpl
  template <class T>
  void insertAsArrayOf() {
    Array<T> array = node["value"];
    map.insert(key, array);
  }
};

} // namespace

void to_json(json::json& jsonNode, MetainfoMapImpl const& map) {
  for(auto it = map.begin(), end = map.end(); it != end; ++it) {
    const MetainfoValueImpl& value = it->second;
    const std::string& key = it->first;

    jsonNode[key]["type_id"] = static_cast<int>(value.type());

    json::json valueNode;
    const bool isArray = TypeUtil::isArray(value.type());

    switch(TypeUtil::getPrimitive(value.type())) {
    case TypeID::Boolean:
      if(isArray) {
        for(const bool& v : value.as<Array<bool>>())
          valueNode.push_back(v);
      } else {
        valueNode = value.as<bool>();
      }
      break;
    case TypeID::Int32:
      if(isArray) {
        for(const int& v : value.as<Array<int>>())
          valueNode.push_back(v);
      } else {
        valueNode = value.as<int>();
      }
      break;
    case TypeID::Int64:
      if(isArray) {
        for(const std::int64_t& v : value.as<Array<std::int64_t>>())
          valueNode.push_back(v);
      } else {
        valueNode = value.as<std::int64_t>();
      }
      break;
    case TypeID::Float32:
      if(isArray) {
        for(const float& v : value.as<Array<float>>())
          valueNode.push_back(v);
      } else {
        valueNode = value.as<float>();
      }
      break;
    case TypeID::Float64:
      if(isArray) {
        for(const double& v : value.as<Array<double>>())
          valueNode.push_back(v);
      } else {
        valueNode = value.as<double>();
      }
      break;
    case TypeID::String:
      if(isArray) {
        for(const std::string& v : value.as<Array<std::string>>())
          valueNode.push_back(v);
      } else {
        valueNode = value.as<std::string>();
      }
      break;
    default:
      serialbox_unreachable("Invalid TypeID");
    }

    jsonNode[key]["value"] = valueNode;
  }
}

void from_json(json::json const& jsonNode, MetainfoMapImpl& map) {
  map.clear();

  if(jsonNode.is_null() || jsonNode.empty())
    return;

  for(auto it = jsonNode.begin(), end = jsonNode.end(); it != end; ++it) {

    if(!it->count("type_id"))
      throw Exception("sub-node '%s' has no node 'type_id'", it.key());

    if(!it->count("value"))
      throw Exception("sub-node '%s' has no node 'value'", it.key());

    const json::json& node = it.value();
    const std::string& key = it.key();
    const int typeAsInt = node["type_id"];
    const TypeID type = static_cast<TypeID>(typeAsInt);
    const bool isArray = TypeUtil::isArray(type);

    InsertHelper insertHelper{map, key, node};

    switch(TypeUtil::getPrimitive(type)) {
    case TypeID::Boolean:
      if(isArray) {
        insertHelper.insertAsArrayOf<bool>();
      } else {
        insertHelper.insertAs<bool>(&json::json::is_boolean, "boolean");
      }
      break;
    case TypeID::Int32:
      if(isArray) {
        insertHelper.insertAsArrayOf<int>();
      } else {
        insertHelper.insertAs<int>(&json::json::is_number_integer, "integer");
      }
      break;
    case TypeID::Int64:
      if(isArray) {
        insertHelper.insertAsArrayOf<std::int64_t>();
      } else {
        insertHelper.insertAs<std::int64_t>(&json::json::is_number_integer, "integer");
      }
      break;
    case TypeID::Float32:
      if(isArray) {
        insertHelper.insertAsArrayOf<float>();
      } else {
        insertHelper.insertAs<float>(&json::json::is_number, "floating pointer number");
      }
      break;
    case TypeID::Float64:
      if(isArray) {
        insertHelper.insertAsArrayOf<double>();
      } else {
        insertHelper.insertAs<double>(&json::json::is_number, "floating pointer number");
      }
      break;
    case TypeID::String:
      if(isArray) {
        insertHelper.insertAsArrayOf<std::string>();
      } else {
        insertHelper.insertAs<std::string>(&json::json::is_string, "string");
      }
      break;
    default:
      serialbox_unreachable("Invalid TypeID");
    }
  }
}

} // namespace serialbox
