//===-- serialbox/Core/Frontend/STELLA/MetainfoSet.cpp ------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the meta-information set implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Frontend/STELLA/MetainfoSet.h"
#include "serialbox/Core/Frontend/STELLA/Utility.h"
#include "serialbox/Core/MetaInfoMap.h"
#include "serialbox/Core/Unreachable.h"

namespace serialbox {

namespace stella {

namespace internal {

template <class KeyType>
static MetaInfoMap::const_iterator checkKeyExists(const MetaInfoMap* mapImpl, KeyType&& key) {
  auto it = mapImpl->find(key);
  if(it == mapImpl->end())
    internal::throwSerializationException("Error: requested key %s is not in set", key);
  return it;
}

}

MetainfoSet::MetainfoSet() {
  mapImpl_ = new MetaInfoMap();
  owner_ = true;
}

MetainfoSet::~MetainfoSet() {
  if(owner_)
    delete mapImpl_;
}

MetainfoSet& MetainfoSet::operator=(const MetainfoSet& other) {
  mapImpl_->clear();
  *mapImpl_ = *other.mapImpl_;
  return (*this);
}

void MetainfoSet::Cleanup() { mapImpl_->clear(); }

bool MetainfoSet::HasKey(const std::string& key) const { return mapImpl_->hasKey(key); }

void MetainfoSet::AddMetainfo(const std::string& key, const bool& value) {
  if(!mapImpl_->insert(key, value))
    internal::throwSerializationException("Error: metainfo with key = %s exists already", key);
}

void MetainfoSet::AddMetainfo(const std::string& key, const int& value) {
  if(!mapImpl_->insert(key, value))
    internal::throwSerializationException("Error: metainfo with key = %s exists already", key);
}

void MetainfoSet::AddMetainfo(const std::string& key, const float& value) {
  if(!mapImpl_->insert(key, value))
    internal::throwSerializationException("Error: metainfo with key = %s exists already", key);
}

void MetainfoSet::AddMetainfo(const std::string& key, const double& value) {
  if(!mapImpl_->insert(key, value))
    internal::throwSerializationException("Error: metainfo with key = %s exists already", key);
}

void MetainfoSet::AddMetainfo(const std::string& key, const std::string& value) {
  if(!mapImpl_->insert(key, value))
    internal::throwSerializationException("Error: metainfo with key = %s exists already", key);
}

const boost::any& MetainfoSet::AsAny(const std::string& key) const {
  return internal::checkKeyExists(mapImpl_, key)->second.any();
}

bool MetainfoSet::AsBool(const std::string& key) const {
  MetaInfoValue value = internal::checkKeyExists(mapImpl_, key)->second;
  switch(value.type()) {
  case TypeID::Boolean:
    return value.as<bool>();
  case TypeID::Int32:
    return (bool)value.as<int>();
  case TypeID::Int64:
    return (bool)value.as<std::int64_t>();
  case TypeID::Float32:
    return (bool)value.as<float>();
  case TypeID::Float64:
    return (bool)value.as<double>();
  case TypeID::String: {
    std::stringstream ss;
    ss << value.as<std::string>();
    bool v;
    ss >> v;
    return v;
  }
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

int MetainfoSet::AsInt(const std::string& key) const {
  MetaInfoValue value = internal::checkKeyExists(mapImpl_, key)->second;
  switch(value.type()) {
  case TypeID::Boolean:
    return (int)value.as<bool>();
  case TypeID::Int32:
    return (int)value.as<int>();
  case TypeID::Int64:
    return (int)value.as<std::int64_t>();
  case TypeID::Float32: {
    float v = value.as<float>();
    if(float(int(v)) != v)
      internal::throwSerializationException(
          "Error: Trying to access the key %s as int, but its value is %f", key, v);
    return (int)v;
  }
  case TypeID::Float64: {
    double v = value.as<double>();
    if(double(int(v)) != v)
      internal::throwSerializationException(
          "Error: Trying to access the key %s as int, but its value is %f", key, v);
    return (int)v;
  }
  case TypeID::String: {
    std::stringstream ss;
    ss << value.as<std::string>();
    int v;
    ss >> v;
    return v;
  }
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

float MetainfoSet::AsFloat(const std::string& key) const {
  MetaInfoValue value = internal::checkKeyExists(mapImpl_, key)->second;
  switch(value.type()) {
  case TypeID::Boolean:
    return (float)value.as<bool>();
  case TypeID::Int32:
    return (float)value.as<int>();
  case TypeID::Int64:
    return (float)value.as<std::int64_t>();
  case TypeID::Float32:
    return value.as<float>();
  case TypeID::Float64:
    return (float)value.as<double>();
  case TypeID::String: {
    std::stringstream ss;
    ss << value.as<std::string>();
    float v;
    ss >> v;
    return v;
  }
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

double MetainfoSet::AsDouble(const std::string& key) const {
  MetaInfoValue value = internal::checkKeyExists(mapImpl_, key)->second;
  switch(value.type()) {
  case TypeID::Boolean:
    return (double)value.as<bool>();
  case TypeID::Int32:
    return (double)value.as<int>();
  case TypeID::Int64:
    return (double)value.as<std::int64_t>();
  case TypeID::Float32:
    return (double)value.as<float>();
  case TypeID::Float64:
    return value.as<double>();
  case TypeID::String: {
    std::stringstream ss;
    ss << value.as<std::string>();
    double v;
    ss >> v;
    return v;
  }
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

std::string MetainfoSet::AsString(const std::string& key) const {
  MetaInfoValue value = internal::checkKeyExists(mapImpl_, key)->second;
  switch(value.type()) {
  case TypeID::Boolean:
    return (value.as<bool>() ? "true" : "false");
  case TypeID::Int32:
    return std::to_string(value.as<int>());
  case TypeID::Int64:
    return std::to_string(value.as<std::int64_t>());
  case TypeID::Float32:
    return std::to_string(value.as<float>());
  case TypeID::Float64:
    return std::to_string(value.as<double>());
  case TypeID::String:
    return value.as<std::string>();
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

std::string MetainfoSet::ToString() const {
  std::ostringstream ss;
  ss << "[ ";
  for(auto it = mapImpl_->begin(), end = mapImpl_->end(); it != end; ++it)
    ss << it->first << "=" << it->second.toString() << " ";
  ss << "]";
  return ss.str();
}

std::size_t MetainfoSet::size() const { return mapImpl_->size(); }

void MetainfoSet::FromMetaInfoMap(MetaInfoMap* map) {
  if(owner_)
    delete mapImpl_;
  owner_ = false;
  mapImpl_ = map;
}

} // namespace stella

} // namespace serialbox
