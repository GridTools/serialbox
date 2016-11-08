//===-- serialbox/core/frontend/stella/MetainfoSet.cpp ------------------------------*- C++ -*-===//
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

#include "serialbox/core/frontend/stella/MetainfoSet.h"
#include "serialbox/core/Exception.h"
#include "serialbox/core/MetainfoMapImpl.h"
#include "serialbox/core/Unreachable.h"
#include "serialbox/core/frontend/stella/Utility.h"
#include <boost/algorithm/string.hpp>

namespace serialbox {

namespace stella {

namespace internal {

template <class KeyType>
static MetainfoMapImpl::const_iterator
checkKeyExists(const boost::shared_ptr<MetainfoMapImpl>& mapImpl, KeyType&& key) {
  auto it = mapImpl->find(key);
  if(it == mapImpl->end())
    internal::throwSerializationException("Error: requested key %s is not in set", key);
  return it;
}
}

MetainfoSet::MetainfoSet() : mapImpl_(boost::make_shared<MetainfoMapImpl>()) {}

MetainfoSet::MetainfoSet(const boost::shared_ptr<MetainfoMapImpl>& map) : mapImpl_(map){};

MetainfoSet::MetainfoSet(const MetainfoSet& other) { mapImpl_ = other.mapImpl_; }

MetainfoSet& MetainfoSet::operator=(const MetainfoSet& other) {
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
  MetainfoValueImpl value = internal::checkKeyExists(mapImpl_, key)->second;
  return value.as<bool>();
}

int MetainfoSet::AsInt(const std::string& key) const {
  MetainfoValueImpl value = internal::checkKeyExists(mapImpl_, key)->second;
  try {
    return value.as<int>();
  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
  serialbox_unreachable("unreachable");
}

float MetainfoSet::AsFloat(const std::string& key) const {
  MetainfoValueImpl value = internal::checkKeyExists(mapImpl_, key)->second;
  return value.as<float>();
}

double MetainfoSet::AsDouble(const std::string& key) const {
  MetainfoValueImpl value = internal::checkKeyExists(mapImpl_, key)->second;
  return value.as<double>();
}

std::string MetainfoSet::AsString(const std::string& key) const {
  MetainfoValueImpl value = internal::checkKeyExists(mapImpl_, key)->second;
  return value.as<std::string>();
}

std::string MetainfoSet::ToString() const {
  std::ostringstream ss;
  ss << "[ ";
  for(auto it = mapImpl_->begin(), end = mapImpl_->end(); it != end; ++it)
    if(!boost::algorithm::starts_with(it->first, "__"))
      ss << it->first << "=" << it->second.toString() << " ";
  ss << "]";
  return ss.str();
}

std::size_t MetainfoSet::size() const {
  std::size_t s = 0;
  for(auto it = mapImpl_->begin(), end = mapImpl_->end(); it != end; ++it)
    if(!boost::algorithm::starts_with(it->first, "__"))
      s++;
  return s;
}

bool MetainfoSet::operator==(const MetainfoSet& other) const {
  return (*mapImpl_ == *other.mapImpl_);
}

void MetainfoSet::setImpl(const boost::shared_ptr<MetainfoMapImpl>& metaInfoMap) {
  mapImpl_ = metaInfoMap;
}

std::vector<std::string> MetainfoSet::keys() const {
  std::vector<std::string> keyStr;
  for(auto it = mapImpl_->begin(), end = mapImpl_->end(); it != end; ++it)
    keyStr.push_back(it->first);
  return keyStr;
}

boost::shared_ptr<MetainfoMapImpl>& MetainfoSet::getImpl() { return mapImpl_; }
const boost::shared_ptr<MetainfoMapImpl>& MetainfoSet::getImpl() const { return mapImpl_; }

} // namespace stella

} // namespace serialbox
