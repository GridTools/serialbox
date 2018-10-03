//===-- serialbox/core/MetainfoMapImpl.cpp ------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/MetainfoMapImpl.h"
#include "serialbox/core/Array.h"
#include "serialbox/core/Unreachable.h"
#include <iostream>

namespace serialbox {

std::vector<std::string> MetainfoMapImpl::keys() const {
  std::vector<std::string> keys;
  keys.reserve(map_.size());
  for(auto it = map_.begin(), end = map_.end(); it != end; ++it)
    keys.push_back(it->first);
  return keys;
}

std::vector<TypeID> MetainfoMapImpl::types() const {
  std::vector<TypeID> types;
  types.reserve(map_.size());
  for(auto it = map_.begin(), end = map_.end(); it != end; ++it)
    types.push_back(it->second.type());
  return types;
}

MetainfoMapImpl::mapped_type& MetainfoMapImpl::at(const MetainfoMapImpl::key_type& key) {
  try {
    return map_.at(key);
  } catch(std::out_of_range&) {
    throw Exception("no key '%s' exists", key);
  }
}

const MetainfoMapImpl::mapped_type&
MetainfoMapImpl::at(const MetainfoMapImpl::key_type& key) const {
  try {
    return map_.at(key);
  } catch(std::out_of_range&) {
    throw Exception("no key '%s' exists", key);
  }
}

std::ostream& operator<<(std::ostream& stream, const MetainfoMapImpl& s) {
  std::stringstream ss;
  ss << "{";

  for(auto it = s.begin(), end = s.end(); it != end; ++it) {
    ss << "\"" << it->first << "\": ";

    if(TypeUtil::isArray(it->second.type()))
      ss << "[" << ArrayUtil::toString(it->second.as<Array<std::string>>()) << "]";
    else
      ss << it->second.as<std::string>();

    auto itCp = it;
    if(++itCp != s.end())
      ss << ", ";
  }
  ss << "}";
  return (stream << ss.str());
}

} // namespace serialbox
