//===-- Core/Savepoint.cpp ----------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the implementation the Savepoint.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Logging.h"
#include "serialbox/Core/Savepoint.h"

namespace serialbox {

json::json Savepoint::toJSON() const {
  json::json jsonNode;
  jsonNode["name"] = name_;
  jsonNode["meta_info"] = metaInfo_.toJSON();
  return jsonNode;
}

void Savepoint::fromJSON(const json::json& jsonNode) {
  name_.clear();
  metaInfo_.clear();

  if(jsonNode.is_null() || jsonNode.empty())
    throw Exception("node is empty");

  if(!jsonNode.count("name"))
    throw Exception("no node 'name'");
  name_ = jsonNode["name"];

  if(jsonNode.count("meta_info"))
    metaInfo_.fromJSON(jsonNode["meta_info"]);
}

std::ostream& operator<<(std::ostream& stream, const Savepoint& s) {
  stream << s.name_ << " [ ";
  for(auto it = s.metaInfo_.begin(), end = s.metaInfo_.end(); it != end; ++it)
    stream << it->first << " = " << it->second.toString() << " ";
  stream << "]\n";
  return stream;
}

} // namespace serialbox
