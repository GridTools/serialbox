//===-- serialbox/core/SavepointImplSerializer.cpp ----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/SavepointImplSerializer.h"
#include "serialbox/core/MetainfoMapImplSerializer.h"

namespace serialbox {

void to_json(json::json& jsonNode, SavepointImpl const& savepoint) {
  jsonNode["name"] = savepoint.name();
  jsonNode["meta_info"] = savepoint.metaInfo();
}

void from_json(json::json const& jsonNode, SavepointImpl& savepoint) {
  if(!savepoint.metaInfoPtr())
    savepoint.metaInfoPtr() = std::make_shared<MetainfoMapImpl>();

  savepoint.metaInfo().clear();

  if(jsonNode.is_null() || jsonNode.empty())
    throw Exception("node is empty");

  savepoint.setName(jsonNode.at("name"));

  if(jsonNode.count("meta_info"))
    savepoint.metaInfo() = jsonNode["meta_info"];
}

} // namespace serialbox
