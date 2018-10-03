//===-- serialbox/core/FieldMetainfoImplImpl.h ------------------------------------------*- C++
//-*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/FieldMetainfoImplSerializer.h"
#include "serialbox/core/MetainfoMapImplSerializer.h"

namespace serialbox {

void to_json(json::json& j, FieldMetainfoImpl const& f) {
  j["type_id"] = static_cast<int>(f.type());
  j["dims"] = f.dims();
  j["meta_info"] = f.metaInfo();
}

void from_json(json::json const& j, FieldMetainfoImpl& f) {
  f.dims().clear();
  f.metaInfo().clear();

  if(j.is_null() || j.empty())
    throw Exception("node is empty");

  if(!j.count("type_id"))
    throw Exception("no node 'type_id'");
  f.type() = static_cast<TypeID>(int(j["type_id"]));

  if(!j.count("dims"))
    throw Exception("no node 'value'");
  for(auto it = j["dims"].begin(), end = j["dims"].end(); it != end; ++it)
    f.dims().push_back(int(*it));

  f.metaInfo() = j.at("meta_info");
}

} // namespace serialbox
