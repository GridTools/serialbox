//===-- serialbox/core/FieldMapSerializer.cpp ----------------------------------------*- C+ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/FieldMapSerializer.h"
#include "serialbox/core/FieldMetainfoImplSerializer.h"

namespace serialbox {

void to_json(json::json& j, FieldMap const& fieldMap) {
  for(FieldMap::const_iterator it = fieldMap.begin(), end = fieldMap.end(); it != end; ++it)
    j[it->first] = *(it->second);
}

void from_json(json::json const& j, FieldMap& fieldMap) {
  fieldMap.clear();

  if(j.is_null() || j.empty())
    return;

  for(auto it = j.begin(), end = j.end(); it != end; ++it) {
    try {
      fieldMap.insert(it.key(), it.value());
    } catch(Exception& e) {
      throw Exception("cannot insert node '%s' in FieldMap: JSON node ill-formed: %s", it.key(),
                      e.what());
    }
  }
}

} // namespace serialbox
