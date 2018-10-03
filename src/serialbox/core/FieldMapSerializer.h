//===-- serialbox/core/FieldMap.h ---------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Enable json <-> FieldMap conversions via ADL
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FIELDMAP_SERIALIZER_H
#define SERIALBOX_CORE_FIELDMAP_SERIALIZER_H

#include "serialbox/core/FieldMap.h"
#include "serialbox/core/Json.h"

namespace serialbox {

/// \addtogroup core
/// @{
void to_json(json::json& j, FieldMap const& fieldMap);

void from_json(json::json const& j, FieldMap& fieldMap);
/// @}

} // namespace serialbox

#endif
