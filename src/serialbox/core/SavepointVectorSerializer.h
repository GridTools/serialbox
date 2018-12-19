//===-- serialbox/core/SavepointVectorSerializer.h ----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Enable json <-> SavepointVector conversions via ADL
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SAVEPOINTVECTORSERIALIZER_H
#define SERIALBOX_CORE_SAVEPOINTVECTORSERIALIZER_H

#include "serialbox/core/Json.h"
#include "serialbox/core/SavepointVector.h"

namespace serialbox {

/// \addtogroup core
/// @{
void to_json(json::json& jsonNode, SavepointVector const& v);

void from_json(json::json const& jsonNode, SavepointVector& v);
/// @}

} // namespace serialbox

#endif
