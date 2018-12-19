//===-- serialbox/core/SavepointImplSerializer.h ------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_SAVEPOINTIMPLSERIALIZER_H
#define SERIALBOX_CORE_SAVEPOINTIMPLSERIALIZER_H

#include "serialbox/core/Json.h"
#include "serialbox/core/SavepointImpl.h"

namespace serialbox {

/// \addtogroup core
/// @{
void to_json(json::json& jsonNode, SavepointImpl const& savepoint);

void from_json(json::json const& jsonNode, SavepointImpl& savepoint);
/// @}

} // namespace serialbox

#endif
