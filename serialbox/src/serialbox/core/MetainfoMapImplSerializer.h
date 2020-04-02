//===-- serialbox/core/MetainfoMapImplSerializer.h ----------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_METAINFOMAPIMPLSERIALIZER_H
#define SERIALBOX_CORE_METAINFOMAPIMPLSERIALIZER_H

#include "serialbox/core/Json.h"
#include "serialbox/core/MetainfoMapImpl.h"

namespace serialbox {

/// \addtogroup core
/// @{
void to_json(json::json& jsonNode, MetainfoMapImpl const& map);

void from_json(json::json const& jsonNode, MetainfoMapImpl& map);
/// @}

} // namespace serialbox

#endif
