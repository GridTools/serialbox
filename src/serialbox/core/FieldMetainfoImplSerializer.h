//===-- serialbox/core/FieldMetainfoImpl.h ------------------------------------------*- C++
//-*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Enable json <-> FieldMetainfoImpl conversions via ADL
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FIELDMAPMETAINFOIMPLSERIALIZER_H
#define SERIALBOX_CORE_FIELDMAPMETAINFOIMPLSERIALIZER_H

#include "serialbox/core/FieldMetainfoImpl.h"
#include <memory>

namespace serialbox {

/// \addtogroup core
/// @{
void to_json(json::json& j, FieldMetainfoImpl const& f);

void from_json(json::json const& j, FieldMetainfoImpl& f);
/// @}

} // namespace serialbox

#endif
