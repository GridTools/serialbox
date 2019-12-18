//===-- serialbox/core/FieldMap.cpp -------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the field map which stores the meta-information of each field.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/FieldMap.h"
#include "serialbox/core/FieldMapSerializer.h"

namespace serialbox {

std::ostream& operator<<(std::ostream& stream, const FieldMap& s) {
  return (stream << "FieldMap = " << json::json{s}.dump(4));
}

} // namespace serialbox
