//===-- Support/Type.cpp ------------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains definitions for types recognized by Serialbox.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Support/Type.h"
#include "serialbox/Support/Unreachable.h"

namespace serialbox {

std::string TypeUtil::toString(TypeID id) {
  switch(id) {
  case TypeID::Boolean:
    return std::string("bool");
  case TypeID::Int32:
    return std::string("int");
  case TypeID::Int64:
    return std::string("int64");
  case TypeID::Float32:
    return std::string("float");
  case TypeID::Float64:
    return std::string("double");
  default:
    serialbox_unreachable("TypeID <Invalid>");
  }
}

int TypeUtil::sizeOf(TypeID id) noexcept {
  switch(id) {
  case TypeID::Boolean:
    return 1;
  case TypeID::Float32:
  case TypeID::Int32:
    return 4;
  case TypeID::Int64:
  case TypeID::Float64:
    return 8;
  default:
    serialbox_unreachable("TypeID <Invalid>");
  }
}

} // namespace serialbox
