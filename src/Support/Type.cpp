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

namespace serialbox {

std::string to_string(TypeID id) {
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
  case TypeID::String:
    return std::string("string");
  default:
    return std::string("unknown");
  }
}

} // namespace serialbox
