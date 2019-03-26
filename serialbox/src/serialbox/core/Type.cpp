//===-- serialbox/core/Type.cpp -----------------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/Type.h"
#include "serialbox/core/Exception.h"
#include "serialbox/core/Unreachable.h"
#include <ostream>

namespace serialbox {

std::ostream& operator<<(std::ostream& stream, const OpenModeKind& mode) {
  switch(mode) {
  case OpenModeKind::Write:
    return (stream << std::string("Write"));
  case OpenModeKind::Read:
    return (stream << std::string("Read"));
  case OpenModeKind::Append:
    return (stream << std::string("Append"));
  default:
    serialbox_unreachable("invalid OpenModeKind");
  }
}

std::string TypeUtil::toString(TypeID id) {
  std::string str;

  if(TypeUtil::isArray(id))
    str += "std::vector<";

  switch(TypeUtil::getPrimitive(id)) {
  case TypeID::Boolean:
    str += "bool";
    break;
  case TypeID::Int32:
    str += "int";
    break;
  case TypeID::Int64:
    str += "std::int64_t";
    break;
  case TypeID::Float32:
    str += "float";
    break;
  case TypeID::Float64:
    str += "double";
    break;
  case TypeID::String:
    str += "std::string";
    break;
  case TypeID::Invalid:
    str += "invalid-type";
    break;
  default:
    serialbox_unreachable("invalid TypeID for TypeUtil::toString");
  }

  if(TypeUtil::isArray(id))
    str += ">";

  return str;
}

int TypeUtil::sizeOf(TypeID id) {
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
    throw Exception("invalid type '%s' for TypeUtil::sizeOf", TypeUtil::toString(id));
  }
  serialbox_unreachable("unreachable");
}

bool TypeUtil::isPrimitive(TypeID id) noexcept { return (!TypeUtil::isArray(id)); }

bool TypeUtil::isArray(TypeID id) noexcept { return ((int)id & (int)TypeID::Array); }

TypeID TypeUtil::getPrimitive(TypeID id) noexcept {
  if(TypeUtil::isArray(id))
    return TypeID((int)id & ~((int)TypeID::Array));
  return id;
}

TypeID TypeUtil::getArray(TypeID id) noexcept {
  if(TypeUtil::isArray(id))
    return id;
  return TypeID((int)id | ((int)TypeID::Array));
}

std::ostream& operator<<(std::ostream& stream, const TypeID& t) {
  return (stream << TypeUtil::toString(t));
}

} // namespace serialbox
