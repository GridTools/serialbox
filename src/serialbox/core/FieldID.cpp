//===-- serialbox/core/FieldID.cpp --------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file defines a unique identification of a savepoint within a field.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/FieldID.h"
#include <iostream>

namespace serialbox {

bool operator==(const FieldID& left, const FieldID& right) {
  return ((left.name == right.name) && (left.id == right.id));
}

bool operator!=(const FieldID& left, const FieldID& right) { return (!(left == right)); }

std::ostream& operator<<(std::ostream& stream, const FieldID& f) {
  return (stream << "{" << f.name << ", " << f.id << "}");
}

} // namespace serialbox
