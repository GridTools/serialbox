//===-- Core/FieldID.h --------------------------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/FieldID.h"
#include <iostream>

namespace serialbox {

bool operator==(const FieldID& left, const FieldID& right) {
  return ((left.name == right.name) && (left.id == right.id));
}

bool operator!=(const FieldID& left, const FieldID& right) { return (!(left == right)); }

std::ostream& operator<<(std::ostream& stream, const FieldID& f) {
  stream << "{\n";
  stream << "  name: " << f.name << "\n";
  stream << "  id: " << f.id << "\n";
  stream << "}";
  return stream;
}

} // namespace serialbox
