//===-- serialbox/Core/FieldID.h ----------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file defines the Field type.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FIELD_H
#define SERIALBOX_CORE_FIELD_H

#include <iosfwd>
#include <string>

namespace serialbox {

/// \brief Unique identification of a field
struct FieldID {
  std::string name; ///< Name of the field
  unsigned int id;  ///< ID within the field
};

} // namespace serialbox

#endif
