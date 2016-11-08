//===-- serialbox/core/FieldID.h ----------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file defines the FielID to uniquely identifiy a field.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FIELDID_H
#define SERIALBOX_CORE_FIELDID_H

#include <iosfwd>
#include <string>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Uniquely identifiy a field
struct FieldID {
  std::string name; ///< Name of the field
  unsigned int id;  ///< ID within the field
};

/// \brief Check for equality of FieldIDs
bool operator==(const FieldID& left, const FieldID& right);

/// \brief Check for inequality of FieldIDs
bool operator!=(const FieldID& left, const FieldID& right);

/// \brief Convert FieldID to stream
std::ostream& operator<<(std::ostream& stream, const FieldID& f);

/// @}

} // namespace serialbox

#endif
