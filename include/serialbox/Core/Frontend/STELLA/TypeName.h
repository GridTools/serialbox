//===-- serialbox/Core/Frontend/STELLA/TypeName.h -----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the type_name class which maps types to strings.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_TYPENAME_H
#define SERIALBOX_CORE_FRONTEND_STELLA_TYPENAME_H

namespace serialbox {

namespace stella {

#include <string>

template <typename TData>
inline std::string type_name();

#define SERIALBOX_TYPENAMEFUNCTION(type)                                                           \
  template <>                                                                                      \
  inline std::string type_name<type>() {                                                           \
    return #type;                                                                                  \
  }

SERIALBOX_TYPENAMEFUNCTION(int)
SERIALBOX_TYPENAMEFUNCTION(float)
SERIALBOX_TYPENAMEFUNCTION(double)

} // namespace stella

} // namespace serialbox

#endif
