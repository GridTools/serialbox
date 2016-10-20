//===-- serialbox/Core/Frontend/gridtools/Type.h ------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the gridtools type definitions.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_TYPE_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_TYPE_H

#include "serialbox/Core/Type.h"

namespace serialbox {

namespace gridtools {

/// \typedef open_mode
/// \brief Policy for opening files in the serializer
///
/// \ingroup gridtools
using open_mode = serialbox::OpenModeKind;

/// \enum type_id
/// \brief Type-id of types recognized by serialbox
///
/// \ingroup gridtools
using type_id = serialbox::TypeID;

} // namespace gridtools

} // namespace serialbox

#endif
