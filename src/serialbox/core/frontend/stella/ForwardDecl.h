//===-- serialbox/core/frontend/stella/ForwardDecl.h --------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains forward declaration of the Core infrastructure.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_FORWARDDECL_H
#define SERIALBOX_CORE_FRONTEND_STELLA_FORWARDDECL_H

#include "serialbox/core/Compiler.h"

namespace serialbox {

class FieldMap;
class FieldMetainfoImpl;
class MetainfoMapImpl;
class MetainfoValueImpl;
class SerializerImpl;
class SavepointImpl;

/// \namespace stella
/// \brief Namespace of the STELLA frontend
namespace stella {}

} // namespace serialbox

/// \namespace ser
/// \brief Namespace of the old serialbox library
namespace ser = serialbox::stella;

#endif
