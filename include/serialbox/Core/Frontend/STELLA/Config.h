//===-- serialbox/Core/Frontend/STELLA/Config.h -------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the configuration of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_CONFIG_H
#define SERIALBOX_CORE_FRONTEND_STELLA_CONFIG_H

/// \macro SERIALBOX_STELLA_NO_INCLUDE
/// \brief If defined, do not include the STELLA headers
/// 
/// Note that the STELLA frontend depends on the \b definitions defined by "SharedInfrastructure.h".
#ifndef SERIALBOX_STELLA_NO_INCLUDE
#include "SharedInfrastructure.h"
#endif

#endif
