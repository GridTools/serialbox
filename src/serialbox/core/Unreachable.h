//===-- serialbox/core/Unreachable.h ------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Provide utility to indicate the current location is not supposed to be reachable.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_UNREACHABLE_H
#define SERIALBOX_CORE_UNREACHABLE_H

#include "serialbox/core/Compiler.h"

namespace serialbox {

/// \addtogroup core
/// @{

/// \fn serialbox_unreachable_internal
/// \brief This function calls abort() and prints the optional message to stderr
///
/// Use the llvm_unreachable macro (that adds location info), instead of
/// calling this function directly.
SERIALBOX_ATTRIBUTE_NORETURN void serialbox_unreachable_internal(const char* msg = nullptr,
                                                                 const char* file = nullptr,
                                                                 unsigned line = 0);

/// \macro serialbox_unreachable
/// \brief Marks that the current location is not supposed to be reachable
///
/// In !NDEBUG builds, prints the message and location info to stderr. In NDEBUG builds, becomes an
/// optimizer hint that the current location is not supposed to be reachable. On compilers that
/// don't support such hints, prints a reduced message instead.
#ifndef NDEBUG
#define serialbox_unreachable(msg)                                                                 \
  ::serialbox::serialbox_unreachable_internal(msg, __FILE__, __LINE__)
#elif defined(SERIALBOX_BUILTIN_UNREACHABLE)
#define serialbox_unreachable(msg) SERIALBOX_BUILTIN_UNREACHABLE
#else
#define serialbox_unreachable(msg) ::serialbox::serialbox_unreachable_internal()
#endif

/// @}

} // namespace serialbox

#endif
