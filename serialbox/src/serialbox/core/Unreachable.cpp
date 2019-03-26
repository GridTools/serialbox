//===-- serialbox/core/Unreachable.cpp ----------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/Unreachable.h"
#include <cstdio>
#include <iostream>

namespace serialbox {

SERIALBOX_ATTRIBUTE_NORETURN void serialbox_unreachable_internal(const char* msg, const char* file,
                                                                 unsigned line) {
  std::cerr << "FATAL ERROR: UNREACHABLE executed : ";
  if(msg)
    std::cerr << "\"" << msg << "\"";
  if(file)
    std::cerr << " at " << file << ":" << line;
  std::cerr << std::endl;
  std::abort();

#ifdef SERIALBOX_BUILTIN_UNREACHABLE
  SERIALBOX_BUILTIN_UNREACHABLE;
#endif
}

} // namespace serialbox
