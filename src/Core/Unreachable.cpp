//===-- Core/Unreachable.cpp --------------------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/Unreachable.h"
#include <cstdio>
#include <iostream>

namespace serialbox {

SERIALBOX_ATTRIBUTE_NORETURN void serialbox_unreachable_internal(const char* msg, const char* file,
                                                                 unsigned line) {
  if(msg)
    std::cerr << msg << "\n";
  std::cerr << "UNREACHABLE executed";
  if(file)
    std::cerr << " at " << file << ":" << line;
  std::cerr << "!" << std::endl;
  std::abort();
#ifdef LLVM_BUILTIN_UNREACHABLE
  LLVM_BUILTIN_UNREACHABLE;
#endif
}

} // namespace serialbox
