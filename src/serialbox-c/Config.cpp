/*===-- serialbox-c/Config.cpp ------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains platform specific definitions of the Serialbox C Interface.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Config.h"
#include "serialbox-c/Utility.h"
#include <string>

char* serialboxConfigOptions(void) {
  std::string options;

  // BOOST_LIB_VERSION
  options += "BOOST_VERSION=" BOOST_LIB_VERSION ";";

  // SERIALBOX_HAS_OPENSSL
  options += "SERIALBOX_HAS_OPENSSL=" + std::to_string(SERIALBOX_HAS_OPENSSL) + ";";

  // SERIALBOX_HAS_NETCDF
  options += "SERIALBOX_HAS_NETCDF=" + std::to_string(SERIALBOX_HAS_NETCDF) + ";";

  // SERIALBOX_C_COMPILER
  options += "SERIALBOX_C_COMPILER=" SERIALBOX_C_COMPILER_STRING ";";

  // SERIALBOX_CXX_COMPILER
  options += "SERIALBOX_CXX_COMPILER=" SERIALBOX_CXX_COMPILER_STRING ";";

  // SERIALBOX_C_FLAGS
  options += "SERIALBOX_C_FLAGS=" SERIALBOX_C_FLAGS ";";

  // SERIALBOX_CXX_FLAGS
  options += "SERIALBOX_CXX_FLAGS=" SERIALBOX_CXX_FLAGS;

  return serialboxC::allocateAndCopyString(options);
}
