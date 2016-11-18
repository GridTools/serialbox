/*===-- serialbox-c/ConfigOptions.cpp -----------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file exposes the platform and compiler specific options.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Config.h"
#include "serialbox-c/ConfigOptions.h"
#include "serialbox-c/Utility.h"
#include <string>

char* serialboxConfigOptions(void) {
  std::string options;

  // BOOST_LIB_VERSION
  options += "BOOST_VERSION=" BOOST_LIB_VERSION ";";

#ifdef SERIALBOX_HAS_OPENSSL
  // SERIALBOX_HAS_OPENSSL
  options += "SERIALBOX_HAS_OPENSSL=" + std::to_string(SERIALBOX_HAS_OPENSSL) + ";";
#endif

#ifdef SERIALBOX_HAS_NETCDF
  // SERIALBOX_HAS_NETCDF
  options += "SERIALBOX_HAS_NETCDF=" + std::to_string(SERIALBOX_HAS_NETCDF) + ";";
#endif

#ifdef SERIALBOX_ASYNC_API
  // SERIALBOX_ASYNC_API
  options += "SERIALBOX_ASYNC_API=" + std::to_string(SERIALBOX_ASYNC_API) + ";";
#endif

  // SERIALBOX_CXX_COMPILER
  options += "SERIALBOX_CXX_COMPILER=" SERIALBOX_CXX_COMPILER_STRING ";";

  // SERIALBOX_CXX_FLAGS
  options += "SERIALBOX_CXX_FLAGS=" SERIALBOX_CXX_FLAGS;

  return serialboxC::allocateAndCopyString(options);
}
