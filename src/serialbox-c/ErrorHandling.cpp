/*===-- serialbox-c/ErrorHandling.cpp -----------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the error handling of the C Interface of Serialbox.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/ErrorHandling.h"
#include "serialbox/Core/Compiler.h"
#include <cassert>
#include <cstdio>
#include <cstdlib>

SERIALBOX_ATTRIBUTE_NORETURN static void serialboxDefaultFatalErrorHandler(const char* Reason) {
  std::fprintf(stderr, "Serialbox: ERROR: %s\n", Reason);
  std::fflush(stderr);
  std::exit(1);
}

static serialboxFatalErrorHandler_t FatalErrorHandler = serialboxDefaultFatalErrorHandler;

void serialboxInstallFatalErrorHandler(serialboxFatalErrorHandler_t handler) {
  assert(handler);
  FatalErrorHandler = handler;
}

void serialboxResetFatalErrorHandler(void) {
  FatalErrorHandler = serialboxDefaultFatalErrorHandler;
}

void serialboxFatalError(const char* reason) {
  assert(FatalErrorHandler);
  (*FatalErrorHandler)(reason);
}
