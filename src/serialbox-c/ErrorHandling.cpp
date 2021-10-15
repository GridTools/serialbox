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
#include "serialbox/core/Compiler.h"
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>

#include <iostream>

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

/*===------------------------------------------------------------------------------------------===*\
 *     Error Handler Callbacks
\*===------------------------------------------------------------------------------------------===*/

void serialboxDefaultFatalErrorHandler(const char* reason) {
  std::fprintf(stderr, "Serialbox: ERROR: %s\n", reason);
  std::fflush(stderr);
  serialboxStateErrorHandler(reason);
}

namespace internal {

struct ErrorState {
  bool hasError = false;
  std::string errMsg = "";
};

static ErrorState errorState;

} // namespace internal

void serialboxStateErrorHandler(const char* reason) {
  internal::errorState.hasError = true;
  internal::errorState.errMsg = reason;
}

int serialboxStateErrorHandlerHasError(void) { return internal::errorState.hasError; }

char* serialboxStateErrorHandlerGetErrorMessage(void) {
  std::size_t size = internal::errorState.errMsg.size() + 1;
  char* errorMessage = (char*)std::malloc(size * sizeof(char));
  std::memcpy(errorMessage, internal::errorState.errMsg.c_str(), size);
  return errorMessage;
}

void serialboxStateErrorHandlerResetState(void) {
  internal::errorState.hasError = false;
  internal::errorState.errMsg.clear();
}
