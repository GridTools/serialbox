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

void serialboxDefaultFatalErrorHandler(const char* Reason) {
  std::fprintf(stderr, "Serialbox: ERROR: %s\n", Reason);
  std::fflush(stderr);
  std::exit(1);
}

namespace internal {

struct ErrorState {
  bool hasError = false;
  std::string errMsg = "";
};

static ErrorState errorState;

} // namespace internal

void serialboxStateErrorHandler(const char* Reason) {
  internal::errorState.hasError = true;
  internal::errorState.errMsg = Reason;
}

int serialboxStateErrorHandlerHasError(void) {
  return internal::errorState.hasError;
}

char* serialboxStateErrorHandlerGetErrorMessage(void) {
  std::size_t size = internal::errorState.errMsg.size() + 1;
  char* errorMessage = (char*)std::malloc(size * sizeof(char));
  std::memcpy(errorMessage, internal::errorState.errMsg.c_str(), size);
  return errorMessage;
}

//void serialboxStateErrorHandlerGetCurrentError(int* hasError, char** errorMessage) {
//  (*hasError) = internal::errorState.hasError;
//  (*errorMessage) = NULL;

//  if(*hasError) {
//    std::size_t size = internal::errorState.errMsg.size() + 1;
//    (*errorMessage) = (char*)std::malloc(size * sizeof(char));

//    if(*errorMessage)
//      std::memcpy(*errorMessage, internal::errorState.errMsg.c_str(), size);
//  }
//}

void serialboxStateErrorHandlerResetState(void) {
  internal::errorState.hasError = false;
  internal::errorState.errMsg.clear();
}

//void serialboxStateErrorHandlerGetCurrentErrorAndReset(int* hasError, char** errorMessage) {
//  serialboxStateErrorHandlerGetCurrentError(hasError, errorMessage);
//  serialboxStateErrorHandlerResetState();
//}
