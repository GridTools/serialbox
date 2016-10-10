//===-- Utility/CInterfaceTestBase.cpp ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file provides a template to setup C Interface tests and easily check for errors.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/CInterfaceTestBase.h"
#include "Utility/Config.h"
#include <cstdlib>
#include <cstring>
#include <iostream>

#ifdef SERIALBOX_HAS_C
#include "serialbox-c/ErrorHandling.h"

static bool HasError = false;
static char* ErrorMessage = nullptr;

// Copy reason into ErrorMessage and set HasError to true
static void CInterfaceTestBaseFatalErrorHandler(const char* reason) {
  std::string str(reason);

  if(ErrorMessage)
    delete ErrorMessage;

  try {
    ErrorMessage = new char[str.size() + 1];
    std::memcpy(ErrorMessage, str.c_str(), str.size() + 1);
  } catch(std::exception& e) {
    std::cerr << "FATAL ERROR: out of memory!" << std::endl;
    std::abort();
  }

  HasError = true;
}

namespace serialbox {

namespace unittest {

void CInterfaceTestBase::SetUp() {
  serialboxInstallFatalErrorHandler(CInterfaceTestBaseFatalErrorHandler);
}

void CInterfaceTestBase::TearDown() { serialboxResetFatalErrorHandler(); }

bool CInterfaceTestBase::hasError() const noexcept { return HasError; }

std::string CInterfaceTestBase::getLastErrorMsg() const {
  std::string errMsg("Exception caught: \"");
  errMsg += ErrorMessage ? ErrorMessage : "<unknown>";
  errMsg += "\"";
  HasError = false;
  ErrorMessage = nullptr;
  return errMsg;
}

} // namespace unittest

} // namespace serialbox

#endif
