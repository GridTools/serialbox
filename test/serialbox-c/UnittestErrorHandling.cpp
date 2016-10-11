//===-- serialbox-c/UnittestErrorHandling.cpp ---------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface ErrorHandling.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox-c/ErrorHandling.h"
#include <gtest/gtest.h>

TEST(CErrorHandling, Test) {
#if defined(SERIALBOX_RUN_DEATH_TESTS) && !defined(NDEBUG)
  serialboxResetFatalErrorHandler();
  ASSERT_DEATH_IF_SUPPORTED(serialboxFatalError("blub"), "Serialbox: ERROR: blub");
#endif
}
