//===-- serialbox/core/UnittestMain.cpp ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file sets-up GTest and parses command-line arguments.
///
//===------------------------------------------------------------------------------------------===//

#include "utility/UnittestEnvironment.h"
#include "serialbox/core/Logging.h"
#include <gtest/gtest.h>

using namespace serialbox;

int main(int argc, char* argv[]) {

  // Initialize gtest
  testing::InitGoogleTest(&argc, argv);

  // Initialize Logger
  Logging::disable();

  // Register test environment
  testing::AddGlobalTestEnvironment(&unittest::UnittestEnvironment::getInstance());

  return RUN_ALL_TESTS();
}
