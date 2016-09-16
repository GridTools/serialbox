//===-- Unittest/Cpp/UnittestMain.cpp -----------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/Logging.h"
#include "Utility/UnittestEnvironment.h"
#include <gtest/gtest.h>

int main(int argc, char *argv[]) {

  // Initialize gtest
  testing::InitGoogleTest(&argc, argv);

  // Initialize glog
  serialbox::Logging::init(argv[0], true);
  serialbox::Logging::setLogToStderr(true);
  serialbox::Logging::setMinLogLevel(serialbox::Logging::Warning);
  serialbox::Logging::setColorLogToStdErr(true);
  
  // Register test environment
  testing::AddGlobalTestEnvironment(&serialbox::unittest::UnittestEnvironment::getInstance());

  return RUN_ALL_TESTS();
}
