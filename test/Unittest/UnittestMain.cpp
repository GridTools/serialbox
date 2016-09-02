//===-- Unittest/UnittestMain.cpp ---------------------------------------------------*- C++ -*-===//
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

#include "serialbox/Support/Logging.h"
#include <gtest/gtest.h>

int main(int argc, char *argv[]) {

  // Initialize gtest
  testing::InitGoogleTest(&argc, argv);

  // Initialize glog
  serialbox::Logging::init(argv[0], true);
  serialbox::Logging::setLogToStderr(true);
  serialbox::Logging::setMinLogLevel(serialbox::Logging::Info);
  serialbox::Logging::setColorLogToStdErr(true);

  return RUN_ALL_TESTS();
}
