//===-- serialbox/core/UnittestException.cpp ----------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the Exception class.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/Exception.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(ExceptionTest, Throw) {
  auto throwAnExcpetion = [](bool doIt) -> void {
    if(doIt)
      throw Exception("you asked for it!");
  };

  EXPECT_THROW(throwAnExcpetion(true), Exception);
  EXPECT_NO_THROW(throwAnExcpetion(false));

  try {
    throw Exception("the %s should be %i", "answer", 42);
  } catch(std::exception& e) {
    EXPECT_STREQ(e.what(), "the answer should be 42");
  }
}
