//===-- Unittest/Cpp/UnittestSTELLAException.cpp ------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the SerializationException of the STELLA Frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Frontend/STELLA/SerializationException.h"
#include "serialbox/Core/Frontend/STELLA/Utility.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(STELLAExceptionTest, Throw) {
  auto throwAnExcpetion = [](bool doIt) -> void {
    if(doIt) {
      ser::SerializationException exception;
      exception.Init("you asked for it!");
      throw exception;
    }
  };

  EXPECT_THROW(throwAnExcpetion(true), stella::SerializationException);
  EXPECT_NO_THROW(throwAnExcpetion(false));

  try {
    ser::internal::throwSerializationException("the %s should be %i", "answer", 42);
  } catch(ser::SerializationException& e) {
    EXPECT_STREQ(e.what(), "the answer should be 42");
    EXPECT_EQ(e.message(), "the answer should be 42");
  }
}
