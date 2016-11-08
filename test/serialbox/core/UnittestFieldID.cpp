//===-- serialbox/core/UnittestFieldID.cpp ------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the FieldID.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/FieldID.h"
#include <gtest/gtest.h>
#include <sstream>

using namespace serialbox;

TEST(FieldIDTest, Comparison) {
  FieldID f{"field1", 0};
  FieldID f_equal{"field1", 0};
  FieldID f_wrong_id{"field1", 1};
  FieldID f_wrong_field{"field2", 0};
  
  EXPECT_TRUE(f == f_equal);
  EXPECT_TRUE(f != f_wrong_id);
  EXPECT_TRUE(f != f_wrong_field);
}

TEST(FieldIDTest, toString) {
  FieldID f{"field1", 0};
  std::stringstream ss;
  ss << f;
  EXPECT_STREQ(ss.str().c_str(), "{field1, 0}");
}
