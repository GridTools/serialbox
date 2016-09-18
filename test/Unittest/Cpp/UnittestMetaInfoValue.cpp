//===-- Unittest/Cpp/UnittestMetaInfoValue.cpp --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the MetaInfoValue.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/MetaInfoValue.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(MetaInfoValue, Construction) {
  // Default construct
  MetaInfoValue m;
  EXPECT_EQ(m.type(), TypeID::Invalid);

  // Boolean
  MetaInfoValue bool_value(bool(true));
  EXPECT_EQ(bool_value.as<bool>(), true);
  bool b_val = bool_value;
  EXPECT_EQ(b_val, true);

  // int
  MetaInfoValue int32_value0(int(32));
  EXPECT_EQ(int32_value0.as<int>(), 32);

  int int32 = 32;
  int& int32_ref = int32;
  MetaInfoValue int32_value1(int32_ref);
  EXPECT_EQ(int32_value1.as<int>(), int32);

  const int const_int32 = 12012091;
  const int& const_int32_ref = const_int32;
  MetaInfoValue int32_value2(const_int32_ref);
  EXPECT_EQ(int32_value2.as<int>(), const_int32);

  // int64
  MetaInfoValue int64_value(std::int64_t(64));
  EXPECT_EQ(int64_value.as<std::int64_t>(), 64);

  // float
  MetaInfoValue float32_value(float(32.f));
  EXPECT_EQ(float32_value.as<float>(), 32.f);

  // double
  MetaInfoValue float64_value(double(64.0));
  EXPECT_EQ(float64_value.as<double>(), 64.0);

  // string
  MetaInfoValue std_string_value0(std::string("str"));
  EXPECT_EQ(std_string_value0.as<std::string>(), "str");
  
  std::string std_string_value_implicit = std_string_value0;
  EXPECT_EQ(std_string_value_implicit, "str");  

  auto getStr = []() -> std::string { return std::string("rts"); };
  std::string&& str_rvalue_ref = getStr();
  MetaInfoValue std_string_value1(str_rvalue_ref);
  EXPECT_EQ(std_string_value1.as<std::string>(), "rts");

  // Comparison
  // int32_value0 == 32 and int32_value1 == 12012091
  EXPECT_TRUE(int32_value0 == int32_value0);
  EXPECT_FALSE(int32_value0 != int32_value0);
  EXPECT_TRUE(int32_value0 == int32_value1);
  EXPECT_FALSE(int32_value0 == int32_value2);
  EXPECT_FALSE(int32_value0 == float32_value);

  // Swap
  int32_value0.swap(int32_value2);
  EXPECT_EQ(int32_value0.as<int>(), 12012091);
  EXPECT_EQ(int32_value2.as<int>(), 32);

  // Failures
  EXPECT_THROW(std_string_value0.as<int>(), Exception);
  EXPECT_THROW(float32_value.as<int>(), Exception);
}

