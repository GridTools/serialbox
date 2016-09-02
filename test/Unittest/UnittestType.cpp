//===-- Unittest/UnittestType.cpp ---------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the StorageView.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Support/Type.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(TypeTest, toTypeID) {
  static_assert(toTypeID<bool>::value == TypeID::Boolean, "Boolean");
  static_assert(toTypeID<std::int32_t>::value == TypeID::Int32, "Int32");
  static_assert(toTypeID<std::int64_t>::value == TypeID::Int64, "Int64");
  static_assert(toTypeID<float>::value == TypeID::Float32, "Float32");
  static_assert(toTypeID<double>::value == TypeID::Float64, "Float64");
  static_assert(toTypeID<std::string>::value == TypeID::String, "String");
}

TEST(TypeTest, toType) {
  testing::StaticAssertTypeEq<toType<TypeID::Boolean>::type, bool>();
  testing::StaticAssertTypeEq<toType<TypeID::Int32>::type, std::int32_t>();
  testing::StaticAssertTypeEq<toType<TypeID::Int64>::type, std::int64_t>();
  testing::StaticAssertTypeEq<toType<TypeID::Float32>::type, float>();
  testing::StaticAssertTypeEq<toType<TypeID::Float64>::type, double>();
  testing::StaticAssertTypeEq<toType<TypeID::String>::type, std::string>();
}

TEST(TypeTest, toString) {
  EXPECT_STREQ(to_string(TypeID::Boolean).c_str(), "bool");
  EXPECT_STREQ(to_string(TypeID::Int32).c_str(), "int");
  EXPECT_STREQ(to_string(TypeID::Int64).c_str(), "int64");
  EXPECT_STREQ(to_string(TypeID::Float32).c_str(), "float");
  EXPECT_STREQ(to_string(TypeID::Float64).c_str(), "double");
  EXPECT_STREQ(to_string(TypeID::String).c_str(), "string");
}
