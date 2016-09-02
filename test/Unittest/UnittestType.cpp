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

TEST(TypeTest, sizeOf) {
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Boolean), sizeof(bool));
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Int32), sizeof(int));
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Int64), sizeof(std::int64_t));
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Float32), sizeof(float));
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Float64), sizeof(double));
}

TEST(TypeTest, toString) {
  EXPECT_STREQ(TypeUtil::toString(TypeID::Boolean).c_str(), "bool");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Int32).c_str(), "int");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Int64).c_str(), "int64");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Float32).c_str(), "float");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Float64).c_str(), "double");
}

TEST(TypeTest, toTypeID) {
  static_assert(toTypeID<bool>::value == TypeID::Boolean, "Boolean");
  static_assert(toTypeID<std::int32_t>::value == TypeID::Int32, "Int32");
  static_assert(toTypeID<std::int64_t>::value == TypeID::Int64, "Int64");
  static_assert(toTypeID<float>::value == TypeID::Float32, "Float32");
  static_assert(toTypeID<double>::value == TypeID::Float64, "Float64");
}

TEST(TypeTest, toType) {
  testing::StaticAssertTypeEq<toType<TypeID::Boolean>::type, bool>();
  testing::StaticAssertTypeEq<toType<TypeID::Int32>::type, std::int32_t>();
  testing::StaticAssertTypeEq<toType<TypeID::Int64>::type, std::int64_t>();
  testing::StaticAssertTypeEq<toType<TypeID::Float32>::type, float>();
  testing::StaticAssertTypeEq<toType<TypeID::Float64>::type, double>();
}
