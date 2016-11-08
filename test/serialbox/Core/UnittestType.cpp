//===-- serialbox/core/UnittestType.cpp ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the type unittests.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/Exception.h"
#include "serialbox/core/Type.h"
#include <gtest/gtest.h>
#include <sstream>

using namespace serialbox;

TEST(TypeTest, sizeOf) {
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Boolean), sizeof(bool));
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Int32), sizeof(int));
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Int64), sizeof(std::int64_t));
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Float32), sizeof(float));
  EXPECT_EQ(TypeUtil::sizeOf(TypeID::Float64), sizeof(double));

  EXPECT_THROW(TypeUtil::sizeOf(TypeID::String), Exception);
  EXPECT_THROW(TypeUtil::sizeOf(TypeID::ArrayOfBoolean), Exception);
}

TEST(TypeTest, toString) {
  EXPECT_STREQ(TypeUtil::toString(TypeID::Boolean).c_str(), "bool");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Int32).c_str(), "int");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Int64).c_str(), "std::int64_t");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Float32).c_str(), "float");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Float64).c_str(), "double");
  EXPECT_STREQ(TypeUtil::toString(TypeID::Invalid).c_str(), "invalid-type");

  EXPECT_STREQ(TypeUtil::toString(TypeID::ArrayOfBoolean).c_str(), "std::vector<bool>");
  EXPECT_STREQ(TypeUtil::toString(TypeID::ArrayOfInt32).c_str(), "std::vector<int>");
  EXPECT_STREQ(TypeUtil::toString(TypeID::ArrayOfInt64).c_str(), "std::vector<std::int64_t>");
  EXPECT_STREQ(TypeUtil::toString(TypeID::ArrayOfFloat32).c_str(), "std::vector<float>");
  EXPECT_STREQ(TypeUtil::toString(TypeID::ArrayOfFloat64).c_str(), "std::vector<double>");

  std::stringstream ss;
  ss << TypeID::Boolean;
  EXPECT_STREQ(ss.str().c_str(), "bool");
}

TEST(TypeTest, IsArray) {
  EXPECT_TRUE(TypeUtil::isArray(TypeID::ArrayOfBoolean));
  EXPECT_TRUE(TypeUtil::isArray(TypeID::ArrayOfString));
  EXPECT_FALSE(TypeUtil::isArray(TypeID::Float64));
  EXPECT_FALSE(TypeUtil::isArray(TypeID::Boolean));
}

TEST(TypeTest, isPrimitive) {
  EXPECT_FALSE(TypeUtil::isPrimitive(TypeID::ArrayOfBoolean));
  EXPECT_FALSE(TypeUtil::isPrimitive(TypeID::ArrayOfString));
  EXPECT_TRUE(TypeUtil::isPrimitive(TypeID::Float64));
  EXPECT_TRUE(TypeUtil::isPrimitive(TypeID::Boolean));
}

TEST(TypeTest, getArray) {
  EXPECT_EQ(TypeUtil::getArray(TypeID::ArrayOfBoolean), TypeID::ArrayOfBoolean);
  EXPECT_EQ(TypeUtil::getArray(TypeID::ArrayOfString), TypeID::ArrayOfString);
  EXPECT_EQ(TypeUtil::getArray(TypeID::Float64), TypeID::ArrayOfFloat64);
  EXPECT_EQ(TypeUtil::getArray(TypeID::Boolean), TypeID::ArrayOfBoolean);
}

TEST(TypeTest, getPrimitive) {
  EXPECT_EQ(TypeUtil::getPrimitive(TypeID::ArrayOfBoolean), TypeID::Boolean);
  EXPECT_EQ(TypeUtil::getPrimitive(TypeID::ArrayOfString), TypeID::String);
  EXPECT_EQ(TypeUtil::getPrimitive(TypeID::Float64), TypeID::Float64);
  EXPECT_EQ(TypeUtil::getPrimitive(TypeID::Boolean), TypeID::Boolean);
}

TEST(TypeTest, ToTypeID) {
  static_assert(ToTypeID<bool>::value == TypeID::Boolean, "Boolean");
  static_assert(ToTypeID<std::int32_t>::value == TypeID::Int32, "Int32");
  static_assert(ToTypeID<std::int64_t>::value == TypeID::Int64, "Int64");
  static_assert(ToTypeID<float>::value == TypeID::Float32, "Float32");
  static_assert(ToTypeID<double>::value == TypeID::Float64, "Float64");
  static_assert(ToTypeID<std::string>::value == TypeID::String, "String");

  static_assert(ToTypeID<Array<bool>>::value == TypeID::ArrayOfBoolean, "ArrayOfBoolean");
  static_assert(ToTypeID<Array<std::int32_t>>::value == TypeID::ArrayOfInt32, "ArrayOfInt32");
  static_assert(ToTypeID<Array<std::int64_t>>::value == TypeID::ArrayOfInt64, "ArrayOfInt64");
  static_assert(ToTypeID<Array<float>>::value == TypeID::ArrayOfFloat32, "ArrayOfFloat32");
  static_assert(ToTypeID<Array<double>>::value == TypeID::ArrayOfFloat64, "ArrayOfFloat64");
  static_assert(ToTypeID<Array<std::string>>::value == TypeID::ArrayOfString, "ArrayOfString");
}

TEST(TypeTest, ToType) {
  testing::StaticAssertTypeEq<ToType<TypeID::Boolean>::type, bool>();
  testing::StaticAssertTypeEq<ToType<TypeID::Int32>::type, std::int32_t>();
  testing::StaticAssertTypeEq<ToType<TypeID::Int64>::type, std::int64_t>();
  testing::StaticAssertTypeEq<ToType<TypeID::Float32>::type, float>();
  testing::StaticAssertTypeEq<ToType<TypeID::Float64>::type, double>();
  testing::StaticAssertTypeEq<ToType<TypeID::String>::type, std::string>();

  testing::StaticAssertTypeEq<ToType<TypeID::ArrayOfBoolean>::type, Array<bool>>();
  testing::StaticAssertTypeEq<ToType<TypeID::ArrayOfInt32>::type, Array<std::int32_t>>();
  testing::StaticAssertTypeEq<ToType<TypeID::ArrayOfInt64>::type, Array<std::int64_t>>();
  testing::StaticAssertTypeEq<ToType<TypeID::ArrayOfFloat32>::type, Array<float>>();
  testing::StaticAssertTypeEq<ToType<TypeID::ArrayOfFloat64>::type, Array<double>>();
  testing::StaticAssertTypeEq<ToType<TypeID::ArrayOfString>::type, Array<std::string>>();
}

TEST(TypeTest, MatchCVQualifier) {
  testing::StaticAssertTypeEq<match_cv_qualifier<const int, float>::type, const float>();
  testing::StaticAssertTypeEq<match_cv_qualifier<int, float>::type, float>();
}
