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
#include <utility>

using namespace serialbox;

namespace {
template <class T>
class MetaInfoValueTypedTest : public testing::Test {};

// Generate a pair of values v1 and v2 of type T wiht v1 != v2
template <class T>
std::pair<T, T> getValuePair() {
  static_assert(isSupported<T>::value, "type is not supported (cannot be mapped to TypeID)");
  return std::make_pair<T, T>(T(), T());
}

template <>
std::pair<bool, bool> getValuePair<bool>() {
  return std::make_pair<bool, bool>(true, false);
}

template <>
std::pair<int, int> getValuePair<int>() {
  return std::make_pair<int, int>(1, 2);
}

template <>
std::pair<std::int64_t, std::int64_t> getValuePair<std::int64_t>() {
  return std::make_pair<std::int64_t, std::int64_t>(1, 2);
}

template <>
std::pair<float, float> getValuePair<float>() {
  return std::make_pair<float, float>(1.0f, 2.0f);
}

template <>
std::pair<double, double> getValuePair<double>() {
  return std::make_pair<double, double>(1.0, 2.0);
}

template <>
std::pair<std::string, std::string> getValuePair<std::string>() {
  return std::make_pair<std::string, std::string>("str1", "str2");
}

// Convert value to string
template <class T>
std::string toString(const T& v) {
  return std::to_string(v);
}

template <>
std::string toString<std::string>(const std::string& v) {
  return v;
}

using TestTypes = testing::Types<bool, double, float, int, std::int64_t, std::string>;

} // namespace anonymous

TYPED_TEST_CASE(MetaInfoValueTypedTest, TestTypes);

TYPED_TEST(MetaInfoValueTypedTest, Constrcution) {
  // Default construct
  MetaInfoValue m;
  EXPECT_EQ(m.type(), TypeID::Invalid);

  // Construct with value
  auto pair = getValuePair<TypeParam>();
  MetaInfoValue value1(TypeParam(pair.first));
  MetaInfoValue value2(TypeParam(pair.second));
  TypeID typeID = ToTypeID<TypeParam>::value;
  EXPECT_EQ(value1.type(), typeID);

  // Construct with l-value refrence
  const TypeParam& v1_lref = pair.first;
  MetaInfoValue value1_lref(v1_lref);
  EXPECT_EQ(value1_lref.as<TypeParam>(), pair.first);

  // Construct with r-value refrence
  auto getValue1 = [&]() -> TypeParam { return pair.first; };
  TypeParam&& v1_rref = getValue1();
  MetaInfoValue value1_rref(v1_rref);
  EXPECT_EQ(value1_rref.as<TypeParam>(), pair.first);

  // Explicit conversion
  EXPECT_EQ(value1.as<TypeParam>(), pair.first);
  EXPECT_EQ(value2.as<TypeParam>(), pair.second);

  // Implicit conversion
  TypeParam v1 = value1;
  TypeParam v2 = value2;
  EXPECT_TRUE(v1 == pair.first);
  EXPECT_TRUE(v2 == pair.second);

  // Conversion with wrong type
  if(std::is_same<TypeParam, std::string>::value)
    ASSERT_THROW(value1.as<bool>(), Exception);
  else
    ASSERT_THROW(value1.as<std::string>(), Exception);

  // Swap
  value1.swap(value2);
  EXPECT_EQ(value1.as<TypeParam>(), pair.second);
  EXPECT_EQ(value2.as<TypeParam>(), pair.first);
}

TYPED_TEST(MetaInfoValueTypedTest, Comparison) {
  auto pair = getValuePair<TypeParam>();
  MetaInfoValue value1(TypeParam(pair.first));
  MetaInfoValue value2(TypeParam(pair.second));

  // Equality
  EXPECT_TRUE(value1 == value1);
  EXPECT_TRUE(value2 == value2);
  EXPECT_FALSE(value1 == value2);
  EXPECT_FALSE(value2 == value1);

  // Inequality
  EXPECT_TRUE(value1 != value2);
  EXPECT_TRUE(value2 != value1);
  EXPECT_FALSE(value1 != value1);
  EXPECT_FALSE(value2 != value2);

  // Comparison with wrong type
  if(std::is_same<TypeParam, std::string>::value) {
    MetaInfoValue bool_value(bool(true));
    EXPECT_FALSE(value1 == bool_value);
  } else {
    MetaInfoValue string_value(std::string("str"));
    EXPECT_FALSE(value1 == string_value);
  }
}

TYPED_TEST(MetaInfoValueTypedTest, toString) {
  auto pair = getValuePair<TypeParam>();
  MetaInfoValue value1(TypeParam(pair.first));
  MetaInfoValue value2(TypeParam(pair.second));

  EXPECT_STREQ(value1.toString().c_str(), toString(pair.first).c_str());
  EXPECT_STREQ(value2.toString().c_str(), toString(pair.second).c_str());
}
