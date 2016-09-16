//===-- Unittest/Cpp/UnittestMetaInfoMap.cpp ----------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This implements the unittests of the MetaInfoMap.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/MetaInfoMap.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(MetaInfoMap, Value) {
  // Boolean
  MetaInfoMap::Value bool_value(bool(true));
  EXPECT_EQ(bool_value.as<bool>(), true);
  bool b_val = bool_value;
  EXPECT_EQ(b_val, true);

  // int
  MetaInfoMap::Value int32_value0(int(32));
  EXPECT_EQ(int32_value0.as<int>(), 32);

  int int32 = 32;
  int& int32_ref = int32;
  MetaInfoMap::Value int32_value1(int32_ref);
  EXPECT_EQ(int32_value1.as<int>(), int32);

  const int const_int32 = 12012091;
  const int& const_int32_ref = const_int32;
  MetaInfoMap::Value int32_value2(const_int32_ref);
  EXPECT_EQ(int32_value2.as<int>(), const_int32);

  // int64
  MetaInfoMap::Value int64_value(std::int64_t(64));
  EXPECT_EQ(int64_value.as<std::int64_t>(), 64);

  // float
  MetaInfoMap::Value float32_value(float(32.f));
  EXPECT_EQ(float32_value.as<float>(), 32.f);

  // double
  MetaInfoMap::Value float64_value(double(64.0));
  EXPECT_EQ(float64_value.as<double>(), 64.0);

  // string
  MetaInfoMap::Value std_string_value0(std::string("str"));
  EXPECT_EQ(std_string_value0.as<std::string>(), "str");
  
  std::string std_string_value_implicit = std_string_value0;
  EXPECT_EQ(std_string_value_implicit, "str");  

  auto getStr = []() -> std::string { return std::string("rts"); };
  std::string&& str_rvalue_ref = getStr();
  MetaInfoMap::Value std_string_value1(str_rvalue_ref);
  EXPECT_EQ(std_string_value1.as<std::string>(), "rts");

  // Comparison
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

TEST(MetaInfoMap, Construction) {
  MetaInfoMap map;

  EXPECT_TRUE(map.empty());
  EXPECT_EQ(map.size(), 0);

  // Insert a value
  ASSERT_TRUE(map.insert("key1", std::string("value1")));
  ASSERT_TRUE(map.hasKey("key1"));

  // Reinsert same value (should do nothing)
  ASSERT_FALSE(map.insert("key1", std::string("value2")));
  EXPECT_FALSE(map.empty());
  EXPECT_EQ(map.size(), 1);
  EXPECT_STRCASEEQ(map["key1"].as<std::string>().c_str(), "value1");

  // Modify value
  map["key1"] = MetaInfoMap::Value(std::string("value2"));
  EXPECT_EQ(map["key1"].as<std::string>(), "value2");
  EXPECT_EQ(map.at("key1").as<std::string>(), "value2");
  EXPECT_THROW(map.at("key2").as<std::string>(), Exception);

  // Clear map
  map.clear();
  EXPECT_TRUE(map.empty());
  EXPECT_EQ(map.size(), 0);

  // Insert more values
  ASSERT_TRUE(map.insert("bool", bool(true)));
  ASSERT_TRUE(map.insert("int32", int(32)));
  ASSERT_TRUE(map.insert("int64", std::int64_t(64)));
  ASSERT_TRUE(map.insert("float32", float(32.0f)));
  ASSERT_TRUE(map.insert("float64", double(64.0f)));
  ASSERT_TRUE(map.insert("string", std::string("string")));
  EXPECT_EQ(map.size(), 6);

  ASSERT_EQ(map["bool"].as<bool>(), true);
  ASSERT_EQ(map["int32"].as<int>(), 32);
  ASSERT_EQ(map["int64"].as<std::int64_t>(), 64);
  ASSERT_EQ(map["float32"].as<float>(), 32.0f);
  ASSERT_EQ(map["float64"].as<double>(), 64.0);
  ASSERT_EQ(map["string"].as<std::string>(), "string");

  // Comparison
  MetaInfoMap map2(map);
  ASSERT_TRUE(map == map2);

  // Swap
  map2.clear();
  map.swap(map2);
  ASSERT_TRUE(map.empty());
  ASSERT_FALSE(map2.empty());
  map.swap(map2);
  ASSERT_FALSE(map.empty());
  ASSERT_TRUE(map2.empty());

  // Iterate values
  std::vector<boost::any> anyvec;
  for(const auto& map_element : map)
    anyvec.push_back(map_element.second.any());
  EXPECT_EQ(anyvec.size(), map.size());

  // Erase value by key
  map.erase("bool");
  ASSERT_FALSE(map.hasKey("bool"));

  // Erase value by iterator
  auto map_element_pair = *map.begin();

  map.erase(map.begin());
  ASSERT_FALSE(map.hasKey(map_element_pair.first));

  // Erase range
  map.erase(map.begin(), map.end());
  EXPECT_TRUE(map.empty());
  EXPECT_EQ(map.size(), 0);
}

TEST(MetaInfoMap, toJSON) {
  MetaInfoMap map;
  EXPECT_TRUE(map.toJSON().empty());

  ASSERT_TRUE(map.insert("bool", bool(true)));
  ASSERT_TRUE(map.insert("int32", int(32)));
  ASSERT_TRUE(map.insert("int64", std::int64_t(64)));
  ASSERT_TRUE(map.insert("float32", float(32.0f)));
  ASSERT_TRUE(map.insert("float64", double(64.0f)));
  ASSERT_TRUE(map.insert("string", std::string("string")));

  json::json j(map.toJSON());

  // bool
  ASSERT_TRUE(j.count("bool"));
  ASSERT_EQ(int(j["bool"]["type_id"]), (int)TypeID::Boolean);
  ASSERT_EQ(bool(j["bool"]["value"]), true);

  // int32
  ASSERT_TRUE(j.count("int32"));
  ASSERT_EQ(int(j["int32"]["type_id"]), (int)TypeID::Int32);
  ASSERT_EQ(int(j["int32"]["value"]), 32);

  // int64
  ASSERT_TRUE(j.count("int64"));
  ASSERT_EQ(int(j["int64"]["type_id"]), (int)TypeID::Int64);
  ASSERT_EQ(std::int64_t(j["int64"]["value"]), 64);

  // float32
  ASSERT_TRUE(j.count("float32"));
  ASSERT_EQ(int(j["float32"]["type_id"]), (int)TypeID::Float32);
  ASSERT_EQ(float(j["float32"]["value"]), 32.0f);

  // float64
  ASSERT_TRUE(j.count("float64"));
  ASSERT_EQ(int(j["float64"]["type_id"]), (int)TypeID::Float64);
  ASSERT_EQ(double(j["float64"]["value"]), 64.0);

  // string
  ASSERT_TRUE(j.count("string"));
  ASSERT_EQ(int(j["string"]["type_id"]), (int)TypeID::String);
  std::string str = j["string"]["value"];
  ASSERT_STREQ(str.c_str(), "string");
}

TEST(MetaInfoMap, fromJSON) {
  // -----------------------------------------------------------------------------------------------
  // Success
  // -----------------------------------------------------------------------------------------------

  // Empty
  {
    MetaInfoMap map;
    json::json j;
    EXPECT_NO_THROW(map.fromJSON(j));
    EXPECT_TRUE(map.empty());
  }

#define CHECK_FROM_JSON(type, type_id, value1, value2)                                             \
  {                                                                                                \
    MetaInfoMap map;                                                                               \
    json::json j;                                                                                  \
    j["key1"] = {{"type_id", (int)type_id}, {"value", type(value1)}};                              \
    j["key2"] = {{"type_id", (int)type_id}, {"value", type(value2)}};                              \
    std::string typeStr(TypeUtil::toString(type_id));                                              \
    ASSERT_NO_THROW(map.fromJSON(j)) << typeStr;                                                   \
    EXPECT_EQ(map.size(), 2) << typeStr;                                                           \
    ASSERT_TRUE(map.hasKey("key1")) << typeStr;                                                    \
    ASSERT_TRUE(map.hasKey("key2")) << typeStr;                                                    \
    ASSERT_EQ(map["key1"].as<type>(), value1) << typeStr;                                          \
    ASSERT_EQ(map["key2"].as<type>(), value2) << typeStr;                                          \
  }

  CHECK_FROM_JSON(bool, TypeID::Boolean, true, false);
  CHECK_FROM_JSON(int, TypeID::Int32, 1, 0);
  CHECK_FROM_JSON(std::int64_t, TypeID::Int64, 1, 0);
  CHECK_FROM_JSON(float, TypeID::Float32, 1.0f, 0.1f);
  CHECK_FROM_JSON(double, TypeID::Float64, 1.0, 0.1);
  CHECK_FROM_JSON(std::string, TypeID::String, "true", "false");

  // -----------------------------------------------------------------------------------------------
  // Failures
  // -----------------------------------------------------------------------------------------------

  // Missing value
  {
    MetaInfoMap map;
    json::json ill_formed;
    ill_formed["key"] = {{"type_id", (int)TypeID::Boolean}};
    EXPECT_THROW(map.fromJSON(ill_formed), Exception);
  }

  // Missing type id
  {
    MetaInfoMap map;
    json::json ill_formed;
    ill_formed["key"] = {{"value", (int)5}};
    EXPECT_THROW(map.fromJSON(ill_formed), Exception);
  }

  // TypeId / value mismatch I
  {
    MetaInfoMap map;
    json::json ill_formed;
    ill_formed["key"] = {{"type_id", (int)TypeID::Boolean}, {"value", double(5)}};
    EXPECT_THROW(map.fromJSON(ill_formed), Exception);
  }

  // TypeId / value mismatch II
  {
    MetaInfoMap map;
    json::json ill_formed;
    ill_formed["key1"] = {{"type_id", (int)TypeID::Boolean}, {"value", true}};
    ill_formed["key2"] = {{"type_id", (int)TypeID::Int32}, {"value", int(3)}};
    ill_formed["key3"] = {{"type_id", (int)TypeID::Boolean}, {"value", double(5)}}; // Failure
    EXPECT_THROW(map.fromJSON(ill_formed), Exception);
  }
}
