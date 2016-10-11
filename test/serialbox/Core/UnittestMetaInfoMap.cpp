//===-- serialbox/Core/UnittestMetaInfoMap.cpp --------------------------------------*- C++ -*-===//
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
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

using namespace serialbox;

TEST(MetaInfoMapTest, Construction) {
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
  map["key1"] = MetaInfoValue(std::string("value2"));
  EXPECT_EQ(map["key1"].as<std::string>(), "value2");
  EXPECT_EQ(map.at("key1").as<std::string>(), "value2");
  EXPECT_THROW(map.at("key2").as<std::string>(), Exception);
  
  // Get vector of keys
  EXPECT_EQ(map.keys(), std::vector<std::string>{"key1"});
  
  // Get vector of types
  EXPECT_EQ(map.types(), std::vector<TypeID>{TypeID::String});  

  const MetaInfoMap constMap;
  EXPECT_THROW(constMap.at("key2").as<std::string>(), Exception);

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

TEST(MetaInfoMapTest, Comparison) {
  MetaInfoMap map1;
  MetaInfoMap map2;

  // Empty maps compare equal
  EXPECT_TRUE(map1 == map2);
  EXPECT_FALSE(map1 != map2);

  // Maps of diffrent sizes are never equal
  ASSERT_TRUE(map1.insert("bool", bool(true)));
  EXPECT_FALSE(map1 == map2);

  // Maps are equal again
  ASSERT_TRUE(map2.insert("bool", bool(true)));
  EXPECT_TRUE(map1 == map2);

  // Maps have equal sizes and keys but one of their values differs
  ASSERT_TRUE(map1.insert("double", double(1)));
  ASSERT_TRUE(map2.insert("double", double(2)));
  EXPECT_TRUE(map1 != map2);
}

TEST(MetaInfoMapTest, Conversion) {
  MetaInfoMap map;
  ASSERT_TRUE(map.insert("key", int(32)));
  EXPECT_EQ(map.size(), 1);

  // Boolean
  EXPECT_EQ(map.as<bool>("key"), true);
  EXPECT_EQ(map.as<int>("key"), 32);
  EXPECT_EQ(map.as<std::int64_t>("key"), 32);
  EXPECT_EQ(map.as<float>("key"), 32.0f);
  EXPECT_EQ(map.as<double>("key"), 32.0);
  EXPECT_EQ(map.as<std::string>("key"), "32");
  ASSERT_THROW(map.as<bool>("XXX"), Exception);
}

TEST(MetaInfoMapTest, toJSON) {
  MetaInfoMap map;
  EXPECT_TRUE(map.toJSON().empty());

  ASSERT_TRUE(map.insert("bool", bool(true)));
  ASSERT_TRUE(map.insert("int32", int(32)));
  ASSERT_TRUE(map.insert("int64", std::int64_t(64)));
  ASSERT_TRUE(map.insert("float32", float(32.0f)));
  ASSERT_TRUE(map.insert("float64", double(64.0f)));
  ASSERT_TRUE(map.insert("string", std::string("string")));

  ASSERT_TRUE(map.insert("ArrayOfBoolean", (Array<bool>{true, false, true})));
  ASSERT_TRUE(map.insert("ArrayOfInt32", (Array<int>{1, 2, 3})));
  ASSERT_TRUE(map.insert("ArrayOfInt64", (Array<std::int64_t>{1, 2, 3})));
  ASSERT_TRUE(map.insert("ArrayOfFloat32", (Array<float>{1.0f, 2.0f, 3.0f})));
  ASSERT_TRUE(map.insert("ArrayOfFloat64", (Array<double>{1.0, 2.0, 3.0})));
  ASSERT_TRUE(map.insert("ArrayOfString", (Array<std::string>{"one", "two", "three"})));

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

  // array of bool
  ASSERT_TRUE(j.count("ArrayOfBoolean"));
  ASSERT_EQ(int(j["ArrayOfBoolean"]["type_id"]), (int)TypeID::ArrayOfBoolean);
  Array<bool> arrayOfBoolean = j["ArrayOfBoolean"]["value"];
  ASSERT_EQ(arrayOfBoolean, (Array<bool>{true, false, true}));

  // array of int32
  ASSERT_TRUE(j.count("ArrayOfInt32"));
  ASSERT_EQ(int(j["ArrayOfInt32"]["type_id"]), (int)TypeID::ArrayOfInt32);
  Array<int> arrayOfInt32 = j["ArrayOfInt32"]["value"];
  ASSERT_EQ(arrayOfInt32, (Array<int>{1, 2, 3}));

  // array of int64
  ASSERT_TRUE(j.count("ArrayOfInt64"));
  ASSERT_EQ(int(j["ArrayOfInt64"]["type_id"]), (int)TypeID::ArrayOfInt64);
  Array<std::int64_t> arrayOfInt64 = j["ArrayOfInt64"]["value"];
  ASSERT_EQ(arrayOfInt64, (Array<std::int64_t>{1, 2, 3}));

  // array of float32
  ASSERT_TRUE(j.count("ArrayOfFloat32"));
  ASSERT_EQ(int(j["ArrayOfFloat32"]["type_id"]), (int)TypeID::ArrayOfFloat32);
  Array<float> arrayOfFloat32 = j["ArrayOfFloat32"]["value"];
  ASSERT_EQ(arrayOfFloat32, (Array<float>{1.0f, 2.0f, 3.0f}));

  // array of float64
  ASSERT_TRUE(j.count("ArrayOfFloat64"));
  ASSERT_EQ(int(j["ArrayOfFloat64"]["type_id"]), (int)TypeID::ArrayOfFloat64);
  Array<double> arrayOfFloat64 = j["ArrayOfFloat64"]["value"];
  ASSERT_EQ(arrayOfFloat64, (Array<double>{1.0, 2.0, 3.0}));

  // array of string
  ASSERT_TRUE(j.count("ArrayOfString"));
  ASSERT_EQ(int(j["ArrayOfString"]["type_id"]), (int)TypeID::ArrayOfString);
  Array<std::string> arrayOfString = j["ArrayOfString"]["value"];
  ASSERT_EQ(arrayOfString, (Array<std::string>{"one", "two", "three"}));
}

TEST(MetaInfoMapTest, fromJSON) {
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
    j["array"] = {{"type_id", (int)type_id | (int)TypeID::Array},                                  \
                  {"value", Array<type>{value1, value2}}};                                         \
    std::string typeStr(TypeUtil::toString(type_id));                                              \
    ASSERT_NO_THROW(map.fromJSON(j)) << typeStr;                                                   \
    EXPECT_EQ(map.size(), 3) << typeStr;                                                           \
    ASSERT_TRUE(map.hasKey("key1")) << typeStr;                                                    \
    ASSERT_TRUE(map.hasKey("key2")) << typeStr;                                                    \
    ASSERT_EQ(map["key1"].as<type>(), value1) << typeStr;                                          \
    ASSERT_EQ(map["key2"].as<type>(), value2) << typeStr;                                          \
    ASSERT_EQ((map["array"].as<Array<type>>()), (Array<type>{value1, value2})) << typeStr;         \
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

TEST(MetaInfoMapTest, toString) {
  std::stringstream ss;

  MetaInfoMap map;
  map.insert("key1", std::string("value2"));
  map.insert("key2", double(5.1));

  ss << map;
  EXPECT_TRUE(boost::algorithm::starts_with(ss.str(), "MetaInfoMap"));
  EXPECT_NE(ss.str().find("key1"), std::string::npos);
  EXPECT_NE(ss.str().find("key2"), std::string::npos);
}
