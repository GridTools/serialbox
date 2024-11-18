//===-- serialbox/core/UnittestMetainfoMapImpl.cpp --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This implements the unittests of the gridtools::meta_info_map.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/frontend/gridtools/MetainfoMap.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace gridtools;

TEST(GridToolsMetainfoMapTest, Construction) {
  meta_info_map map1;

  EXPECT_TRUE(map1.empty());
  EXPECT_EQ(map1.size(), 0);

  // Insert a value
  ASSERT_TRUE(map1.insert("key1", std::string("value1")));
  ASSERT_TRUE(map1.has_key("key1"));

  // Reinsert same value (should do nothing)
  ASSERT_FALSE(map1.insert("key1", std::string("value2")));
  EXPECT_FALSE(map1.empty());
  EXPECT_EQ(map1.size(), 1);
  EXPECT_STRCASEEQ(map1["key1"].as<std::string>().c_str(), "value1");

  // Modify value
  map1["key1"] = MetainfoValueImpl(std::string("value2"));
  EXPECT_EQ(map1["key1"].as<std::string>(), "value2");
  EXPECT_EQ(map1.at("key1").as<std::string>(), "value2");
  EXPECT_THROW(map1.at("key2").as<std::string>(), exception);
  
  // Get vector of keys
  EXPECT_EQ(map1.keys(), std::vector<std::string>{"key1"});
  
  // Get vector of types
  EXPECT_EQ(map1.types(), std::vector<TypeID>{TypeID::String});  

  // Clear map
  map1.clear();
  EXPECT_TRUE(map1.empty());
  EXPECT_EQ(map1.size(), 0);

  // Insert a value again
  ASSERT_TRUE(map1.insert("bool", bool(true)));

  // Shallow copy
  meta_info_map map2(map1);
  
  // Insert value (will also insert in map1)
  map2.insert("key", "str");
  ASSERT_TRUE(map1 == map2);

  // Assign initializer list 
  map2 = {{"key1", meta_info_value(double(4))}, {"key2", meta_info_value(int(2))}};
  ASSERT_FALSE(map1 == map2);  
  
  // Deep copy
  meta_info_map map3(map1.clone());
  map1.clear();
  ASSERT_FALSE(map3.empty());
  ASSERT_FALSE(map3 == map1);

  // Swap (map1 and map3 -- map1 is empty)
  map1.swap(map3);
  ASSERT_TRUE(map3.empty());
  ASSERT_FALSE(map1.empty());
  map1.swap(map3);
  ASSERT_FALSE(map3.empty());
  ASSERT_TRUE(map1.empty());

  // Iterate values
  std::vector<std::any> anyvec;
  for(const auto& map_element : map1)
    anyvec.push_back(map_element.second.any());
  EXPECT_EQ(anyvec.size(), map1.size());
}

TEST(GridToolsMetainfoMapTest, Comparison) {
  meta_info_map map1;
  meta_info_map map2;

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
