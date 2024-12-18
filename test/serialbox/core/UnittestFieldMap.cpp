//===-- serialbox/core/UnittestFieldMap.cpp -----------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the FieldMap.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/FieldMap.h"
#include "serialbox/core/FieldMapSerializer.h"
#include <gtest/gtest.h>

using namespace serialbox;

static FieldMetainfoImpl constructFieldMetainfoImpl(double value_key2 = 5.0) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15, 20};
  MetainfoMapImpl metaInfo(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("str"))}, {"key2", MetainfoValueImpl(value_key2)}});
  return FieldMetainfoImpl(type, dims, metaInfo);
}

TEST(FieldMapTest, Construction) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15, 20};
  MetainfoMapImpl metaInfo;

  FieldMap map;
  const FieldMap& const_map = map;

  EXPECT_TRUE(map.empty());
  EXPECT_EQ(map.size(), 0);

  // Insert a field
  ASSERT_TRUE(map.insert("field1", constructFieldMetainfoImpl()));
  ASSERT_TRUE(map.hasField("field1"));
  ASSERT_TRUE(const_map.hasField("field1"));

  // Perfect forwarding
  ASSERT_TRUE(map.insert("field_forwarded_args", type, dims, metaInfo));

  // Copy construct (deep copy)
  FieldMap map_copy(map);
  ASSERT_TRUE(map_copy == map);
  map_copy.insert("new_field", type, dims, metaInfo);
  ASSERT_FALSE(map_copy == map);

  // Query dimensions
  EXPECT_EQ(map.findField("field1")->second->dims(), dims);
  EXPECT_EQ(map.getDimsOf("field1"), dims);
  EXPECT_THROW(map.getDimsOf("X"), Exception);

  EXPECT_EQ(const_map.findField("field1")->second->dims(), dims);
  EXPECT_EQ(const_map.getDimsOf("field1"), dims);
  EXPECT_THROW(const_map.getDimsOf("X"), Exception);

  // Query type
  EXPECT_EQ(map.findField("field1")->second->type(), type);
  EXPECT_EQ(map.getTypeOf("field1"), type);
  EXPECT_THROW(map.getTypeOf("X"), Exception);

  EXPECT_EQ(const_map.findField("field1")->second->type(), type);
  EXPECT_EQ(const_map.getTypeOf("field1"), type);
  EXPECT_THROW(map.getTypeOf("X"), Exception);

  // Query meta information of the field
  EXPECT_EQ(map.findField("field1")->second->metaInfo().at("key1").as<std::string>(), "str");

  EXPECT_EQ(map.getFieldMetainfoImplOf("field1").metaInfo().at("key1").as<std::string>(), "str");
  EXPECT_THROW(map.getFieldMetainfoImplOf("X").metaInfo().at("key1").as<std::string>(), Exception);

  EXPECT_EQ(map.getMetainfoOf("field1").at("key1").as<std::string>(), "str");
  EXPECT_THROW(map.getMetainfoOf("X").at("key1").as<std::string>(), Exception);

  EXPECT_EQ(const_map.findField("field1")->second->metaInfo().at("key1").as<std::string>(), "str");

  EXPECT_EQ(const_map.getFieldMetainfoImplOf("field1").metaInfo().at("key1").as<std::string>(),
            "str");
  EXPECT_THROW(const_map.getFieldMetainfoImplOf("X").metaInfo().at("key1").as<std::string>(),
               Exception);

  EXPECT_EQ(const_map.getMetainfoOf("field1").at("key1").as<std::string>(), "str");
  EXPECT_THROW(const_map.getMetainfoOf("X").at("key1").as<std::string>(), Exception);

  // Reinsert same field but with diffrent meta information (should do nothing)
  FieldMetainfoImpl f1(constructFieldMetainfoImpl());
  f1.metaInfo()["key1"].as<std::string>() = "strXXX";

  ASSERT_FALSE(map.insert("field1", f1));
  EXPECT_FALSE(map.empty());
  EXPECT_EQ(map.size(), 2);
  EXPECT_EQ(map.findField("field1")->second->metaInfo().at("key1").as<std::string>(), "str");

  // Comparison
  //
  // map :  { field1 with (key2 = 5.0), field2 }
  // map2 : { field1 with (key2 = 2.0)
  //
  FieldMap map2;
  ASSERT_TRUE(map2.insert("field1", constructFieldMetainfoImpl(2.0)));
  ASSERT_TRUE(map.insert("field2", constructFieldMetainfoImpl(2.0)));

  EXPECT_TRUE(map == const_map);
  EXPECT_FALSE(map == map2);
  EXPECT_TRUE(map != map2);

  // Swap
  map2.clear();
  map.swap(map2);
  ASSERT_TRUE(map.empty());
  ASSERT_FALSE(map2.empty());
  map.swap(map2);
  ASSERT_FALSE(map.empty());
  ASSERT_TRUE(map2.empty());

  // Iterate values
  std::vector<FieldMetainfoImpl> fieldMetaVec;
  for(const auto& map_element : map)
    fieldMetaVec.push_back(*map_element.second);
  EXPECT_EQ(fieldMetaVec.size(), map.size());

  // Clear map
  map.clear();
  EXPECT_TRUE(map.empty());
  EXPECT_EQ(map.size(), 0);
}

TEST(FieldMapTest, toJSON) {
  FieldMap map;
  ASSERT_TRUE(map.insert("field1", constructFieldMetainfoImpl(1.0)));
  ASSERT_TRUE(map.insert("field2", constructFieldMetainfoImpl(2.0)));

  json::json j = map;

  // The correct serialization of the FieldMetainfoImpl is tested elsewhere
  ASSERT_TRUE(j.count("field1"));
  ASSERT_TRUE(j.count("field2"));
}

TEST(FieldMapTest, fromJSON) {
  // -----------------------------------------------------------------------------------------------
  // Success
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
    {
        "field1": {
            "dims": [
                32,
                16
            ],
            "meta_info": {
                "key1": {
                    "type_id": 6,
                    "value": "field1_meta_info_str"
                },
                "key2": {
                    "type_id": 5,
                    "value": 1
                }
            },
            "type_id": 4
        },
        "field2": {
            "dims": [
                20,
                55,
                1992
            ],
            "meta_info": {
                "key1": {
                    "type_id": 6,
                    "value": "field2_meta_info_str"
                },
                "key2": {
                    "type_id": 5,
                    "value": 2
                }
            },
            "type_id": 5
        }
    }
    )"_json;

    FieldMap map = j;

    // type
    EXPECT_EQ(map.getTypeOf("field1"), TypeID::Float32);
    EXPECT_EQ(map.getTypeOf("field2"), TypeID::Float64);

    // dims
    EXPECT_EQ(map.getDimsOf("field1"), (std::vector<int>{32, 16}));
    EXPECT_EQ(map.getDimsOf("field2"), (std::vector<int>{20, 55, 1992}));

    // meta-info
    EXPECT_EQ(map.getMetainfoOf("field1").at("key1").as<std::string>(), "field1_meta_info_str");
    EXPECT_EQ(map.getMetainfoOf("field2").at("key1").as<std::string>(), "field2_meta_info_str");

    EXPECT_EQ(map.getMetainfoOf("field1").at("key2").as<double>(), 1.0);
    EXPECT_EQ(map.getMetainfoOf("field2").at("key2").as<double>(), 2.0);
  }

  // -----------------------------------------------------------------------------------------------
  // Success (empty)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"({})"_json;
    FieldMap map = j;
    EXPECT_TRUE(map.empty());
  }

  // -----------------------------------------------------------------------------------------------
  // Failure (corrupted meta_info of field1; key1 is missing the type-id)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
    {
        "field1": {
            "dims": [
                32,
                16
            ],
            "meta_info": {
                "key1": {
                    "value": "field1_meta_info_str"
                },
                "key2": {
                    "type_id": 5,
                    "value": 1
                }
            },
            "type_id": 4
        }
    }
    )"_json;
    FieldMap map;
    ASSERT_THROW(map = j, Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Failure (multiple field1)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
    {
        "field1": {
            "dims": [
                32,
                16
            ],
            "meta_info": {
                "key1": {
                    "value": "field1_meta_info_str"
                },
                "key2": {
                    "type_id": 5,
                    "value": 1
                }
            },
            "type_id": 4
        },
        "field1": {
             "dims": [
                 32,
                 16
             ],
             "meta_info": {
                 "key1": {
                     "value": "field1_meta_info_str"
                 },
                 "key2": {
                     "type_id": 5,
                     "value": 1
                 }
             },
             "type_id": 4
         }
    }
    )"_json;
    FieldMap map;
    ASSERT_THROW(map = j, Exception);
  }
}

TEST(FieldMapTest, toString) {
  FieldMap map;
  ASSERT_TRUE(map.insert("field1", constructFieldMetainfoImpl(1.0)));

  std::stringstream ss;
  ss << map;
  EXPECT_TRUE(ss.str().rfind("FieldMap", 0) == 0); // replace by starts_with in C++20
  EXPECT_NE(ss.str().find("field1"), std::string::npos);
  EXPECT_NE(ss.str().find("key1"), std::string::npos);
  EXPECT_NE(ss.str().find("key2"), std::string::npos);
}
