//===-- serialbox/core/UnittestFieldMetainfoImpl.cpp ------------------------------------*- C++
//-*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the FieldMetainfoImpl.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/FieldMetainfoImpl.h"
#include "serialbox/core/FieldMetainfoImplSerializer.h"
#include <gtest/gtest.h>

using nlohmann::basic_json;

using namespace serialbox;

template <class MetainfoMapImplType1, class MetainfoMapImplType2>
static bool mapEqual(const MetainfoMapImplType1& map1, const MetainfoMapImplType2& map2) {
  if(map1.size() != map2.size())
    return false;

  for(const auto& map_element : map1) {
    const std::string& key = map_element.first;
    if(!map2.hasKey(key))
      return false;

    if(map_element.second != map2.at(key))
      return false;
  }
  return true;
}

TEST(FieldMetainfoImplTest, Construction) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15, 20};
  MetainfoMapImpl metaInfo(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("str"))}, {"key2", MetainfoValueImpl(double(5))}});

  // -----------------------------------------------------------------------------------------------
  // Default constructor
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetainfoImpl f;
    EXPECT_EQ(f.type(), TypeID::Invalid);
    EXPECT_TRUE(f.dims().empty());
    EXPECT_TRUE(f.metaInfo().empty());

    const FieldMetainfoImpl const_f;
    EXPECT_EQ(const_f.type(), TypeID::Invalid);
    EXPECT_TRUE(const_f.dims().empty());
    EXPECT_TRUE(const_f.metaInfo().empty());
  }

  // -----------------------------------------------------------------------------------------------
  // Construct members externally
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetainfoImpl f(type, dims, metaInfo);

    EXPECT_EQ(f.type(), TypeID::Float64);
    EXPECT_EQ(f.dims(), dims);

    EXPECT_EQ(f.metaInfo().size(), 2);

    ASSERT_TRUE(f.metaInfo().hasKey("key1"));
    EXPECT_EQ(f.metaInfo().at("key1").as<std::string>(), "str");

    ASSERT_TRUE(f.metaInfo().hasKey("key2"));
    EXPECT_EQ(f.metaInfo().at("key2").as<double>(), double(5));
  }

  // -----------------------------------------------------------------------------------------------
  // Copy & Move construct
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetainfoImpl f(type, dims, metaInfo);
    FieldMetainfoImpl f_copy(f);

    EXPECT_EQ(f.type(), f_copy.type());
    EXPECT_TRUE(std::equal(f.dims().begin(), f.dims().end(), f_copy.dims().begin()));
    EXPECT_TRUE(mapEqual(f.metaInfo(), f_copy.metaInfo()));

    // Check that we actually performed a deep copy
    f.metaInfo().insert("newKey", "str");
    EXPECT_FALSE(mapEqual(f.metaInfo(), f_copy.metaInfo()));

    FieldMetainfoImpl f_move(std::move(f));
    EXPECT_EQ(f_move.type(), f_copy.type());
    EXPECT_TRUE(std::equal(f_move.dims().begin(), f_move.dims().end(), f_copy.dims().begin()));
  }

  // -----------------------------------------------------------------------------------------------
  // Copy & Move assign
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetainfoImpl f(type, dims, metaInfo);

    FieldMetainfoImpl f_copy;
    f_copy = f;
    EXPECT_EQ(f.type(), f_copy.type());
    EXPECT_TRUE(std::equal(f.dims().begin(), f.dims().end(), f_copy.dims().begin()));
    EXPECT_TRUE(mapEqual(f.metaInfo(), f_copy.metaInfo()));

    FieldMetainfoImpl f_move;
    f_move = std::move(f);
    EXPECT_EQ(f_move.type(), f_copy.type());
    EXPECT_TRUE(std::equal(f_move.dims().begin(), f_move.dims().end(), f_copy.dims().begin()));
    EXPECT_TRUE(mapEqual(f_move.metaInfo(), f_move.metaInfo()));
  }

  // -----------------------------------------------------------------------------------------------
  // Comparison operators
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetainfoImpl f(type, dims, metaInfo);
    FieldMetainfoImpl f_equal(type, dims, metaInfo);
    FieldMetainfoImpl f_wrong_type(TypeID::Float32, dims, metaInfo);
    FieldMetainfoImpl f_wrong_dims1(type, std::vector<int>{1, 2, 3}, metaInfo);
    FieldMetainfoImpl f_wrong_dims2(type, std::vector<int>{1, 2}, metaInfo);
    FieldMetainfoImpl f_wrong_metaInfo1(type, dims, metaInfo);
    f_wrong_metaInfo1.metaInfo()["key1"] = MetainfoValueImpl(std::string("wrong-value"));
    FieldMetainfoImpl f_wrong_metaInfo2(type, dims, metaInfo);
    f_wrong_metaInfo2.metaInfo().erase("key1");

    EXPECT_TRUE(f == f_equal);
    EXPECT_TRUE(f != f_wrong_type);
    EXPECT_TRUE(f != f_wrong_dims1);
    EXPECT_TRUE(f != f_wrong_dims2);
    EXPECT_TRUE(f != f_wrong_metaInfo1);
    EXPECT_TRUE(f != f_wrong_metaInfo2);
  }

  // -----------------------------------------------------------------------------------------------
  // Swap
  // -----------------------------------------------------------------------------------------------
  {
    std::vector<int> dims2{1, 2, 3};

    FieldMetainfoImpl f(type, dims, metaInfo);
    FieldMetainfoImpl f_swap(TypeID::Float32, std::vector<int>{1, 2, 3}, metaInfo);
    f_swap.metaInfo()["key1"] = MetainfoValueImpl(std::string("changed-value"));

    f.swap(f_swap);
    EXPECT_EQ(f.type(), TypeID::Float32);
    EXPECT_TRUE(std::equal(f.dims().begin(), f.dims().end(), dims2.begin()));
    ASSERT_TRUE(f.metaInfo().hasKey("key1"));
    EXPECT_EQ(f.metaInfo().at("key1").as<std::string>(), "changed-value");
  }
}

TEST(FieldMetainfoImplTest, toJSON) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15};
  MetainfoMapImpl metaInfo(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("str"))}, {"key2", MetainfoValueImpl(double(5))}});

  FieldMetainfoImpl f(type, dims, metaInfo);
  json::json j = f;

  // Dims
  ASSERT_TRUE(j.count("dims"));
  EXPECT_EQ(int(j["dims"][0]), dims[0]);
  EXPECT_EQ(int(j["dims"][1]), dims[1]);

  // Type
  ASSERT_TRUE(j.count("type_id"));
  EXPECT_EQ(int(j["type_id"]), static_cast<int>(type));

  // Meta-info (this is properly tested in the MetainfoMapImpl unittests)
  ASSERT_TRUE(j.count("meta_info"));

  ASSERT_TRUE(j["meta_info"].count("key1"));
  EXPECT_EQ(int(j["meta_info"]["key1"]["type_id"]), static_cast<int>(TypeID::String));
  std::string key1_value = j["meta_info"]["key1"]["value"];
  EXPECT_EQ(key1_value, "str");

  ASSERT_TRUE(j["meta_info"].count("key2"));
  EXPECT_EQ(int(j["meta_info"]["key2"]["type_id"]), static_cast<int>(TypeID::Float64));
  EXPECT_EQ(double(j["meta_info"]["key2"]["value"]), double(5));
}

TEST(FieldMetainfoImplTest, fromJSON) {
  // -----------------------------------------------------------------------------------------------
  // Success
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "dims": [
             33,
             29
         ],
         "meta_info": {
             "key1": {
                 "type_id": 6,
                 "value": "a-string"
             },
             "key2": {
                 "type_id": 5,
                 "value": 0.322
             }
         },
         "type_id": 5
     }
    )"_json;

    FieldMetainfoImpl f(j);

    // Type
    EXPECT_EQ(f.type(), TypeID::Float64);

    // Dims
    std::vector<int> dims{33, 29};
    EXPECT_TRUE(std::equal(f.dims().begin(), f.dims().end(), dims.begin()));

    // Metainfo
    ASSERT_TRUE(f.metaInfo().hasKey("key1"));
    EXPECT_EQ(int(f.metaInfo()["key1"].type()), static_cast<int>(TypeID::String));
    EXPECT_EQ(f.metaInfo()["key1"].as<std::string>(), "a-string");

    ASSERT_TRUE(f.metaInfo().hasKey("key2"));
    EXPECT_EQ(int(f.metaInfo()["key2"].type()), static_cast<int>(TypeID::Float64));
    EXPECT_EQ(f.metaInfo()["key2"].as<double>(), double(0.322));
  }

  // -----------------------------------------------------------------------------------------------
  // Failure (empty)
  // -----------------------------------------------------------------------------------------------
  {
    json::json j;
    FieldMetainfoImpl f;
    ASSERT_THROW(f = j;, Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Failure (missing dims)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "meta_info": {
             "key1": {
                 "type_id": 6,
                 "value": "a-string"
             },
             "key2": {
                 "type_id": 5,
                 "value": 0.322
             }
         },
         "type_id": 5
     }
    )"_json;

    FieldMetainfoImpl f;
    ASSERT_THROW(f = j;, Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Failure (missing type)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "dims": [
             33,
             29
         ],
         "meta_info": {
             "key1": {
                 "type_id": 6,
                 "value": "a-string"
             },
             "key2": {
                 "type_id": 5,
                 "value": 0.322
             }
         }
     }
    )"_json;

    FieldMetainfoImpl f;
    ASSERT_THROW(f = j;, Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Failure (missing meta info)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "dims": [
             33,
             29
         ],
         "type_id": 5
     }
    )"_json;

    FieldMetainfoImpl f;

    ASSERT_ANY_THROW(f = j);
  }
}

TEST(FieldMetainfoImplTest, toString) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15, 20};
  MetainfoMapImpl metaInfo(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("str"))}, {"key2", MetainfoValueImpl(double(5))}});

  std::stringstream ss;

  FieldMetainfoImpl map(type, dims, metaInfo);

  ss << map;
  EXPECT_NE(ss.str().find("type"), std::string::npos);
  EXPECT_NE(ss.str().find("dims"), std::string::npos);
  EXPECT_NE(ss.str().find("metainfo"), std::string::npos);
  EXPECT_NE(ss.str().find("key1"), std::string::npos);
  EXPECT_NE(ss.str().find("key2"), std::string::npos);
}
