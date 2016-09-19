//===-- Unittest/Cpp/UnittestFieldMetaInfo.cpp --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the FieldMetaInfo.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/FieldMetaInfo.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

using namespace serialbox;

TEST(FieldMetaInfoTest, Construction) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15, 20};
  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

  // -----------------------------------------------------------------------------------------------
  // Default constrcutor
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetaInfo f;
    EXPECT_EQ(f.type(), TypeID::Invalid);
    EXPECT_TRUE(f.dims().empty());
    EXPECT_TRUE(f.metaInfo().empty());

    const FieldMetaInfo const_f;
    EXPECT_EQ(const_f.type(), TypeID::Invalid);
    EXPECT_TRUE(const_f.dims().empty());
    EXPECT_TRUE(const_f.metaInfo().empty());
  }

  // -----------------------------------------------------------------------------------------------
  // Construct members externally
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetaInfo f(type, dims, metaInfo);

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
    FieldMetaInfo f(type, dims, metaInfo);

    FieldMetaInfo f_copy(f);
    EXPECT_EQ(f.type(), f_copy.type());
    EXPECT_TRUE(std::equal(f.dims().begin(), f.dims().end(), f_copy.dims().begin()));
    EXPECT_TRUE(std::equal(f.metaInfo().begin(), f.metaInfo().end(), f_copy.metaInfo().begin()));

    FieldMetaInfo f_move(std::move(f));
    EXPECT_EQ(f_move.type(), f_copy.type());
    EXPECT_TRUE(std::equal(f_move.dims().begin(), f_move.dims().end(), f_copy.dims().begin()));
    EXPECT_TRUE(
        std::equal(f_move.metaInfo().begin(), f_move.metaInfo().end(), f_copy.metaInfo().begin()));
  }

  // -----------------------------------------------------------------------------------------------
  // Copy & Move assign
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetaInfo f(type, dims, metaInfo);

    FieldMetaInfo f_copy;
    f_copy = f;
    EXPECT_EQ(f.type(), f_copy.type());
    EXPECT_TRUE(std::equal(f.dims().begin(), f.dims().end(), f_copy.dims().begin()));
    EXPECT_TRUE(std::equal(f.metaInfo().begin(), f.metaInfo().end(), f_copy.metaInfo().begin()));

    FieldMetaInfo f_move;
    f_move = std::move(f);
    EXPECT_EQ(f_move.type(), f_copy.type());
    EXPECT_TRUE(std::equal(f_move.dims().begin(), f_move.dims().end(), f_copy.dims().begin()));
    EXPECT_TRUE(
        std::equal(f_move.metaInfo().begin(), f_move.metaInfo().end(), f_copy.metaInfo().begin()));
  }

  // -----------------------------------------------------------------------------------------------
  // Comparison operator
  // -----------------------------------------------------------------------------------------------
  {
    FieldMetaInfo f(type, dims, metaInfo);
    FieldMetaInfo f_equal(type, dims, metaInfo);
    FieldMetaInfo f_wrong_type(TypeID::Float32, dims, metaInfo);
    FieldMetaInfo f_wrong_dims1(type, std::vector<int>{1, 2, 3}, metaInfo);
    FieldMetaInfo f_wrong_dims2(type, std::vector<int>{1, 2}, metaInfo);
    FieldMetaInfo f_wrong_metaInfo1(type, dims, metaInfo);
    f_wrong_metaInfo1.metaInfo()["key1"] = MetaInfoValue(std::string("wrong-value"));
    FieldMetaInfo f_wrong_metaInfo2(type, dims, metaInfo);
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

    FieldMetaInfo f(type, dims, metaInfo);
    FieldMetaInfo f_swap(TypeID::Float32, std::vector<int>{1, 2, 3}, metaInfo);
    f_swap.metaInfo()["key1"] = MetaInfoValue(std::string("changed-value"));

    f.swap(f_swap);
    EXPECT_EQ(f.type(), TypeID::Float32);
    EXPECT_TRUE(std::equal(f.dims().begin(), f.dims().end(), dims2.begin()));
    ASSERT_TRUE(f.metaInfo().hasKey("key1"));
    EXPECT_EQ(f.metaInfo().at("key1").as<std::string>(), "changed-value");
  }
}

TEST(FieldMetaInfoTest, toJSON) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15};
  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

  FieldMetaInfo f(type, dims, metaInfo);
  json::json j = f.toJSON();

  // Dims
  ASSERT_TRUE(j.count("dims"));
  EXPECT_EQ(int(j["dims"][0]), dims[0]);
  EXPECT_EQ(int(j["dims"][1]), dims[1]);

  // Type
  ASSERT_TRUE(j.count("type_id"));
  EXPECT_EQ(int(j["type_id"]), static_cast<int>(type));

  // Meta-info (this is properly tested in the MetaInfoMap unittests)
  ASSERT_TRUE(j.count("meta_info"));

  ASSERT_TRUE(j["meta_info"].count("key1"));
  EXPECT_EQ(int(j["meta_info"]["key1"]["type_id"]), static_cast<int>(TypeID::String));
  std::string key1_value = j["meta_info"]["key1"]["value"];
  EXPECT_EQ(key1_value, "str");

  ASSERT_TRUE(j["meta_info"].count("key2"));
  EXPECT_EQ(int(j["meta_info"]["key2"]["type_id"]), static_cast<int>(TypeID::Float64));
  EXPECT_EQ(double(j["meta_info"]["key2"]["value"]), double(5));
}

TEST(FieldMetaInfoTest, fromJSON) {
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

    FieldMetaInfo f(j);

    // Type
    EXPECT_EQ(f.type(), TypeID::Float64);

    // Dims
    std::vector<int> dims{33, 29};
    EXPECT_TRUE(std::equal(f.dims().begin(), f.dims().end(), dims.begin()));

    // MetaInfo
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
    FieldMetaInfo f;
    ASSERT_THROW(f.fromJSON(j);, Exception);
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
    
    FieldMetaInfo f;
    ASSERT_THROW(f.fromJSON(j);, Exception);
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
    
    FieldMetaInfo f;
    ASSERT_THROW(f.fromJSON(j);, Exception);
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
    
    FieldMetaInfo f;
    ASSERT_THROW(f.fromJSON(j);, Exception);
  }
}

TEST(FieldMetaInfoTest, toString) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15, 20};
  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

  std::stringstream ss;

  FieldMetaInfo map(type, dims, metaInfo);

  ss << map;
  EXPECT_NE(ss.str().find("type_id"), std::string::npos);
  EXPECT_NE(ss.str().find("dims"), std::string::npos);
  EXPECT_NE(ss.str().find("meta_info"), std::string::npos);
  EXPECT_NE(ss.str().find("key1"), std::string::npos);
  EXPECT_NE(ss.str().find("key2"), std::string::npos);
}
