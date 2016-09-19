//===-- Unittest/Cpp/UnittestSavepointImpl.cpp --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the shared savepoint implementation.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/SavepointImpl.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

using namespace serialbox;

TEST(SavepointImplTest, Construction) {
  std::string name("TestSavepoint");
  std::vector<FieldID> fields{{"field1", 0}, {"field2", 5}};
  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

  //------------------------------------------------------------------------------------------------
  // Empty constructor
  //------------------------------------------------------------------------------------------------
  {
    SavepointImpl s(name);
    EXPECT_EQ(s.name(), name);
    EXPECT_TRUE(s.empty());

    const SavepointImpl const_s(name);
    EXPECT_EQ(const_s.name(), name);
    EXPECT_TRUE(const_s.empty());
  }

  // -----------------------------------------------------------------------------------------------
  // Construct members externally
  //-----------------------------------------------------------------------------------------------
  {
    SavepointImpl s(name, metaInfo, fields);

    EXPECT_EQ(s.name(), name);
    EXPECT_EQ(s.fields(), fields);

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
  }

  //----------------------------------------------------------------------------------------------
  //  Move construct
  //----------------------------------------------------------------------------------------------
  {
    SavepointImpl s_to_move(name, metaInfo, fields);
    SavepointImpl s(std::move(s_to_move));

    EXPECT_EQ(s.name(), name);
    EXPECT_EQ(s.fields(), fields);

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
  }

  //----------------------------------------------------------------------------------------------
  //  Move assign
  //----------------------------------------------------------------------------------------------
  {
    SavepointImpl s_to_move(name, metaInfo, fields);
    SavepointImpl s(name);
    s = std::move(s_to_move);

    EXPECT_EQ(s.name(), name);
    EXPECT_EQ(s.fields(), fields);

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
  }

  //----------------------------------------------------------------------------------------------
  // Comparison operators
  //----------------------------------------------------------------------------------------------
  {
    SavepointImpl s(name, metaInfo, fields);
    SavepointImpl s_equal(name, metaInfo, fields);
    SavepointImpl s_wrong_name("wrong_name", metaInfo, fields);
    SavepointImpl s_wrong_field1(name, metaInfo, std::vector<FieldID>{{"fieldXXX", 0}});
    SavepointImpl s_wrong_field2(name, metaInfo, fields);
    s_wrong_field2.fields()[0] = FieldID{"field1", 1};
    SavepointImpl s_wrong_metaInfo1(name, metaInfo, fields);
    s_wrong_metaInfo1.metaInfo()["key1"] = MetaInfoValue(std::string("wrong-value"));
    SavepointImpl s_wrong_metaInfo2(name, metaInfo, fields);
    s_wrong_metaInfo2.metaInfo().erase("key1");

    EXPECT_TRUE(s == s_equal);
    EXPECT_TRUE(s != s_wrong_name);
    EXPECT_TRUE(s != s_wrong_field1);
    EXPECT_TRUE(s != s_wrong_field2);
    EXPECT_TRUE(s != s_wrong_metaInfo1);
    EXPECT_TRUE(s != s_wrong_metaInfo2);
  }

  //----------------------------------------------------------------------------------------------
  // Swap
  //----------------------------------------------------------------------------------------------
  {
    std::vector<FieldID> fields2{{"fieldXXX", 0}};

    SavepointImpl s(name, metaInfo, fields);
    SavepointImpl s_swap("SavepointTest-swap", metaInfo, fields2);
    s_swap.metaInfo()["key1"] = MetaInfoValue(std::string("changed-value"));

    s.swap(s_swap);
    EXPECT_EQ(s.name(), "SavepointTest-swap");
    EXPECT_TRUE((s.fields()[0] == FieldID{"fieldXXX", 0}));
    EXPECT_EQ(s.fields().size(), 1);
    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "changed-value");
  }
}

TEST(SavepointImplTest, QueryFields) {
  SavepointImpl s("TestSavepoint");

  // Register new field
  ASSERT_NO_THROW(s.registerField(FieldID{"field1", 0}));
  EXPECT_EQ(s.numFields(), 1);
  ASSERT_NO_THROW(s.registerField(FieldID{"field2", 5}));
  EXPECT_EQ(s.numFields(), 2);

  // Register already existing field -> Exception
  ASSERT_THROW(s.registerField(FieldID{"field1", 0}), Exception);
  EXPECT_EQ(s.numFields(), 2);
  ASSERT_THROW(s.registerField(FieldID{"field1", 1}), Exception);
  EXPECT_EQ(s.numFields(), 2);

  // Check if field exists
  EXPECT_TRUE(s.hasField("field1"));
  EXPECT_TRUE(s.hasField("field2"));
  EXPECT_FALSE(s.hasField("fieldXXX"));

  // Get fieldID
  EXPECT_EQ(s.getFieldID("field1"), (FieldID{"field1", 0}));
  EXPECT_EQ(s.getFieldID("field2"), (FieldID{"field2", 5}));
  ASSERT_THROW(s.getFieldID("fieldXX"), Exception);
}

TEST(SavepointImplTest, toJSON) {
  std::string name("TestSavepoint");
  std::vector<FieldID> fields{{"field1", 0}, {"field2", 5}};
  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

  SavepointImpl s(name, metaInfo, fields);
  json::json j = s.toJSON();

  // Type
  ASSERT_TRUE(j.count("name"));
  EXPECT_EQ(name, j["name"]);

  // Fields
  ASSERT_TRUE(j.count("fields"));
  EXPECT_EQ(int(j["fields"]["field1"]), 0);
  EXPECT_EQ(int(j["fields"]["field2"]), 5);

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

TEST(SavepointImplTest, fromJSON) {
  std::string name("TestSavepoint");
  std::vector<FieldID> fields{{"field1", 0}, {"field2", 5}};
  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

  // -----------------------------------------------------------------------------------------------
  // Success
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "fields": {
             "field1": 0,
             "field2": 5
         },
         "meta_info": {
             "key1": {
                 "type_id": 6,
                 "value": "str"
             },
             "key2": {
                 "type_id": 5,
                 "value": 5
             }
         },
         "name": "TestSavepoint"
     }
    )"_json;

    SavepointImpl s(j);
    EXPECT_EQ(s.name(), name);
    EXPECT_EQ(s.fields(), fields);

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
  }

  // -----------------------------------------------------------------------------------------------
  // Success (empty meta-info)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "fields": {
             "field1": 0,
             "field2": 5
         },
         "meta_info": null,
         "name": "TestSavepoint"
     }
    )"_json;

    SavepointImpl s(j);
    EXPECT_EQ(s.name(), name);
    EXPECT_EQ(s.fields(), fields);
    EXPECT_TRUE(s.metaInfo().empty());
  }
  
  // -----------------------------------------------------------------------------------------------
  // Failure (missing name)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
      {
          "fields": {
              "field1": 0,
              "field2": 5
          },
          "meta_info": {
              "key1": {
                  "type_id": 6,
                  "value": "str"
              },
              "key2": {
                  "type_id": 5,
                  "value": 5
              }
          }
      }
    )"_json;

    ASSERT_THROW((SavepointImpl(j)), Exception);
  }
  
  // -----------------------------------------------------------------------------------------------
  // Failure (missing meta_info)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "fields": {
             "field1": 0,
             "field2": 5
         },
         "name": "TestSavepoint"
     }
    )"_json;

    ASSERT_THROW((SavepointImpl(j)), Exception);
  }
  
  // -----------------------------------------------------------------------------------------------
  // Failure (missing fields)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {

         "meta_info": {
             "key1": {
                 "type_id": 6,
                 "value": "str"
             },
             "key2": {
                 "type_id": 5,
                 "value": 5
             }
         },
         "name": "TestSavepoint"
     }
    )"_json;

    ASSERT_THROW((SavepointImpl(j)), Exception);
  }
}

TEST(SavepointImplTest, toString) {
  std::string name("TestSavepoint");
  std::vector<FieldID> fields{{"field1", 0}, {"field2", 5}};
  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

  std::stringstream ss;
  SavepointImpl savepoint(name, metaInfo, fields);

  ss << savepoint;
  EXPECT_NE(ss.str().find("name"), std::string::npos);
  EXPECT_NE(ss.str().find("meta_info"), std::string::npos);
  EXPECT_NE(ss.str().find("fields"), std::string::npos);
}
