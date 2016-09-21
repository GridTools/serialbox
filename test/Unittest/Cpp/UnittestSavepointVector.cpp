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
/// This file implements the unittests of the savepoint vector.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/SavepointVector.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

using namespace serialbox;

TEST(SavepointVectorTest, Construction) {
  // s1 and s2 have same name but diffrent meta-info, s3 has a diffrent name and no meta-info
  Savepoint savepoint1("savepoint");
  Savepoint savepoint2("savepoint");
  Savepoint savepoint3("diffrent-savepoint");

  ASSERT_NO_THROW(savepoint1.addMetaInfo("key1", "s1"));
  ASSERT_NO_THROW(savepoint2.addMetaInfo("key1", "s2"));

  //------------------------------------------------------------------------------------------------
  //  Empty constructor
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s;
    EXPECT_TRUE(s.empty());

    const SavepointVector const_s;
    EXPECT_TRUE(const_s.empty());
  }

  //------------------------------------------------------------------------------------------------
  //  Register savepoints
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s;    
    
    // Insert savepoint into empty vector
    ASSERT_TRUE(s.insert(savepoint1));
    EXPECT_TRUE(s.exists(savepoint1));
    EXPECT_EQ(s.size(), 1);
    
    // Insert savepoint with already existing name but diffrent meta-data
    ASSERT_TRUE(s.insert(savepoint2));
    EXPECT_TRUE(s.exists(savepoint2));    
    EXPECT_EQ(s.size(), 2);
    
    // Insert savepoint with already existing name and meta-data -> Fail
    ASSERT_FALSE(s.insert(savepoint1));
    EXPECT_TRUE(s.exists(savepoint1));    
    EXPECT_EQ(s.size(), 2);
    
    // Insert savepoint with diffrent name
    ASSERT_TRUE(s.insert(savepoint3));
    EXPECT_TRUE(s.exists(savepoint3));    
    EXPECT_EQ(s.size(), 3);
  }
  
  //------------------------------------------------------------------------------------------------
  //  Register fields
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s;    
    ASSERT_TRUE(s.insert(savepoint1));
    ASSERT_TRUE(s.insert(savepoint2));
    
    // Add field u (id = 0) to savepoint1
    ASSERT_TRUE(s.addField(savepoint1, FieldID{"u", 0}));
    ASSERT_EQ(s.fieldsOf(savepoint1).size(), 1);
    EXPECT_EQ(s.fieldsOf(savepoint1)[0], (FieldID{"u", 0})); 
    
    // Query fieldIDs
    EXPECT_EQ(s.getFieldID(savepoint1, "u"), (FieldID{"u", 0}));
    EXPECT_THROW(s.getFieldID(savepoint3, "XXX"), Exception); // Savepoint does not exists        
    
    // Add already existing field to savepoint1
    ASSERT_FALSE(s.addField(savepoint1, FieldID{"u", 0}));
    ASSERT_EQ(s.fieldsOf(savepoint1).size(), 1);    
    
    // Add already existing field with diffrent id to savepoint1
    ASSERT_FALSE(s.addField(savepoint1, FieldID{"u", 1}));
    ASSERT_EQ(s.fieldsOf(savepoint1).size(), 1); 
    
    // Add field via iterator
    SavepointVector::iterator it = s.find(savepoint2);
    ASSERT_NE(it, s.end());
    ASSERT_TRUE(s.addField(it, FieldID{"v", 1}));   
    EXPECT_EQ(s.getFieldID(it, "v"), (FieldID{"v", 1}));    
    
    ASSERT_FALSE(s.addField(it, FieldID{"v", 1}));
    ASSERT_FALSE(s.addField(it, FieldID{"v", 0}));
    ASSERT_EQ(s.fieldsOf(savepoint2).size(), 1);
    
    // Query nonexistent field of savepoint2
    EXPECT_THROW(s.getFieldID(it, "XXX"), Exception);    
  }
  
  //------------------------------------------------------------------------------------------------
  //  Copy construct
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s1;
    ASSERT_TRUE(s1.insert(savepoint1));
    ASSERT_TRUE(s1.insert(savepoint2));
    
    SavepointVector s2(s1);
    EXPECT_TRUE(s2.exists(savepoint1));    
    EXPECT_TRUE(s2.exists(savepoint2));    
    EXPECT_TRUE(s1.exists(savepoint1));    
    EXPECT_TRUE(s1.exists(savepoint2));  
  }
  
  //------------------------------------------------------------------------------------------------
  //  Move construct
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s1;
    ASSERT_TRUE(s1.insert(savepoint1));
    ASSERT_TRUE(s1.insert(savepoint2));
    
    SavepointVector s2(std::move(s1));
    EXPECT_TRUE(s2.exists(savepoint1));    
    EXPECT_TRUE(s2.exists(savepoint2));    
  }
  
  //------------------------------------------------------------------------------------------------
  //  Copy assign
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s1;
    ASSERT_TRUE(s1.insert(savepoint1));
    ASSERT_TRUE(s1.insert(savepoint2));
    
    SavepointVector s2;
    s2 = s1;
    EXPECT_TRUE(s2.exists(savepoint1));    
    EXPECT_TRUE(s2.exists(savepoint2));    
    EXPECT_TRUE(s1.exists(savepoint1));    
    EXPECT_TRUE(s1.exists(savepoint2));  
  }
  
  //------------------------------------------------------------------------------------------------
  //  Move assign
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s1;
    ASSERT_TRUE(s1.insert(savepoint1));
    ASSERT_TRUE(s1.insert(savepoint2));
    
    SavepointVector s2;
    s2 = std::move(s1);
    EXPECT_TRUE(s2.exists(savepoint1));    
    EXPECT_TRUE(s2.exists(savepoint2));    
  }

  //----------------------------------------------------------------------------------------------
  //  Swap
  //----------------------------------------------------------------------------------------------
  {
    SavepointVector s1;
    ASSERT_TRUE(s1.insert(savepoint1));
    ASSERT_TRUE(s1.insert(savepoint2));
    
    SavepointVector s2;
    ASSERT_TRUE(s2.insert(savepoint3));
    
    s1.swap(s2);
    
    EXPECT_TRUE(s1.exists(savepoint3));    
    EXPECT_TRUE(s2.exists(savepoint1)); 
    EXPECT_TRUE(s2.exists(savepoint2)); 
  }
}

//TEST(SavepointImplTest, QueryFields) {
//  SavepointImpl s("TestSavepoint");

//  // Register new field
//  ASSERT_NO_THROW(s.registerField(FieldID{"field1", 0}));
//  EXPECT_EQ(s.numFields(), 1);
//  ASSERT_NO_THROW(s.registerField(FieldID{"field2", 5}));
//  EXPECT_EQ(s.numFields(), 2);

//  // Register already existing field -> Exception
//  ASSERT_THROW(s.registerField(FieldID{"field1", 0}), Exception);
//  EXPECT_EQ(s.numFields(), 2);
//  ASSERT_THROW(s.registerField(FieldID{"field1", 1}), Exception);
//  EXPECT_EQ(s.numFields(), 2);

//  // Check if field exists
//  EXPECT_TRUE(s.hasField("field1"));
//  EXPECT_TRUE(s.hasField("field2"));
//  EXPECT_FALSE(s.hasField("fieldXXX"));

//  // Get fieldID
//  EXPECT_EQ(s.getFieldID("field1"), (FieldID{"field1", 0}));
//  EXPECT_EQ(s.getFieldID("field2"), (FieldID{"field2", 5}));
//  ASSERT_THROW(s.getFieldID("fieldXX"), Exception);
//}

//TEST(SavepointImplTest, toJSON) {
//  std::string name("TestSavepoint");
//  std::vector<FieldID> fields{{"field1", 0}, {"field2", 5}};
//  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
//      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

//  SavepointImpl s(name, metaInfo, fields);
//  json::json j = s.toJSON();

//  // Type
//  ASSERT_TRUE(j.count("name"));
//  EXPECT_EQ(name, j["name"]);

//  // Fields
//  ASSERT_TRUE(j.count("fields"));
//  EXPECT_EQ(int(j["fields"]["field1"]), 0);
//  EXPECT_EQ(int(j["fields"]["field2"]), 5);

//  // Meta-info (this is properly tested in the MetaInfoMap unittests)
//  ASSERT_TRUE(j.count("meta_info"));

//  ASSERT_TRUE(j["meta_info"].count("key1"));
//  EXPECT_EQ(int(j["meta_info"]["key1"]["type_id"]), static_cast<int>(TypeID::String));
//  std::string key1_value = j["meta_info"]["key1"]["value"];
//  EXPECT_EQ(key1_value, "str");

//  ASSERT_TRUE(j["meta_info"].count("key2"));
//  EXPECT_EQ(int(j["meta_info"]["key2"]["type_id"]), static_cast<int>(TypeID::Float64));
//  EXPECT_EQ(double(j["meta_info"]["key2"]["value"]), double(5));
//}

//TEST(SavepointImplTest, fromJSON) {
//  std::string name("TestSavepoint");
//  std::vector<FieldID> fields{{"field1", 0}, {"field2", 5}};
//  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
//      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

//  // -----------------------------------------------------------------------------------------------
//  // Success
//  // -----------------------------------------------------------------------------------------------
//  {
//    auto j = R"(
//     {
//         "fields": {
//             "field1": 0,
//             "field2": 5
//         },
//         "meta_info": {
//             "key1": {
//                 "type_id": 6,
//                 "value": "str"
//             },
//             "key2": {
//                 "type_id": 5,
//                 "value": 5
//             }
//         },
//         "name": "TestSavepoint"
//     }
//    )"_json;

//    SavepointImpl s(j);
//    EXPECT_EQ(s.name(), name);
//    EXPECT_EQ(s.fields(), fields);

//    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
//    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");

//    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
//    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
//  }

//  // -----------------------------------------------------------------------------------------------
//  // Success (empty meta-info)
//  // -----------------------------------------------------------------------------------------------
//  {
//    auto j = R"(
//     {
//         "fields": {
//             "field1": 0,
//             "field2": 5
//         },
//         "meta_info": null,
//         "name": "TestSavepoint"
//     }
//    )"_json;

//    SavepointImpl s(j);
//    EXPECT_EQ(s.name(), name);
//    EXPECT_EQ(s.fields(), fields);
//    EXPECT_TRUE(s.metaInfo().empty());
//  }
  
//  // -----------------------------------------------------------------------------------------------
//  // Failure (missing name)
//  // -----------------------------------------------------------------------------------------------
//  {
//    auto j = R"(
//      {
//          "fields": {
//              "field1": 0,
//              "field2": 5
//          },
//          "meta_info": {
//              "key1": {
//                  "type_id": 6,
//                  "value": "str"
//              },
//              "key2": {
//                  "type_id": 5,
//                  "value": 5
//              }
//          }
//      }
//    )"_json;

//    ASSERT_THROW((SavepointImpl(j)), Exception);
//  }
  
//  // -----------------------------------------------------------------------------------------------
//  // Failure (missing meta_info)
//  // -----------------------------------------------------------------------------------------------
//  {
//    auto j = R"(
//     {
//         "fields": {
//             "field1": 0,
//             "field2": 5
//         },
//         "name": "TestSavepoint"
//     }
//    )"_json;

//    ASSERT_THROW((SavepointImpl(j)), Exception);
//  }
  
//  // -----------------------------------------------------------------------------------------------
//  // Failure (missing fields)
//  // -----------------------------------------------------------------------------------------------
//  {
//    auto j = R"(
//     {

//         "meta_info": {
//             "key1": {
//                 "type_id": 6,
//                 "value": "str"
//             },
//             "key2": {
//                 "type_id": 5,
//                 "value": 5
//             }
//         },
//         "name": "TestSavepoint"
//     }
//    )"_json;

//    ASSERT_THROW((SavepointImpl(j)), Exception);
//  }
//}

//TEST(SavepointImplTest, toString) {
//  std::string name("TestSavepoint");
//  std::vector<FieldID> fields{{"field1", 0}, {"field2", 5}};
//  MetaInfoMap metaInfo(std::initializer_list<MetaInfoMap::value_type>{
//      {"key1", MetaInfoValue(std::string("str"))}, {"key2", MetaInfoValue(double(5))}});

//  std::stringstream ss;
//  SavepointImpl savepoint(name, metaInfo, fields);

//  ss << savepoint;
//  EXPECT_NE(ss.str().find("name"), std::string::npos);
//  EXPECT_NE(ss.str().find("meta_info"), std::string::npos);
//  EXPECT_NE(ss.str().find("fields"), std::string::npos);
//}
