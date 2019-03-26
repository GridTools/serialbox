//===-- serialbox/core/UnittestSavepointImpl.cpp ------------------------------------*- C++ -*-===//
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

#include "serialbox/core/SavepointVector.h"
#include "serialbox/core/SavepointVectorSerializer.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

using namespace serialbox;

TEST(SavepointVectorTest, Construction) {
  // s1 and s2 have same name but different meta-info, s3 has a different name and no meta-info
  SavepointImpl savepoint1("savepoint");
  SavepointImpl savepoint2("savepoint");
  SavepointImpl savepoint3("different-savepoint");

  ASSERT_NO_THROW(savepoint1.addMetainfo("key1", "s1"));
  ASSERT_NO_THROW(savepoint2.addMetainfo("key1", "s2"));

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
    ASSERT_EQ(s.insert(savepoint1), 0);
    ASSERT_EQ(s[0], savepoint1);
    EXPECT_TRUE(s.exists(savepoint1));
    EXPECT_EQ(s.size(), 1);

    // Insert savepoint with already existing name but different meta-data
    ASSERT_EQ(s.insert(savepoint2), 1);
    EXPECT_TRUE(s.exists(savepoint2));
    EXPECT_EQ(s.size(), 2);

    // Insert savepoint with already existing name and meta-data -> Fail
    ASSERT_EQ(s.insert(savepoint1), -1);
    EXPECT_TRUE(s.exists(savepoint1));
    EXPECT_EQ(s.size(), 2);

    // Insert savepoint with different name
    ASSERT_EQ(s.insert(savepoint3), 2);
    EXPECT_TRUE(s.exists(savepoint3));
    EXPECT_EQ(s.size(), 3);

    // Clear it
    s.clear();
    EXPECT_EQ(s.size(), 0);
    EXPECT_TRUE(s.empty());
  }

  //------------------------------------------------------------------------------------------------
  //  Register fields
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s;
    ASSERT_EQ(s.insert(savepoint1), 0);
    ASSERT_EQ(s.insert(savepoint2), 1);

    // Add field u (id = 0) to savepoint1
    ASSERT_TRUE(s.addField(savepoint1, FieldID{"u", 0}));
    ASSERT_TRUE(s.hasField(savepoint1, "u"));
    ASSERT_TRUE(s.hasField(0, "u"));
    ASSERT_EQ(s.fieldsOf(savepoint1).size(), 1);
    EXPECT_EQ(s.fieldsOf(savepoint1).find("u")->second, 0);

    // Query fieldIDs
    EXPECT_EQ(s.getFieldID(savepoint1, "u"), (FieldID{"u", 0}));

    // Add already existing field to savepoint1 -> Fail
    ASSERT_FALSE(s.addField(savepoint1, FieldID{"u", 0}));
    ASSERT_EQ(s.fieldsOf(savepoint1).size(), 1);

    // Add already existing field with different id to savepoint1 -> Fail
    ASSERT_FALSE(s.addField(savepoint1, FieldID{"u", 1}));
    ASSERT_EQ(s.fieldsOf(savepoint1).size(), 1);

    // Add fields to non-existing savepoint -> Exception
    ASSERT_FALSE(s.addField(savepoint3, FieldID{"u", 1}));
    ASSERT_THROW(s.fieldsOf(savepoint3), Exception);
    EXPECT_THROW(s.getFieldID(savepoint3, "u"), Exception);

    // Add field via index
    int idx = s.find(savepoint2);
    ASSERT_NE(idx, -1);
    ASSERT_TRUE(s.addField(idx, FieldID{"v", 1}));
    ASSERT_TRUE(s.hasField(idx, "v"));
    EXPECT_EQ(s.getFieldID(idx, "v"), (FieldID{"v", 1}));

    ASSERT_FALSE(s.addField(idx, FieldID{"v", 1}));
    ASSERT_FALSE(s.addField(idx, FieldID{"v", 0}));
    ASSERT_EQ(s.fieldsOf(savepoint2).size(), 1);

    // Look for non-existing savepoint
    int idxToEnd = s.find(savepoint3);
    EXPECT_EQ(idxToEnd, -1);

    // Query nonexistent field of savepoint2
    EXPECT_THROW(s.getFieldID(idx, "XXX"), Exception);
  }

  //------------------------------------------------------------------------------------------------
  //  Copy construct
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s1;
    ASSERT_EQ(s1.insert(savepoint1), 0);
    ASSERT_EQ(s1.insert(savepoint2), 1);

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
    ASSERT_EQ(s1.insert(savepoint1), 0);
    ASSERT_EQ(s1.insert(savepoint2), 1);

    SavepointVector s2(std::move(s1));
    EXPECT_TRUE(s2.exists(savepoint1));
    EXPECT_TRUE(s2.exists(savepoint2));
  }

  //------------------------------------------------------------------------------------------------
  //  Copy assign
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s1;
    ASSERT_EQ(s1.insert(savepoint1), 0);
    ASSERT_EQ(s1.insert(savepoint2), 1);

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
    ASSERT_EQ(s1.insert(savepoint1), 0);
    ASSERT_EQ(s1.insert(savepoint2), 1);

    SavepointVector s2;
    s2 = std::move(s1);
    EXPECT_TRUE(s2.exists(savepoint1));
    EXPECT_TRUE(s2.exists(savepoint2));
  }

  //------------------------------------------------------------------------------------------------
  //  Swap
  //------------------------------------------------------------------------------------------------
  {
    SavepointVector s1;
    ASSERT_EQ(s1.insert(savepoint1), 0);
    ASSERT_EQ(s1.insert(savepoint2), 1);

    SavepointVector s2;
    ASSERT_EQ(s2.insert(savepoint3), 0);

    s1.swap(s2);

    EXPECT_TRUE(s1.exists(savepoint3));
    EXPECT_TRUE(s2.exists(savepoint1));
    EXPECT_TRUE(s2.exists(savepoint2));
  }
}

TEST(SavepointVectorTest, toJSON) {
  // s1 and s2 have same name but different meta-info, s3 has a different name and no meta-info
  SavepointImpl savepoint1("savepoint");
  SavepointImpl savepoint2("savepoint");
  SavepointImpl savepoint3("different-savepoint");

  ASSERT_NO_THROW(savepoint1.addMetainfo("key1", "s1"));
  ASSERT_NO_THROW(savepoint2.addMetainfo("key1", "s2"));

  SavepointVector s;
  ASSERT_EQ(s.insert(savepoint1), 0);
  ASSERT_EQ(s.insert(savepoint2), 1);
  ASSERT_EQ(s.insert(savepoint3), 2);

  ASSERT_TRUE(s.addField(savepoint1, FieldID{"u", 0}));
  ASSERT_TRUE(s.addField(savepoint1, FieldID{"v", 0}));
  ASSERT_TRUE(s.addField(savepoint2, FieldID{"u", 1}));

  json::json j = s;

  // Savepoints
  ASSERT_TRUE(j.count("savepoints"));
  EXPECT_EQ(j["savepoints"][0]["name"], "savepoint");
  EXPECT_EQ(j["savepoints"][0]["meta_info"]["key1"]["value"], "s1");

  EXPECT_EQ(j["savepoints"][1]["name"], "savepoint");
  EXPECT_EQ(j["savepoints"][1]["meta_info"]["key1"]["value"], "s2");

  EXPECT_EQ(j["savepoints"][2]["name"], "different-savepoint");
  EXPECT_TRUE(j["savepoints"][2]["meta_info"].is_null());

  // Fields
  ASSERT_TRUE(j.count("fields_per_savepoint"));
  EXPECT_EQ(int(j["fields_per_savepoint"][0]["savepoint"]["u"]), 0);
  EXPECT_EQ(int(j["fields_per_savepoint"][0]["savepoint"]["v"]), 0);

  EXPECT_EQ(int(j["fields_per_savepoint"][1]["savepoint"]["u"]), 1);

  EXPECT_TRUE(j["fields_per_savepoint"][2]["different-savepoint"].is_null());
}

TEST(SavepointVectorTest, fromJSON) {
  SavepointImpl savepoint1("savepoint");
  SavepointImpl savepoint2("savepoint");
  SavepointImpl savepoint3("different-savepoint");

  ASSERT_NO_THROW(savepoint1.addMetainfo("key1", "s1"));
  ASSERT_NO_THROW(savepoint2.addMetainfo("key1", "s2"));

  // -----------------------------------------------------------------------------------------------
  // Success
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "fields_per_savepoint": [
             {
                 "savepoint": {
                     "u": 0,
                     "v": 0
                 }
             },
             {
                 "savepoint": {
                     "u": 1
                 }
             },
             {
                 "different-savepoint": null
             }
         ],
         "savepoints": [
             {
                 "meta_info": {
                     "key1": {
                         "type_id": 6,
                         "value": "s1"
                     }
                 },
                 "name": "savepoint"
             },
             {
                 "meta_info": {
                     "key1": {
                         "type_id": 6,
                         "value": "s2"
                     }
                 },
                 "name": "savepoint"
             },
             {
                 "meta_info": null,
                 "name": "different-savepoint"
             }
         ]
     }
    )"_json;

    SavepointVector s(j);
    EXPECT_EQ(s.savepoints().size(), 3);

    // Check order of savepoints is correct
    EXPECT_EQ(*s.savepoints()[0], savepoint1);
    EXPECT_EQ(*s.savepoints()[1], savepoint2);
    EXPECT_EQ(*s.savepoints()[2], savepoint3);

    // Check fields
    EXPECT_EQ(s.fieldsOf(savepoint1).size(), 2);
    EXPECT_EQ(s.fieldsOf(savepoint2).size(), 1);
    EXPECT_EQ(s.fieldsOf(savepoint3).size(), 0);

    EXPECT_EQ(s.getFieldID(savepoint1, "u"), (FieldID{"u", 0}));
    EXPECT_EQ(s.getFieldID(savepoint1, "v"), (FieldID{"v", 0}));
    EXPECT_EQ(s.getFieldID(savepoint2, "u"), (FieldID{"u", 1}));
  }

  // -----------------------------------------------------------------------------------------------
  // Success (empty)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"({})"_json;

    SavepointVector s(j);
    EXPECT_TRUE(s.empty());
  }

  // -----------------------------------------------------------------------------------------------
  // Failure (entry in "fields_per_savepoint" for "different-savepoint" is missing)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "fields_per_savepoint": [
             {
                 "savepoint": {
                     "u": 0,
                     "v": 0
                 }
             },
             {
                 "savepoint": {
                     "u": 1
                 }
             }
         ],
         "savepoints": [
             {
                 "meta_info": {
                     "key1": {
                         "type_id": 6,
                         "value": "s1"
                     }
                 },
                 "name": "savepoint"
             },
             {
                 "meta_info": {
                     "key1": {
                         "type_id": 6,
                         "value": "s2"
                     }
                 },
                 "name": "savepoint"
             },
             {
                 "meta_info": null,
                 "name": "different-savepoint"
             }
         ]
     }
    )"_json;

    SavepointVector s;
    ASSERT_THROW(s = j, Exception);
  }
}

TEST(SavepointVectorTest, toString) {
  SavepointImpl savepoint1("savepoint");
  ASSERT_NO_THROW(savepoint1.addMetainfo("key1", "s1"));

  std::stringstream ss;
  SavepointVector s;
  ASSERT_NE(s.insert(savepoint1), -1);
  ASSERT_TRUE(s.addField(savepoint1, FieldID{"u", 0}));

  ss << s;
  EXPECT_TRUE(boost::algorithm::starts_with(ss.str(), "SavepointVector"));
  EXPECT_NE(ss.str().find("savepoint"), std::string::npos);
  EXPECT_NE(ss.str().find("key1"), std::string::npos);
}
