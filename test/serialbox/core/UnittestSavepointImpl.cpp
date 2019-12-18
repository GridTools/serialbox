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
/// This file implements the unittests of the shared savepoint implementation.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/SavepointImpl.h"
#include "serialbox/core/SavepointImplSerializer.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>
#include <unordered_map>

using namespace serialbox;

TEST(SavepointImplTest, Construction) {
  std::string name("savepoint");
  MetainfoMapImpl metaInfo(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("str"))}, {"key2", MetainfoValueImpl(double(5))}});

  // -----------------------------------------------------------------------------------------------
  //  Empty constructor
  // -----------------------------------------------------------------------------------------------
  {
    SavepointImpl s(name);
    EXPECT_EQ(s.name(), name);
    EXPECT_TRUE(s.empty());

    const SavepointImpl const_s(name);
    EXPECT_EQ(const_s.name(), name);
    EXPECT_TRUE(const_s.empty());
  }

  // -----------------------------------------------------------------------------------------------
  //  Construct with metaInfo
  // -----------------------------------------------------------------------------------------------
  {
    SavepointImpl s(name, metaInfo);

    EXPECT_EQ(s.name(), name);

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");
    EXPECT_EQ(s.getMetainfoAs<std::string>("key1"), "str");

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
    EXPECT_EQ(s.getMetainfoAs<double>("key2"), double(5));

    // Non-existent key -> Exception
    EXPECT_THROW(s.getMetainfoAs<double>("key3"), Exception);

    // Add meta-info
    s.addMetainfo("key3", bool(true));
    EXPECT_EQ(s.metaInfo().at("key3").as<bool>(), bool(true));
    EXPECT_EQ(s.getMetainfoAs<bool>("key3"), bool(true));

    // Add meta-info with existent key -> Exception
    EXPECT_THROW(s.addMetainfo("key3", bool(false)), Exception);
  }

  // -----------------------------------------------------------------------------------------------
  //  Copy construct
  // -----------------------------------------------------------------------------------------------
  {
    SavepointImpl s_to_copy(name, metaInfo);
    SavepointImpl s(s_to_copy);

    EXPECT_EQ(s.name(), s_to_copy.name());

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(),
              s_to_copy.metaInfo().at("key1").as<std::string>());

    // Check for deep copy
    s.metaInfo().insert("newKey", "str");
    ASSERT_FALSE(s.metaInfo() == s_to_copy.metaInfo());

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), s_to_copy.metaInfo().at("key2").as<double>());
  }

  // ---------------------------------------------------------------------------------------------
  //  Copy assign
  // ---------------------------------------------------------------------------------------------
  {
    SavepointImpl s_to_copy(name, metaInfo);
    SavepointImpl s("name");
    s = s_to_copy;

    EXPECT_EQ(s.name(), s_to_copy.name());

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(),
              s_to_copy.metaInfo().at("key1").as<std::string>());
    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), s_to_copy.metaInfo().at("key2").as<double>());
  }

  //----------------------------------------------------------------------------------------------
  //  Move construct
  //----------------------------------------------------------------------------------------------
  {
    SavepointImpl s_to_move(name, metaInfo);
    SavepointImpl s(std::move(s_to_move));

    EXPECT_EQ(s.name(), name);

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
  }

  //----------------------------------------------------------------------------------------------
  //  Move assign
  //----------------------------------------------------------------------------------------------
  {
    SavepointImpl s_to_move(name, metaInfo);
    SavepointImpl s("name");
    s = std::move(s_to_move);

    EXPECT_EQ(s.name(), name);

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
  }

  // -----------------------------------------------------------------------------------------------
  //  Swap
  // -----------------------------------------------------------------------------------------------
  {
    SavepointImpl s(name, metaInfo);
    SavepointImpl s_swap("savepoint-swap", metaInfo);
    s_swap.metaInfo()["key1"] = MetainfoValueImpl(std::string("changed-value"));

    s.swap(s_swap);
    EXPECT_EQ(s.name(), "savepoint-swap");
    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "changed-value");
  }
}

TEST(SavepointImplTest, Comparison) {
  {
    SavepointImpl s1("s1");
    SavepointImpl s2("s2");

    // Savepoints with diffrent names are not equal equal ...
    EXPECT_FALSE(s1 == s2);
    EXPECT_TRUE(s1 != s2);

    // ... even if they have the same meta-data
    s1.addMetainfo("key1", double(5));
    s2.addMetainfo("key1", double(5));
    EXPECT_FALSE(s1 == s2);
    EXPECT_TRUE(s1 != s2);
  }

  {
    SavepointImpl s1("savepoint");
    SavepointImpl s2("savepoint");

    // Savepoints with equal names and empty metainfo are equal
    EXPECT_TRUE(s1 == s2);

    // Savepoints are equal when their metainfo is equal
    s1.addMetainfo("key1", double(5));
    s2.addMetainfo("key1", double(5));
    EXPECT_TRUE(s1 == s2);

    // Savepoints are unequal if their metainfos are not equal
    s1.addMetainfo("key2", std::string("str-1"));
    s2.addMetainfo("key2", std::string("str-2"));
    EXPECT_FALSE(s1 == s2);
  }
}

TEST(SavepointImplTest, Hash) {
  SavepointImpl s1("savepoint");
  s1.addMetainfo("key1", double(5));
  SavepointImpl s2("savepoint");
  s2.addMetainfo("key1", bool(true));

  std::hash<SavepointImpl> hash;

  // Check that operator() of std::hash<Savepoint> adheres to the concept:

  // 1) Does not throw exceptions when called.
  EXPECT_TRUE(noexcept(hash(s1)));

  // 2) For two savepoints s1 and s2 that are equal,
  //    std::hash<Savepoint>()(s1) == std::hash<Savepoint>()(s2).
  EXPECT_EQ(s1, s1);
  EXPECT_EQ(hash(s1), hash(s1));

  // 3) For two diffrent savepoints s1 and s2 that are not equal, their hash will be equal if they
  //    have the same name.
  EXPECT_NE(s1, s2);
  EXPECT_EQ(hash(s1), hash(s2));
}

TEST(SavepointImplTest, HashMap) {
  SavepointImpl s1("savepoint");
  s1.addMetainfo("key1", double(5));

  SavepointImpl s2("savepoint");
  s2.addMetainfo("key1", bool(true));

  SavepointImpl s3("diffrent-savepoint");
  s3.addMetainfo("key1", bool(true));

  std::unordered_map<SavepointImpl, int> hashMap;
  using value_type = std::unordered_map<SavepointImpl, int>::value_type;

  // Insert savepoint into empty map
  ASSERT_TRUE(hashMap.insert(value_type{s1, 0}).second);

  // Insert savepoint with same hash but different metaInfo
  ASSERT_TRUE(hashMap.insert(value_type{s2, 1}).second);

  // Insert savepoint with same hash and same metaInfo -> Fail
  ASSERT_FALSE(hashMap.insert(value_type{s1, 0}).second);

  // Insert additional savepoint (diffrent name)
  ASSERT_TRUE(hashMap.insert(value_type{s3, 2}).second);

  EXPECT_EQ(hashMap.size(), 3);
}

TEST(SavepointImplTest, toJSON) {
  std::string name("savepoint");
  MetainfoMapImpl metaInfo(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("str"))}, {"key2", MetainfoValueImpl(double(5))}});

  SavepointImpl s(name, metaInfo);
  json::json j = s;

  // Type
  ASSERT_TRUE(j.count("name"));
  EXPECT_EQ(name, j["name"]);

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

TEST(SavepointImplTest, fromJSON) {
  std::string name("savepoint");
  MetainfoMapImpl metaInfo(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("str"))}, {"key2", MetainfoValueImpl(double(5))}});

  // -----------------------------------------------------------------------------------------------
  //  Success
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
         "name": "savepoint"
     }
    )"_json;

    SavepointImpl s = j;
    EXPECT_EQ(s.name(), name);

    ASSERT_TRUE(s.metaInfo().hasKey("key1"));
    EXPECT_EQ(s.metaInfo().at("key1").as<std::string>(), "str");

    ASSERT_TRUE(s.metaInfo().hasKey("key2"));
    EXPECT_EQ(s.metaInfo().at("key2").as<double>(), double(5));
  }

  // -----------------------------------------------------------------------------------------------
  //  Success (empty meta-info)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"(
     {
         "name": "savepoint"
     }
    )"_json;

    SavepointImpl s = j;
    EXPECT_EQ(s.name(), name);
    EXPECT_TRUE(s.metaInfo().empty());
  }

  // -----------------------------------------------------------------------------------------------
  //  Failure (empty node)
  // -----------------------------------------------------------------------------------------------
  {
    auto j = R"({})"_json;
    SavepointImpl s;
    ASSERT_THROW((s = j), Exception);
  }

  // -----------------------------------------------------------------------------------------------
  //  Failure (missing name)
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
         }
     }
    )"_json;
    ASSERT_ANY_THROW(SavepointImpl s = j);
  }
}

TEST(SavepointImplTest, toString) {
  std::string name("savepoint");
  MetainfoMapImpl metaInfo(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("str"))}, {"key2", MetainfoValueImpl(double(5))}});

  std::stringstream ss;
  SavepointImpl savepoint(name, metaInfo);

  ss << savepoint;
  EXPECT_NE(ss.str().find(name), std::string::npos);
  EXPECT_NE(ss.str().find("key1"), std::string::npos);
  EXPECT_NE(ss.str().find("key2"), std::string::npos);
  EXPECT_EQ(ss.str(), savepoint.toString());
}
