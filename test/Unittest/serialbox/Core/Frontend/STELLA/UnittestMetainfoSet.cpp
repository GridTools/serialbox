//===--serialbox/Core/Frontend/STELLA/UnittestserMetainfoSet.cpp --------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the MetainfoSet of the ser Frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Frontend/STELLA/MetainfoSet.h"
#include "serialbox/Core/Frontend/STELLA/SerializationException.h"
#include "serialbox/Core/MetaInfoMap.h"
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <gtest/gtest.h>

using namespace serialbox;

TEST(STELLAMetainfoSetTest, Construction) {
  ser::MetainfoSet set;
  EXPECT_EQ(set.size(), 0);

  //
  // Add some information
  //

  // bool
  ASSERT_NO_THROW(set.AddMetainfo("bool", bool(true)));
  EXPECT_TRUE(set.HasKey("bool"));
  ASSERT_THROW(set.AddMetainfo("bool", bool(true)), ser::SerializationException);

  // int
  ASSERT_NO_THROW(set.AddMetainfo("int32", int(32)));
  EXPECT_TRUE(set.HasKey("int32"));
  ASSERT_THROW(set.AddMetainfo("int32", int(32)), ser::SerializationException);

  // float
  ASSERT_NO_THROW(set.AddMetainfo("float32", float(32.0f)));
  EXPECT_TRUE(set.HasKey("float32"));
  ASSERT_THROW(set.AddMetainfo("float32", float(32.0f)), ser::SerializationException);
  ASSERT_NO_THROW(set.AddMetainfo("float32_1", float(32.1f)));

  // double
  ASSERT_NO_THROW(set.AddMetainfo("float64", double(64.0f)));
  EXPECT_TRUE(set.HasKey("float64"));
  ASSERT_THROW(set.AddMetainfo("float64", double(64.0f)), ser::SerializationException);
  ASSERT_NO_THROW(set.AddMetainfo("float64_1", double(64.1)));

  // string
  ASSERT_NO_THROW(set.AddMetainfo("string", std::string("str")));
  EXPECT_TRUE(set.HasKey("string"));
  ASSERT_THROW(set.AddMetainfo("string", std::string("str")), ser::SerializationException);

  ASSERT_EQ(set.size(), 7);
  
  auto keys = set.keys();
  ASSERT_EQ(keys.size(), set.size());
  EXPECT_TRUE(std::find(keys.begin(), keys.end(), "bool") != keys.end());
  EXPECT_TRUE(std::find(keys.begin(), keys.end(), "int32") != keys.end());
  EXPECT_TRUE(std::find(keys.begin(), keys.end(), "float32") != keys.end());
  EXPECT_TRUE(std::find(keys.begin(), keys.end(), "float32_1") != keys.end());
  EXPECT_TRUE(std::find(keys.begin(), keys.end(), "float64") != keys.end());
  EXPECT_TRUE(std::find(keys.begin(), keys.end(), "float64_1") != keys.end());
  EXPECT_TRUE(std::find(keys.begin(), keys.end(), "string") != keys.end());

  // Query non-existent key
  ASSERT_FALSE(set.HasKey("XXX"));

  ASSERT_EQ(boost::any_cast<float>(set.AsAny("float32")),
            boost::any_cast<float>(boost::any(float(32.0f))));

  //
  // Query as Boolean
  //
  EXPECT_EQ(set.AsBool("bool"), true);
  EXPECT_EQ(set.AsBool("int32"), true);
  EXPECT_EQ(set.AsBool("float32"), true);
  EXPECT_EQ(set.AsBool("float64"), true);
  EXPECT_EQ(set.AsBool("string"), false);
  ASSERT_THROW(set.AsBool("XXX"), ser::SerializationException);

  //
  // Query as Integer
  //
  EXPECT_EQ(set.AsInt("bool"), 1);
  EXPECT_EQ(set.AsInt("int32"), 32);
  EXPECT_EQ(set.AsInt("float32"), 32);
  ASSERT_THROW(set.AsInt("float32_1"), ser::SerializationException);
  EXPECT_EQ(set.AsInt("float64"), 64);
  ASSERT_THROW(set.AsInt("float64_1"), ser::SerializationException);
  EXPECT_EQ(set.AsInt("string"), 0);
  ASSERT_THROW(set.AsInt("XXX"), ser::SerializationException);

  //
  // Query as Float
  //
  EXPECT_EQ(set.AsFloat("bool"), 1);
  EXPECT_EQ(set.AsFloat("int32"), 32);
  EXPECT_EQ(set.AsFloat("float32"), 32.0f);
  EXPECT_EQ(set.AsFloat("float64"), 64.0f);
  EXPECT_EQ(set.AsFloat("string"), 0);
  ASSERT_THROW(set.AsFloat("XXX"), ser::SerializationException);

  //
  // Query as Double
  //
  EXPECT_EQ(set.AsDouble("bool"), 1);
  EXPECT_EQ(set.AsDouble("int32"), 32);
  EXPECT_EQ(set.AsDouble("float32"), 32.0);
  EXPECT_EQ(set.AsDouble("float64"), 64.0);
  EXPECT_EQ(set.AsDouble("string"), 0);
  ASSERT_THROW(set.AsDouble("XXX"), ser::SerializationException);

  //
  // Query as String
  //
  EXPECT_EQ(set.AsString("bool"), "true");
  EXPECT_EQ(set.AsString("int32"), "32");
  EXPECT_EQ(set.AsString("float32"), "32.000000");
  EXPECT_EQ(set.AsString("float64"), "64.000000");
  EXPECT_EQ(set.AsString("string"), "str");
  ASSERT_THROW(set.AsString("XXX"), ser::SerializationException);

  //
  // ToString
  //
  std::string str = set.ToString();
  EXPECT_NE(str.find("bool"), std::string::npos);
  EXPECT_NE(str.find("int32"), std::string::npos);
  EXPECT_NE(str.find("float32"), std::string::npos);
  EXPECT_NE(str.find("float32_1"), std::string::npos);
  EXPECT_NE(str.find("float64"), std::string::npos);
  EXPECT_NE(str.find("float64_1"), std::string::npos);
  EXPECT_NE(str.find("string"), std::string::npos);

  //
  // Assignment
  //
  boost::shared_ptr<MetaInfoMap> map2 = boost::make_shared<MetaInfoMap>();
  ser::MetainfoSet set2(map2);
  set2 = set;
  ASSERT_EQ(set2.size(), 7);
  ASSERT_EQ(map2->size(), 7);
  
  //
  // Comparison
  //
  ASSERT_TRUE(set2 == set);

  //
  // Clear
  //
  set.Cleanup();
  ASSERT_EQ(set.size(), 0);
}
