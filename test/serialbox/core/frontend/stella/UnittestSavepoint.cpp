//===-- serialbox/core/frontend/stella/UnittestSavepoint.cpp ------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the Savepoint implementation of the ser frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/frontend/stella/Savepoint.h"
#include "serialbox/core/frontend/stella/SerializationException.h"
#include "serialbox/core/SavepointImpl.h"
#include <boost/algorithm/string.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <gtest/gtest.h>

using namespace serialbox;

TEST(STELLASavepointTest, Construction) {
  // Construction
  ser::Savepoint savepoint1;
  savepoint1.Init("savepoint");

  // Add meta-information
  EXPECT_NO_THROW(savepoint1.AddMetainfo("bool", bool()));
  EXPECT_NO_THROW(savepoint1.AddMetainfo("int", int()));
  EXPECT_NO_THROW(savepoint1.AddMetainfo("float", float()));
  EXPECT_NO_THROW(savepoint1.AddMetainfo("double", double()));
  EXPECT_NO_THROW(savepoint1.AddMetainfo("std::string", std::string("")));
  ASSERT_EQ(savepoint1.metainfo().size(), 5);

  EXPECT_NO_THROW(savepoint1.AddMetainfo("key1", "str"));
  EXPECT_NO_THROW(savepoint1.AddMetainfo("key2", double(5)));

  EXPECT_THROW(savepoint1.AddMetainfo("key2", double(5)), ser::SerializationException);

  // Query meta-information
  EXPECT_EQ(savepoint1.name(), "savepoint");
  EXPECT_EQ(savepoint1.metainfo().AsString("key1"), "str");
  EXPECT_EQ(savepoint1.metainfo().AsDouble("key2"), double(5));

  // Comparison
  boost::shared_ptr<SavepointImpl> savepointImpl2 = boost::make_shared<SavepointImpl>("savepoint2");
  ser::Savepoint savepoint2(savepointImpl2);
  EXPECT_EQ(savepoint2.name(), "savepoint2");

  EXPECT_TRUE(savepoint1 == savepoint1);
  EXPECT_TRUE(savepoint1 != savepoint2);

  // Assignment
  savepoint2 = savepoint1;
  EXPECT_TRUE(savepoint2 == savepoint1);

  // Copy constructor
  ser::Savepoint savepoint3(savepoint1);
  EXPECT_TRUE(savepoint3 == savepoint1);

  // ToString
  EXPECT_TRUE(boost::algorithm::starts_with(savepoint1.ToString(), "savepoint"));

  std::stringstream ss;
  ss << savepoint1;
  EXPECT_EQ(ss.str(), savepoint1.ToString());
}
