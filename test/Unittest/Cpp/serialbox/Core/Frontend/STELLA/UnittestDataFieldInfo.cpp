//===-- serialbox/Core/Frontend/STELLA/UnittestDataFieldInfo.cpp --------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the DataFieldInfo of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Frontend/STELLA/DataFieldInfo.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

TEST(STELLADataFieldInfoTest, InitConstruction) {
  //
  // Default construct
  //
  ser::DataFieldInfo info;
  
  info.Init("field1", "int", 4, 2, 42, 1, 1, 12, 1, 1, 0, 0, 0, 0, 2, 2);
  
  //
  // Check infomration was set correct
  //
  EXPECT_EQ(std::string("field1"), info.name());
  EXPECT_EQ(std::string("int"), info.type());
  EXPECT_EQ(4, info.bytesPerElement());
  EXPECT_EQ(2, info.rank());
  EXPECT_EQ(42, info.iSize());
  EXPECT_EQ(1, info.jSize());
  EXPECT_EQ(1, info.kSize());
  EXPECT_EQ(12, info.lSize());
  EXPECT_EQ(1, info.iMinusHaloSize());
  EXPECT_EQ(0, info.jMinusHaloSize());
  EXPECT_EQ(0, info.kMinusHaloSize());
  EXPECT_EQ(2, info.lMinusHaloSize());
  EXPECT_EQ(1, info.iPlusHaloSize());
  EXPECT_EQ(0, info.jPlusHaloSize());
  EXPECT_EQ(0, info.kPlusHaloSize());
  EXPECT_EQ(2, info.lPlusHaloSize());
}

#include "Utility/Cpp/STELLA.h"
#ifdef SERIALBOX_HAS_STELLA

TEST(STELLADataFieldInfoTest, STELLAConstruction) {
  //
  // Default construct
  //
  ser::DataFieldInfo info1;

  //
  // Initialize with STELLA field
  //
  IJKSize domain;
  domain.Init(1, 2, 3);

  KBoundary kboundary;
  kboundary.Init(1, 1);

  IJKRealField field;
  field.Init("info", domain, kboundary);

  info1.Init(field);

  //
  // Add meta-info
  //
  info1.AddMetainfo("key1", "str");
  info1.AddMetainfo("key2", double(5));

  //
  // Copy constructor
  //
  ser::DataFieldInfo info2 = info1;
  ASSERT_TRUE(info1 == info2);

  EXPECT_EQ(info2.metainfo().AsString("key1"), "str");
  EXPECT_EQ(info2.metainfo().AsDouble("key2"), double(5));

  //
  // ToString
  //
  EXPECT_TRUE(boost::algorithm::starts_with(info1.ToString(), "info (7x8x3x1)"));
}

#endif
