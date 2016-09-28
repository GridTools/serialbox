//===-- Unittest/Cpp/UnittestSTELLADataFieldInfo.cpp --------------------------------*- C++ -*-===//
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

#include "Utility/STELLA.h"
#include "serialbox/Core/Frontend/STELLA/DataFieldInfo.h"
#include <gtest/gtest.h>

#ifdef SERIALBOX_HAS_STELLA

TEST(STELLADataFieldInfoTest, Construction) {
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
  EXPECT_STREQ(info1.ToString().c_str(), "info (7x8x3x1) [ key2=5.000000 key1=str ]");
}

#endif
