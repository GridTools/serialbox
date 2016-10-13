//===-- serialbox-c/UnittestFieldMetaInfo.cpp ---------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface FieldMetaInfo.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/CInterfaceTestBase.h"
#include "serialbox-c/FieldMetaInfo.h"
#include "serialbox-c/MetaInfo.h"
#include <gtest/gtest.h>

namespace {

class CFieldMetaInfoTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

TEST_F(CFieldMetaInfoTest, Test) {
  serialboxTypeID type = Float64;
  int dimensions[3] = {10, 15, 20};

  serialboxFieldMetaInfo_t* info = serialboxFieldMetaInfoCreate(type, dimensions, 3);

  //
  // Query type
  //
  ASSERT_EQ((int)type, (int)serialboxFieldMetaInfoGetTypeID(info));

  //
  // Query dimensions
  //
  ASSERT_EQ(serialboxFieldMetaInfoGetNumDimensions(info), 3);
  EXPECT_EQ(serialboxFieldMetaInfoGetDimensions(info)[0], 10);
  EXPECT_EQ(serialboxFieldMetaInfoGetDimensions(info)[1], 15);
  EXPECT_EQ(serialboxFieldMetaInfoGetDimensions(info)[2], 20);

  //
  // Add meta-information
  //
  {
    serialboxMetaInfo_t* metaInfo = serialboxFieldMetaInfoGetMetaInfo(info);
    ASSERT_FALSE(metaInfo->ownsData);
    ASSERT_TRUE(serialboxMetaInfoAddBoolean(metaInfo, "key", true));
    serialboxMetaInfoDestroy(metaInfo);
  }

  //
  // Query meta-information
  //
  {
    serialboxMetaInfo_t* metaInfo = serialboxFieldMetaInfoGetMetaInfo(info);
    ASSERT_FALSE(metaInfo->ownsData);    
    ASSERT_TRUE(serialboxMetaInfoHasKey(metaInfo, "key"));
    EXPECT_EQ(serialboxMetaInfoGetBoolean(metaInfo, "key"), true);
    serialboxMetaInfoDestroy(metaInfo);   
  }

  //
  // Comparison
  //
  dimensions[0] = 1024;
  serialboxFieldMetaInfo_t* info2 = serialboxFieldMetaInfoCreate(type, dimensions, 3);
  ASSERT_TRUE(serialboxFieldMetaInfoEqual(info, info));
  ASSERT_FALSE(serialboxFieldMetaInfoEqual(info, info2));

  //
  // Release memory
  //
  serialboxFieldMetaInfoDestroy(info);
  serialboxFieldMetaInfoDestroy(info2);
}
