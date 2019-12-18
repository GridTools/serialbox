//===-- serialbox-c/UnittestFieldMetainfoImpl.cpp ---------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface FieldMetainfoImpl.
///
//===------------------------------------------------------------------------------------------===//

#include "utility/CInterfaceTestBase.h"
#include "serialbox-c/FieldMetainfo.h"
#include "serialbox-c/Metainfo.h"
#include <gtest/gtest.h>

namespace {

class CFieldMetainfoImplTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

TEST_F(CFieldMetainfoImplTest, Test) {
  serialboxTypeID type = Float64;
  int dimensions[3] = {10, 15, 20};

  serialboxFieldMetainfo_t* info = serialboxFieldMetainfoCreate(type, dimensions, 3);

  //
  // Query type
  //
  ASSERT_EQ((int)type, (int)serialboxFieldMetainfoGetTypeID(info));

  //
  // Query dimensions
  //
  ASSERT_EQ(serialboxFieldMetainfoGetNumDimensions(info), 3);
  EXPECT_EQ(serialboxFieldMetainfoGetDimensions(info)[0], 10);
  EXPECT_EQ(serialboxFieldMetainfoGetDimensions(info)[1], 15);
  EXPECT_EQ(serialboxFieldMetainfoGetDimensions(info)[2], 20);

  //
  // Add meta-information
  //
  {
    serialboxMetainfo_t* metaInfo = serialboxFieldMetainfoGetMetainfo(info);
    ASSERT_FALSE(metaInfo->ownsData);
    ASSERT_TRUE(serialboxMetainfoAddBoolean(metaInfo, "key", true));
    serialboxMetainfoDestroy(metaInfo);
  }

  //
  // Query meta-information
  //
  {
    serialboxMetainfo_t* metaInfo = serialboxFieldMetainfoGetMetainfo(info);
    ASSERT_FALSE(metaInfo->ownsData);    
    ASSERT_TRUE(serialboxMetainfoHasKey(metaInfo, "key"));
    EXPECT_EQ(serialboxMetainfoGetBoolean(metaInfo, "key"), true);
    serialboxMetainfoDestroy(metaInfo);   
  }

  //
  // Copy construct
  //

  serialboxFieldMetainfo_t* info2 = serialboxFieldMetainfoCreateFromFieldMetainfo(info);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  
  //
  // Comparison
  //
  ASSERT_TRUE(serialboxFieldMetainfoEqual(info, info2));

  //
  // Release memory
  //
  serialboxFieldMetainfoDestroy(info);
  serialboxFieldMetainfoDestroy(info2);
}
