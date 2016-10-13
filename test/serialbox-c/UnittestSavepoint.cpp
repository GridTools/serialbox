//===-- serialbox-c/UnittestSavepoint.cpp -------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface Savepoint.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/CInterfaceTestBase.h"
#include "serialbox-c/MetaInfo.h"
#include "serialbox-c/Savepoint.h"
#include <gtest/gtest.h>

namespace {

class CSavepointTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

TEST_F(CSavepointTest, Test) {
  const char* name = "savepoint";

  serialboxSavepoint_t* savepoint = serialboxSavepointCreate(name);

  //
  // Query name
  //
  ASSERT_STRCASEEQ(serialboxSavepointGetName(savepoint), name);

  //
  // Add meta-information
  //
  {
    serialboxMetaInfo_t* metaInfo = serialboxSavepointGetMetaInfo(savepoint);
    ASSERT_FALSE(metaInfo->ownsData);
    ASSERT_TRUE(serialboxMetaInfoAddBoolean(metaInfo, "key", true));
    serialboxMetaInfoDestroy(metaInfo);
  }

  //
  // Query meta-information
  //
  {
    serialboxMetaInfo_t* metaInfo = serialboxSavepointGetMetaInfo(savepoint);
    ASSERT_FALSE(metaInfo->ownsData);    
    ASSERT_TRUE(serialboxMetaInfoHasKey(metaInfo, "key"));
    EXPECT_EQ(serialboxMetaInfoGetBoolean(metaInfo, "key"), true);
    serialboxMetaInfoDestroy(metaInfo); 
  }

  //
  // Compare equal
  //
  serialboxSavepoint_t* savepoint_not_equal = serialboxSavepointCreate(name);
  serialboxSavepoint_t* savepoint_equal = serialboxSavepointCreateFromSavepoint(savepoint);

  ASSERT_FALSE(serialboxSavepointEqual(savepoint, savepoint_not_equal));
  ASSERT_TRUE(serialboxSavepointEqual(savepoint, savepoint_equal));

  //
  // To string
  //
  const char* strBuffer = serialboxSavepointToString(savepoint);
  std::string str(strBuffer);
  std::free((void*)strBuffer);

  EXPECT_NE(str.find("key"), std::string::npos);
  EXPECT_NE(str.find("true"), std::string::npos);

  //
  // Release memory
  //
  serialboxSavepointDestroy(savepoint);
  serialboxSavepointDestroy(savepoint_not_equal);
  serialboxSavepointDestroy(savepoint_equal);
}
