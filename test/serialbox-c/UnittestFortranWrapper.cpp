//===-- serialbox-c/UnittestFortranWrapper.cpp --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the Fortran wrapper.
///
//===------------------------------------------------------------------------------------------===//

#include "utility/CInterfaceTestBase.h"
#include "serialbox-c/FieldMetaInfo.h"
#include "serialbox-c/FortranWrapper.h"
#include "serialbox-c/MetaInfo.h"
#include "serialbox-c/Savepoint.h"
#include "serialbox-c/Serializer.h"
#include <gtest/gtest.h>

namespace {

class CFortranWrapperTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

TEST_F(CFortranWrapperTest, Serializer) {
  serialboxSerializer_t* serializer =
      serialboxSerializerCreate(Write, directory->path().c_str(), "Field", "Binary");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add global meta-info
  //
  serialboxFortranSerializerAddMetaInfoBoolean(serializer, "bool", true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddMetaInfoInt32(serializer, "int", 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddMetaInfoFloat32(serializer, "float", 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddMetaInfoFloat64(serializer, "double", 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddMetaInfoString(serializer, "string", "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add existing meta-info -> Error
  //
  serialboxFortranSerializerAddMetaInfoBoolean(serializer, "bool", true);
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Get meta-info
  //
  serialboxMetaInfo_t* metaInfo = serialboxSerializerGetGlobalMetaInfo(serializer);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetBoolean(metaInfo, "bool"), true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetInt32(metaInfo, "int"), 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetFloat32(metaInfo, "float"), 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetFloat64(metaInfo, "double"), 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_STREQ(serialboxMetaInfoGetString(metaInfo, "string"), "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxMetaInfoDestroy(metaInfo);
  serialboxSerializerDestroy(serializer);
}

TEST_F(CFortranWrapperTest, FieldMetaInfo) {
  serialboxSerializer_t* serializer =
      serialboxSerializerCreate(Write, directory->path().c_str(), "Field", "Binary");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Register field
  //
  serialboxFortranSerializerRegisterField(serializer, "field", Float64, 8, 30, 40, 50, 60);

  //
  // Add field meta-info
  //
  serialboxFortranSerializerAddFieldMetaInfoBoolean(serializer, "field", "bool", true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddFieldMetaInfoInt32(serializer, "field", "int", 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddFieldMetaInfoFloat32(serializer, "field", "float", 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddFieldMetaInfoFloat64(serializer, "field", "double", 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddFieldMetaInfoString(serializer, "field", "string", "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Field does not exists -> Error
  //
  serialboxFortranSerializerAddFieldMetaInfoBoolean(serializer, "fieldX", "bool", true);
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add existing field meta-info -> Error
  //
  serialboxFortranSerializerAddFieldMetaInfoBoolean(serializer, "field", "bool", true);
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Get field meta-info
  //
  serialboxFieldMetaInfo_t* info = serialboxSerializerGetFieldMetaInfo(serializer, "field");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  // Number of dimension
  int numDimension = serialboxFieldMetaInfoGetNumDimensions(info);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(numDimension, 4);

  // Dimensions
  const int* dimension = serialboxFieldMetaInfoGetDimensions(info);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(dimension[0], 30);
  EXPECT_EQ(dimension[1], 40);
  EXPECT_EQ(dimension[2], 50);
  EXPECT_EQ(dimension[3], 60);

  // Type
  serialboxTypeID type = serialboxFieldMetaInfoGetTypeID(info);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(type, Float64);

  // Meta information
  serialboxMetaInfo_t* metaInfo = serialboxFieldMetaInfoGetMetaInfo(info);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetBoolean(metaInfo, "bool"), true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetInt32(metaInfo, "int"), 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetFloat32(metaInfo, "float"), 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetFloat64(metaInfo, "double"), 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_STREQ(serialboxMetaInfoGetString(metaInfo, "string"), "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxMetaInfoDestroy(metaInfo);
  serialboxFieldMetaInfoDestroy(info);
  serialboxSerializerDestroy(serializer);
}

TEST_F(CFortranWrapperTest, Savepoint) {
  serialboxSavepoint_t* savepoint = serialboxSavepointCreate("Savepoint");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add meta-info
  //
  serialboxFortranSavepointAddMetaInfoBoolean(savepoint, "bool", true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSavepointAddMetaInfoInt32(savepoint, "int", 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSavepointAddMetaInfoFloat32(savepoint, "float", 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSavepointAddMetaInfoFloat64(savepoint, "double", 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSavepointAddMetaInfoString(savepoint, "string", "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add existing meta-info -> Error
  //
  serialboxFortranSavepointAddMetaInfoBoolean(savepoint, "bool", true);
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Get meta-info
  //
  serialboxMetaInfo_t* metaInfo = serialboxSavepointGetMetaInfo(savepoint);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetBoolean(metaInfo, "bool"), true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetInt32(metaInfo, "int"), 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetFloat32(metaInfo, "float"), 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetaInfoGetFloat64(metaInfo, "double"), 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_STREQ(serialboxMetaInfoGetString(metaInfo, "string"), "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxMetaInfoDestroy(metaInfo);
  serialboxSavepointDestroy(savepoint);
}
