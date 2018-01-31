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
#include "serialbox-c/FieldMetainfo.h"
#include "serialbox-c/FortranWrapper.h"
#include "serialbox-c/Metainfo.h"
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
  serialboxFortranSerializerAddMetainfoBoolean(serializer, "bool", true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddMetainfoInt32(serializer, "int", 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddMetainfoFloat32(serializer, "float", 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddMetainfoFloat64(serializer, "double", 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddMetainfoString(serializer, "string", "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add existing meta-info -> Error
  //
  serialboxFortranSerializerAddMetainfoBoolean(serializer, "bool", true);
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Get meta-info
  //
  serialboxMetainfo_t* metaInfo = serialboxSerializerGetGlobalMetainfo(serializer);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetBoolean(metaInfo, "bool"), true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "int"), 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetFloat32(metaInfo, "float"), 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetFloat64(metaInfo, "double"), 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_STREQ(serialboxMetainfoGetString(metaInfo, "string"), "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxMetainfoDestroy(metaInfo);
  serialboxSerializerDestroy(serializer);
}

TEST_F(CFortranWrapperTest, FieldMetainfoImpl) {
  serialboxSerializer_t* serializer =
      serialboxSerializerCreate(Write, directory->path().c_str(), "Field", "Binary");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Register field
  //
  serialboxFortranSerializerRegisterField(serializer, "field", Float64, 8, 30, 40, 50, 60, 1, 1, 23,
                                          42, 0, 0, -2, 2);

  //
  // Add field meta-info
  //
  serialboxFortranSerializerAddFieldMetainfoBoolean(serializer, "field", "bool", true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddFieldMetainfoInt32(serializer, "field", "int", 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddFieldMetainfoFloat32(serializer, "field", "float", 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddFieldMetainfoFloat64(serializer, "field", "double", 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSerializerAddFieldMetainfoString(serializer, "field", "string", "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Field does not exists -> Error
  //
  serialboxFortranSerializerAddFieldMetainfoBoolean(serializer, "fieldX", "bool", true);
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add existing field meta-info -> Error
  //
  serialboxFortranSerializerAddFieldMetainfoBoolean(serializer, "field", "bool", true);
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Get field meta-info
  //
  serialboxFieldMetainfo_t* info = serialboxSerializerGetFieldMetainfo(serializer, "field");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  // Number of dimension
  int numDimension = serialboxFieldMetainfoGetNumDimensions(info);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(numDimension, 4);

  // Dimensions
  int iSize, jSize, kSize, lSize;
  serialboxFortranSerializerGetFieldDimensions(serializer, "field", &iSize, &jSize, &kSize, &lSize);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(iSize, 30);
  EXPECT_EQ(jSize, 40);
  EXPECT_EQ(kSize, 50);
  EXPECT_EQ(lSize, 60);

  // Halos
  int iMinusHalo, iPlusHalo, jMinusHalo, jPlusHalo, kMinusHalo, kPlusHalo, lMinusHalo, lPlusHalo;
  serialboxFortranSerializerGetFieldHalos(serializer, "field", &iMinusHalo, &iPlusHalo, &jMinusHalo, &jPlusHalo, &kMinusHalo, &kPlusHalo, &lMinusHalo, &lPlusHalo);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(iMinusHalo, 1);
  EXPECT_EQ(iPlusHalo, 1);
  EXPECT_EQ(jMinusHalo, 23);
  EXPECT_EQ(jPlusHalo, 42);
  EXPECT_EQ(kMinusHalo, 0);
  EXPECT_EQ(kPlusHalo, 0);
  EXPECT_EQ(lMinusHalo, -2);
  EXPECT_EQ(lPlusHalo, 2);

  // Type
  serialboxTypeID type = serialboxFieldMetainfoGetTypeID(info);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(type, Float64);

  // Meta information
  serialboxMetainfo_t* metaInfo = serialboxFieldMetainfoGetMetainfo(info);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetBoolean(metaInfo, "bool"), true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "int"), 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetFloat32(metaInfo, "float"), 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetFloat64(metaInfo, "double"), 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_STREQ(serialboxMetainfoGetString(metaInfo, "string"), "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxMetainfoDestroy(metaInfo);
  serialboxFieldMetainfoDestroy(info);
  serialboxSerializerDestroy(serializer);
}

TEST_F(CFortranWrapperTest, Savepoint) {
  serialboxSavepoint_t* savepoint = serialboxSavepointCreate("Savepoint");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add meta-info
  //
  serialboxFortranSavepointAddMetainfoBoolean(savepoint, "bool", true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSavepointAddMetainfoInt32(savepoint, "int", 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSavepointAddMetainfoFloat32(savepoint, "float", 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSavepointAddMetainfoFloat64(savepoint, "double", 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxFortranSavepointAddMetainfoString(savepoint, "string", "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Add existing meta-info -> Error
  //
  serialboxFortranSavepointAddMetainfoBoolean(savepoint, "bool", true);
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Get meta-info
  //
  serialboxMetainfo_t* metaInfo = serialboxSavepointGetMetainfo(savepoint);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetBoolean(metaInfo, "bool"), true);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "int"), 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetFloat32(metaInfo, "float"), 1.1f);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetFloat64(metaInfo, "double"), 1.1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_STREQ(serialboxMetainfoGetString(metaInfo, "string"), "str");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxMetainfoDestroy(metaInfo);
  serialboxSavepointDestroy(savepoint);
}

TEST_F(CFortranWrapperTest, ShortFieldName) {
  serialboxSerializer_t* serializer =
      serialboxSerializerCreate(Write, directory->path().c_str(), "Shortname", "Binary");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Register field
  //
  serialboxFortranSerializerRegisterField(serializer, "i0", Float64, 8, 30, 40, 50, 60, 1, 1, 23,
                                          42, 0, 0, -2, 2);

  // Dimensions
  int iSize, jSize, kSize, lSize;
  serialboxFortranSerializerGetFieldDimensions(serializer, "i0", &iSize, &jSize, &kSize, &lSize);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(iSize, 30);
  EXPECT_EQ(jSize, 40);
  EXPECT_EQ(kSize, 50);
  EXPECT_EQ(lSize, 60);

  serialboxSerializerDestroy(serializer);
}
