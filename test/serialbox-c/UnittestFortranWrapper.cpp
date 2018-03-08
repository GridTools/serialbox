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
  int metaInfoValueBool;
  serialboxFortranSerializerGetMetainfoBoolean(serializer, "bool", &metaInfoValueBool);
  EXPECT_EQ(true, (bool) metaInfoValueBool);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  int metaInfoValueInt;
  serialboxFortranSerializerGetMetainfoInt32(serializer, "int", &metaInfoValueInt);
  EXPECT_EQ(2, metaInfoValueInt);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  float metaInfoValueFloat;
  serialboxFortranSerializerGetMetainfoFloat32(serializer, "float", &metaInfoValueFloat);
  EXPECT_EQ(1.1f, metaInfoValueFloat);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  double metaInfoValueDouble;
  serialboxFortranSerializerGetMetainfoFloat64(serializer, "double", &metaInfoValueDouble);
  EXPECT_EQ(1.1, metaInfoValueDouble);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  const char* metaInfoValueString;
  serialboxFortranSerializerGetMetainfoString(serializer, "string", &metaInfoValueString);
  EXPECT_EQ("str", std::string(metaInfoValueString, strnlen(metaInfoValueString, 4)));
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

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

  // Rank
  int rank;
  serialboxFortranSerializerGetFieldRank(serializer, "field", &rank);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(rank, 4);

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
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

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

TEST_F(CFortranWrapperTest, Loc) {

  int test1 = 42, test2 = 109;
  int *ptr1a = &test1, *ptr1b = &test1, *ptr2 = &test2;
  intptr_t loct1, locp1a, locp1b, loct2, locp2;

  serialboxFortranLoc(&test1, &loct1);
  serialboxFortranLoc(ptr1a, &locp1a);
  serialboxFortranLoc(ptr1b, &locp1b);
  serialboxFortranLoc(&test2, &loct2);
  serialboxFortranLoc(ptr2, &locp2);

  ASSERT_GT(loct1, 0);
  ASSERT_GT(locp1a, 0);
  ASSERT_GT(locp1b, 0);
  ASSERT_GT(loct2, 0);
  ASSERT_GT(locp2, 0);

  ASSERT_EQ(loct1, locp1a);
  ASSERT_EQ(loct1, locp1b);
  ASSERT_EQ(loct2, locp2);
  ASSERT_NE(loct1, loct2);
}

TEST_F(CFortranWrapperTest, Rank) {

	serialboxSerializer_t* serializer =
	      serialboxSerializerCreate(Write, directory->path().c_str(), "Rank", "Binary");
	  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

	serialboxFortranSerializerRegisterField(serializer, "int0", Int32, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	serialboxFortranSerializerRegisterField(serializer, "int1", Int32, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	serialboxFortranSerializerRegisterField(serializer, "int2", Int32, 4, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	serialboxFortranSerializerRegisterField(serializer, "int3", Int32, 4, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	serialboxFortranSerializerRegisterField(serializer, "int4", Int32, 4, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

	int rank;
	serialboxFortranSerializerGetFieldRank(serializer, "int0", &rank);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	EXPECT_EQ(rank, 1);
	serialboxFortranSerializerGetFieldRank(serializer, "int1", &rank);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	EXPECT_EQ(rank, 1);
	serialboxFortranSerializerGetFieldRank(serializer, "int2", &rank);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	EXPECT_EQ(rank, 2);
	serialboxFortranSerializerGetFieldRank(serializer, "int3", &rank);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	EXPECT_EQ(rank, 3);
	serialboxFortranSerializerGetFieldRank(serializer, "int4", &rank);
	ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
	EXPECT_EQ(rank, 4);
}
