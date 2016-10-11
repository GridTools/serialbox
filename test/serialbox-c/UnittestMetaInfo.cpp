//===-- serialbox-c/UnittestMetaInfo.cpp --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface MetaInfo.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/CInterfaceTestBase.h"
#include "serialbox-c/MetaInfo.h"
#include <gtest/gtest.h>

namespace internal {

template <class T>
testing::AssertionResult arraysAreEqual(T* a1, T* a2, int len) {
  for(int i = 0; i < len; ++i)
    if(a1[i] != a2[i])
      return testing::AssertionFailure() << "array mismatch at i = " << i << " (" << a1[i]
                                         << " != " << a2[i] << ")";
  return testing::AssertionSuccess();
}

template <>
testing::AssertionResult arraysAreEqual(serialboxArrayOfString_t a1, serialboxArrayOfString_t a2,
                                        int len) {
  for(int i = 0; i < len; ++i) {
    std::string str1(a1[i]);
    std::string str2(a2[i]);
    if(str1 != str2)
      return testing::AssertionFailure() << "array mismatch at i = " << i << " (" << str1
                                         << " != " << str2 << ")";
  }
  return testing::AssertionSuccess();
}

} // namespace internal

namespace {

class CMetaInfoTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

TEST_F(CMetaInfoTest, Test) {
  serialboxMetaInfo_t metaInfo = serialboxMetaInfoCreate();
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  ASSERT_TRUE(serialboxMetaInfoIsEmpty(metaInfo));

  //
  // Add key/value pairs
  //
  ASSERT_TRUE(serialboxMetaInfoAddBoolean(metaInfo, "bool", true));
  ASSERT_TRUE(serialboxMetaInfoHasKey(metaInfo, "bool"));

  ASSERT_TRUE(serialboxMetaInfoAddInt32(metaInfo, "int32", 2));
  ASSERT_TRUE(serialboxMetaInfoHasKey(metaInfo, "int32"));

  ASSERT_TRUE(serialboxMetaInfoAddInt64(metaInfo, "int64", 2));
  ASSERT_TRUE(serialboxMetaInfoHasKey(metaInfo, "int64"));

  ASSERT_TRUE(serialboxMetaInfoAddFloat32(metaInfo, "float32", float(1.1f)));
  ASSERT_TRUE(serialboxMetaInfoHasKey(metaInfo, "float32"));

  ASSERT_TRUE(serialboxMetaInfoAddFloat64(metaInfo, "float64", double(1.1)));
  ASSERT_TRUE(serialboxMetaInfoHasKey(metaInfo, "float64"));

  ASSERT_TRUE(serialboxMetaInfoAddString(metaInfo, "string", "str"));
  ASSERT_TRUE(serialboxMetaInfoHasKey(metaInfo, "string"));

  ASSERT_EQ(serialboxMetaInfoGetSize(metaInfo), 6);

  //
  // Add existing key/value pair -> False
  //
  ASSERT_FALSE(serialboxMetaInfoAddBoolean(metaInfo, "bool", true));

  //
  // Add key/arrays
  //
  serialboxBoolean_t arrayOfBooleanRef[2] = {true, false};
  ASSERT_TRUE(serialboxMetaInfoAddArrayOfBoolean(metaInfo, "ArrayOfBoolean", arrayOfBooleanRef, 2));

  serialboxInt32_t arrayOfInt32Ref[2] = {1, 2};
  ASSERT_TRUE(serialboxMetaInfoAddArrayOfInt32(metaInfo, "ArrayOfInt32", arrayOfInt32Ref, 2));

  serialboxInt64_t arrayOfInt64Ref[2] = {4, 3};
  ASSERT_TRUE(serialboxMetaInfoAddArrayOfInt64(metaInfo, "ArrayOfInt64", arrayOfInt64Ref, 2));

  serialboxFloat32_t arrayOfFloat32Ref[2] = {1.1f, 1.2f};
  ASSERT_TRUE(serialboxMetaInfoAddArrayOfFloat32(metaInfo, "ArrayOfFloat32", arrayOfFloat32Ref, 2));

  serialboxFloat64_t arrayOfFloat64Ref[2] = {4.1, 3.2};
  ASSERT_TRUE(serialboxMetaInfoAddArrayOfFloat64(metaInfo, "ArrayOfFloat64", arrayOfFloat64Ref, 2));

  serialboxString_t arrayOfStringRef[2] = {"str1", "str2"};
  ASSERT_TRUE(serialboxMetaInfoAddArrayOfString(metaInfo, "ArrayOfString", arrayOfStringRef, 2));

  //
  // Query values
  //
  ASSERT_EQ(serialboxMetaInfoGetBoolean(metaInfo, "bool"), true);
  ASSERT_EQ(serialboxMetaInfoGetInt32(metaInfo, "int32"), 2);
  ASSERT_EQ(serialboxMetaInfoGetInt64(metaInfo, "int64"), 2);
  ASSERT_EQ(serialboxMetaInfoGetFloat32(metaInfo, "float32"), float(1.1f));
  ASSERT_EQ(serialboxMetaInfoGetFloat64(metaInfo, "float64"), double(1.1));
  ASSERT_STREQ(serialboxMetaInfoGetString(metaInfo, "string"), "str");

  // Key does not exists -> FatalError
  serialboxMetaInfoGetBoolean(metaInfo, "bool-XXX");
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Query arrays
  //
  int len;

  serialboxArrayOfBoolean_t arrayOfBoolean;
  serialboxMetaInfoGetArrayOfBoolean(metaInfo, "ArrayOfBoolean", &arrayOfBoolean, &len);
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfBoolean, arrayOfBooleanRef, len));
  std::free(arrayOfBoolean);

  serialboxArrayOfInt32_t arrayOfInt32;
  serialboxMetaInfoGetArrayOfInt32(metaInfo, "ArrayOfInt32", &arrayOfInt32, &len);
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfInt32, arrayOfInt32Ref, len));
  std::free(arrayOfInt32);

  serialboxArrayOfInt64_t arrayOfInt64;
  serialboxMetaInfoGetArrayOfInt64(metaInfo, "ArrayOfInt64", &arrayOfInt64, &len);
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfInt64, arrayOfInt64Ref, len));
  std::free(arrayOfInt64);

  serialboxArrayOfFloat32_t arrayOfFloat32;
  serialboxMetaInfoGetArrayOfFloat32(metaInfo, "ArrayOfFloat32", &arrayOfFloat32, &len);
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfFloat32, arrayOfFloat32Ref, len));
  std::free(arrayOfFloat32);

  serialboxArrayOfFloat64_t arrayOfFloat64;
  serialboxMetaInfoGetArrayOfFloat64(metaInfo, "ArrayOfFloat64", &arrayOfFloat64, &len);
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfFloat64, arrayOfFloat64Ref, len));
  std::free(arrayOfFloat64);

  serialboxArrayOfString_t arrayOfString;
  serialboxMetaInfoGetArrayOfString(metaInfo, "ArrayOfString", &arrayOfString, &len);
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfString, arrayOfStringRef, len));

  for(int i = 0; i < len; ++i)
    std::free((void*)arrayOfString[i]);
  std::free(arrayOfString);

  //
  // ToString
  //
  serialboxMetaInfo_t metaInfo2 = serialboxMetaInfoCreate();
  serialboxMetaInfoToString(metaInfo2);
  ASSERT_TRUE(serialboxMetaInfoAddBoolean(metaInfo2, "key", true));

  const char* strBuffer = serialboxMetaInfoToString(metaInfo2);
  std::string str(strBuffer);
  std::free((void*)strBuffer);

  EXPECT_NE(str.find("key"), std::string::npos);
  EXPECT_NE(str.find("true"), std::string::npos);

  //
  // Keys
  //
  char** keys;
  serialboxMetaInfoGetKeys(metaInfo2, &keys, &len);

  ASSERT_EQ(len, 1);
  ASSERT_STREQ(keys[0], "key");

  for(int i = 0; i < len; ++i)
    std::free(keys[i]);
  std::free(keys);

  //
  // Types
  //
  serialboxTypeID* types;
  serialboxMetaInfoGetTypes(metaInfo2, &types, &len);

  ASSERT_EQ(len, 1);
  ASSERT_EQ(types[0], Boolean);

  std::free(types);

  //
  // Clear map
  //
  serialboxMetaInfoClear(metaInfo);
  ASSERT_TRUE(serialboxMetaInfoIsEmpty(metaInfo));

  //
  // Release memory
  //
  serialboxMetaInfoDestroy(&metaInfo);
  serialboxMetaInfoDestroy(&metaInfo2);
}
