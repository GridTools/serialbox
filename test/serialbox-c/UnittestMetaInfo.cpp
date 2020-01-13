//===-- serialbox-c/UnittestMetainfo.cpp --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface Metainfo.
///
//===------------------------------------------------------------------------------------------===//

#include "utility/CInterfaceTestBase.h"
#include "serialbox-c/Metainfo.h"
#include "serialbox-c/Utility.h"
#include <gtest/gtest.h>

using namespace serialboxC;

namespace internal {

template <class T>
testing::AssertionResult arraysAreEqual(T* a1, T* a2) {
  if(a1->len != a2->len)
    return testing::AssertionFailure() << "array size mismatch (" << a1->len << " vs. " << a2->len
                                       << ")";
  for(int i = 0; i < a1->len; ++i)
    if(a1->data[i] != a2->data[i])
      return testing::AssertionFailure() << "array mismatch at i = " << i << " (" << a1->data[i]
                                         << " != " << a2->data[i] << ")";
  return testing::AssertionSuccess();
}

template <>
testing::AssertionResult arraysAreEqual(serialboxArrayOfString_t* a1,
                                        serialboxArrayOfString_t* a2) {
  if(a1->len != a2->len)
    return testing::AssertionFailure() << "array size mismatch (" << a1->len << " vs. " << a2->len
                                       << ")";

  for(int i = 0; i < a1->len; ++i) {
    std::string str1(a1->data[i]);
    std::string str2(a2->data[i]);
    if(str1 != str2)
      return testing::AssertionFailure() << "array mismatch at i = " << i << " (" << str1
                                         << " != " << str2 << ")";
  }
  return testing::AssertionSuccess();
}

} // namespace internal

namespace {

class CMetainfoTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

TEST_F(CMetainfoTest, Test) {
  serialboxMetainfo_t* metaInfo = serialboxMetainfoCreate();
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  ASSERT_TRUE(serialboxMetainfoIsEmpty(metaInfo));

  //
  // Add key/value pairs
  //
  ASSERT_TRUE(serialboxMetainfoAddBoolean(metaInfo, "bool", true));
  EXPECT_TRUE(serialboxMetainfoHasKey(metaInfo, "bool"));
  EXPECT_EQ(serialboxMetainfoGetTypeIDOfKey(metaInfo, "bool"), Boolean);  

  ASSERT_TRUE(serialboxMetainfoAddInt32(metaInfo, "int32", 2));
  EXPECT_TRUE(serialboxMetainfoHasKey(metaInfo, "int32"));
  EXPECT_EQ(serialboxMetainfoGetTypeIDOfKey(metaInfo, "int32"), Int32);    

  ASSERT_TRUE(serialboxMetainfoAddInt64(metaInfo, "int64", 2));
  EXPECT_TRUE(serialboxMetainfoHasKey(metaInfo, "int64"));
  EXPECT_EQ(serialboxMetainfoGetTypeIDOfKey(metaInfo, "int64"), Int64);    

  ASSERT_TRUE(serialboxMetainfoAddFloat32(metaInfo, "float32", float(1.1f)));
  EXPECT_TRUE(serialboxMetainfoHasKey(metaInfo, "float32"));
  EXPECT_EQ(serialboxMetainfoGetTypeIDOfKey(metaInfo, "float32"), Float32);    

  ASSERT_TRUE(serialboxMetainfoAddFloat64(metaInfo, "float64", double(1.1)));
  EXPECT_TRUE(serialboxMetainfoHasKey(metaInfo, "float64"));
  EXPECT_EQ(serialboxMetainfoGetTypeIDOfKey(metaInfo, "float64"), Float64);    

  ASSERT_TRUE(serialboxMetainfoAddString(metaInfo, "string", "str"));
  EXPECT_TRUE(serialboxMetainfoHasKey(metaInfo, "string"));
  EXPECT_EQ(serialboxMetainfoGetTypeIDOfKey(metaInfo, "string"), String);    

  ASSERT_EQ(serialboxMetainfoGetSize(metaInfo), 6);

  //
  // Add existing key/value pair -> False
  //
  ASSERT_FALSE(serialboxMetainfoAddBoolean(metaInfo, "bool", true));

  //
  // Add key/arrays
  //
  auto* arrayOfBooleanRef = serialboxArrayOfBooleanCreate(2);
  arrayOfBooleanRef->data[0] = true;
  arrayOfBooleanRef->data[1] = false;
  ASSERT_TRUE(serialboxMetainfoAddArrayOfBoolean(metaInfo, "ArrayOfBoolean", arrayOfBooleanRef));
  
  auto* arrayOfInt32Ref = serialboxArrayOfInt32Create(2);
  arrayOfInt32Ref->data[0] = 1;
  arrayOfInt32Ref->data[1] = 2;
  ASSERT_TRUE(serialboxMetainfoAddArrayOfInt32(metaInfo, "ArrayOfInt32", arrayOfInt32Ref));
  
  auto* arrayOfInt64Ref = serialboxArrayOfInt64Create(2);
  arrayOfInt64Ref->data[0] = 3;
  arrayOfInt64Ref->data[1] = 4;
  ASSERT_TRUE(serialboxMetainfoAddArrayOfInt64(metaInfo, "ArrayOfInt64", arrayOfInt64Ref));
  
  auto* arrayOfFloat32Ref = serialboxArrayOfFloat32Create(2);
  arrayOfFloat32Ref->data[0] = 1.1f;
  arrayOfFloat32Ref->data[1] = 1.2f;
  ASSERT_TRUE(serialboxMetainfoAddArrayOfFloat32(metaInfo, "ArrayOfFloat32", arrayOfFloat32Ref));
  
  auto* arrayOfFloat64Ref = serialboxArrayOfFloat64Create(2);
  arrayOfFloat64Ref->data[0] = 4.1;
  arrayOfFloat64Ref->data[1] = 3.2;
  ASSERT_TRUE(serialboxMetainfoAddArrayOfFloat64(metaInfo, "ArrayOfFloat64", arrayOfFloat64Ref));
  
  auto* arrayOfStringRef = serialboxArrayOfStringCreate(2);
  arrayOfStringRef->data[0] = allocateAndCopyString(std::string("str1"));
  arrayOfStringRef->data[1] = allocateAndCopyString(std::string("str2"));
  ASSERT_TRUE(serialboxMetainfoAddArrayOfString(metaInfo, "ArrayOfString", arrayOfStringRef));

  //
  // Query values
  //
  ASSERT_EQ(serialboxMetainfoGetBoolean(metaInfo, "bool"), true);
  ASSERT_EQ(serialboxMetainfoGetInt32(metaInfo, "int32"), 2);
  ASSERT_EQ(serialboxMetainfoGetInt64(metaInfo, "int64"), 2);
  ASSERT_EQ(serialboxMetainfoGetFloat32(metaInfo, "float32"), float(1.1f));
  ASSERT_EQ(serialboxMetainfoGetFloat64(metaInfo, "float64"), double(1.1));
  ASSERT_STREQ(serialboxMetainfoGetString(metaInfo, "string"), "str");

  // Key does not exists -> FatalError
  serialboxMetainfoGetBoolean(metaInfo, "bool-XXX");
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  
  serialboxMetainfoGetTypeIDOfKey(metaInfo, "bool-XXX");
  ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  //
  // Query arrays
  //
  auto* arrayOfBoolean = serialboxMetainfoGetArrayOfBoolean(metaInfo, "ArrayOfBoolean");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();  
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfBoolean, arrayOfBooleanRef));
  serialboxArrayOfBooleanDestroy(arrayOfBoolean);
  serialboxArrayOfBooleanDestroy(arrayOfBooleanRef);
  
  auto* arrayOfInt32 = serialboxMetainfoGetArrayOfInt32(metaInfo, "ArrayOfInt32");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();  
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfInt32, arrayOfInt32Ref));
  serialboxArrayOfInt32Destroy(arrayOfInt32);
  serialboxArrayOfInt32Destroy(arrayOfInt32Ref);

  auto* arrayOfInt64 = serialboxMetainfoGetArrayOfInt64(metaInfo, "ArrayOfInt64");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();  
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfInt64, arrayOfInt64Ref));
  serialboxArrayOfInt64Destroy(arrayOfInt64);
  serialboxArrayOfInt64Destroy(arrayOfInt64Ref);
  
  auto* arrayOfFloat32 = serialboxMetainfoGetArrayOfFloat32(metaInfo, "ArrayOfFloat32");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();    
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfFloat32, arrayOfFloat32Ref));
  serialboxArrayOfFloat32Destroy(arrayOfFloat32);
  serialboxArrayOfFloat32Destroy(arrayOfFloat32Ref);
  
  auto* arrayOfFloat64 = serialboxMetainfoGetArrayOfFloat64(metaInfo, "ArrayOfFloat64");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();      
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfFloat64, arrayOfFloat64Ref));
  serialboxArrayOfFloat64Destroy(arrayOfFloat64);
  serialboxArrayOfFloat64Destroy(arrayOfFloat64Ref);

  auto* arrayOfString = serialboxMetainfoGetArrayOfString(metaInfo, "ArrayOfString");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();      
  ASSERT_TRUE(internal::arraysAreEqual(arrayOfString, arrayOfStringRef));
  serialboxArrayOfStringDestroy(arrayOfString);
  serialboxArrayOfStringDestroy(arrayOfStringRef);

  //
  // ToString
  //
  serialboxMetainfo_t* metaInfo2 = serialboxMetainfoCreate();
  serialboxMetainfoToString(metaInfo2);
  ASSERT_TRUE(serialboxMetainfoAddBoolean(metaInfo2, "key", true));

  const char* strBuffer = serialboxMetainfoToString(metaInfo2);
  std::string str(strBuffer);
  std::free((void*)strBuffer);

  EXPECT_NE(str.find("key"), std::string::npos);
  EXPECT_NE(str.find("true"), std::string::npos);

  //
  // ElementInfo
  //
  serialboxMetainfoElementInfo_t* elements = serialboxMetainfoCreateElementInfo(metaInfo2);

  ASSERT_EQ(elements->len, 1);
  ASSERT_STREQ(elements->keys[0], "key");
  ASSERT_EQ(elements->types[0], Boolean);

  serialboxMetainfoDestroyElementInfo(elements);  

  //
  // Copy constructor 
  //
  serialboxMetainfo_t* metaInfoCopy = serialboxMetainfoCreateFromMetainfo(metaInfo);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();  
  ASSERT_TRUE(serialboxMetainfoEqual(metaInfo, metaInfoCopy));

  //
  // Clear map
  //
  serialboxMetainfoClear(metaInfo);
  ASSERT_TRUE(serialboxMetainfoIsEmpty(metaInfo));
  ASSERT_FALSE(serialboxMetainfoIsEmpty(metaInfoCopy)); // no aliasing
  
  //
  // Release memory
  //
  serialboxMetainfoDestroy(metaInfo);
  serialboxMetainfoDestroy(metaInfo2);
  serialboxMetainfoDestroy(metaInfoCopy);
  
}
