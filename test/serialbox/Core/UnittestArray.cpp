//===-- serialbox/Core/UnittestArray.cpp --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the Array unittests.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Array.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(ArrayTest, IsArray) {
  using T1 = Array<double>;
  static_assert(IsArray<T1>::value == true, "Array of doubles");

  using T2 = double;
  static_assert(IsArray<T2>::value == false, "Doubles");
}

TEST(ArrayTest, MakePrimitive) {
  using T1 = Array<double>;
  testing::StaticAssertTypeEq<MakePrimitive<T1>::type, double>();
  
  using T2 = double;
  testing::StaticAssertTypeEq<MakePrimitive<T2>::type, double>();
}
