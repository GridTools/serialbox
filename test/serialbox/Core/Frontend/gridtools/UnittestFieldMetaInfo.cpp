//===-- serialbox/Core/UnittestFieldMetaInfo.cpp ------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This implements the unittests of the FieldMetaInfo.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Frontend/gridtools/FieldMetaInfo.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace gridtools;

TEST(GridToolsFieldMetaInfoTest, Construction) {
  TypeID type(TypeID::Float64);
  std::vector<int> dims{20, 15, 20};
  meta_info_map map{{"key1", meta_info_value(double(4))}, {"key2", meta_info_value(int(2))}};
  
  // Default constructor
  field_meta_info info1;
  EXPECT_TRUE(info1.dims().empty());
  EXPECT_EQ(info1.type(), TypeID::Invalid);
  
  // Construct with type and dims
  field_meta_info info2(type, dims);
  EXPECT_EQ(dims, info2.dims());
  EXPECT_EQ(info2.type(), type);
  
  // Construct with type, dims and meta_info
  field_meta_info info3(type, dims, map);
  EXPECT_EQ(info3.dims(), dims);
  EXPECT_EQ(info3.type(), type);
  ASSERT_EQ(info3.meta_info().size(), map.size());
  ASSERT_EQ(info3.meta_info().at("key1").as<double>(), 4);
  ASSERT_EQ(info3.meta_info().at("key2").as<int>(), 2);
}
