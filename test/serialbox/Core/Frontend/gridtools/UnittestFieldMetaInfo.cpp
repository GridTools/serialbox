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
/// This implements the unittests of the gridtools::field_meta_info.
///
//===------------------------------------------------------------------------------------------===//

#include "utility/GridTools.h"
#include "serialbox/Core/Frontend/gridtools/FieldMetaInfo.h"
#include <gtest/gtest.h>

namespace ser = serialbox::gridtools;

TEST(GridToolsFieldMetaInfoTest, Construction) {
  serialbox::TypeID type(ser::type_id::Float64);
  std::vector<int> dims{20, 15, 20};
  ser::meta_info_map map{{"key1", ser::meta_info_value(double(4))},
                         {"key2", ser::meta_info_value(int(2))}};

  // Default constructor
  ser::field_meta_info info1;
  EXPECT_TRUE(info1.dims().empty());
  EXPECT_EQ(info1.type(), ser::type_id::Invalid);

  // Construct with type and dims
  ser::field_meta_info info2(type, dims);
  EXPECT_EQ(dims, info2.dims());
  EXPECT_EQ(info2.type(), type);

  // Construct with type, dims and meta_info
  ser::field_meta_info info3(type, dims, map);
  EXPECT_EQ(info3.dims(), dims);
  EXPECT_EQ(info3.type(), type);
  ASSERT_EQ(info3.meta_info().size(), map.size());
  ASSERT_EQ(info3.meta_info().at("key1").as<double>(), 4);
  ASSERT_EQ(info3.meta_info().at("key2").as<int>(), 2);

  // Check aliasing
  ser::field_meta_info info_alias_of_1(info1);
  info_alias_of_1.meta_info().insert("int", 5);
  ASSERT_TRUE(info1.meta_info().has_key("int"));
  EXPECT_EQ(info1.meta_info().as<int>("int"), 5);

  // Check cloning
  ser::field_meta_info info_clone_of_2(info2.clone());
  info_clone_of_2.meta_info().insert("int", 5);
  ASSERT_FALSE(info2.meta_info().has_key("int"));

  // Swap
  ser::field_meta_info info_swap = info2.clone();
  info_swap.meta_info().clear();
  info2.swap(info_swap);
  ASSERT_TRUE(info2.meta_info().empty());

  // Comparison
  ser::field_meta_info info_eq = info3.clone();
  ser::field_meta_info info_ne = info3.clone();
  info_ne.meta_info().insert("k", 5);

  ASSERT_EQ(info_eq, info3);
  ASSERT_NE(info_ne, info3);
}

#ifdef SERIALBOX_HAS_GRIDTOOLS

TEST(GridToolsFieldMetaInfoTest, ConstructionWithGridToolsStorage) {
  using types = serialbox::unittest::gridtools_storage_types<double>;

  types::cpu_3d_meta_data_type meta_data(4, 5, 6);
  types::cpu_3d_storage_type storage(meta_data, "storage", -1.0);

  ser::field_meta_info info(storage);
  ASSERT_EQ(info.type(), serialbox::TypeID::Float64);
  ASSERT_EQ(info.dims(), (std::vector<int>{4, 5, 6}));
}

#endif
