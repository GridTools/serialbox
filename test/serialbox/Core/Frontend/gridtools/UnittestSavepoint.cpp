//===-- serialbox/core/UnittestSavepoint.cpp ----------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This implements the unittests of the gridtools::Savepoint.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/frontend/gridtools/Savepoint.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace gridtools;

TEST(GridToolsSavepointTest, Construction) {
  meta_info_map map{{"key1", meta_info_value(double(4))}, {"key2", meta_info_value(int(2))}};
  
  // Default constructor
  savepoint sp1("s1");
  EXPECT_EQ(sp1.name(), "s1");
  
  // Construct with meta-info
  savepoint sp2("s2", map);
  EXPECT_EQ(sp2.name(), "s2");
  EXPECT_TRUE(sp2.meta_info() == map);
  
  // Construct with meta-info (intializer list)
  savepoint sp3("s3", {{"key1", meta_info_value(double(4))}, {"key2", meta_info_value(int(2))}});
  EXPECT_EQ(sp3.name(), "s3");
  EXPECT_TRUE(sp3.meta_info().has_key("key1"));
  EXPECT_TRUE(sp3.meta_info().has_key("key2"));  

  // Check aliasing
  savepoint sp_alias_of_sp1(sp1);
  sp_alias_of_sp1.add_meta_info("int", 5);
  ASSERT_TRUE(sp1.meta_info().has_key("int"));
  EXPECT_EQ(sp1.meta_info().as<int>("int"), 5);
  
  // Check cloning
  savepoint sp_clone_of_sp2(sp2.clone());  
  sp_clone_of_sp2.meta_info().insert("int", 5);
  ASSERT_FALSE(sp2.meta_info().has_key("int"));
  
  // Swap
  savepoint sp_swap = sp3.clone();
  sp_swap.meta_info().clear();
  sp_swap.swap(sp3);
  ASSERT_TRUE(sp3.meta_info().empty());

  // Comparison
  savepoint sp_eq = sp2.clone();
  savepoint sp_ne = sp2.clone();
  sp_ne.meta_info().insert("k", 5);
  
  ASSERT_EQ(sp_eq, sp2);
  ASSERT_NE(sp_ne, sp2);
}
