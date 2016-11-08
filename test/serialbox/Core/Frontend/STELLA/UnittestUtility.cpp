//===-- serialbox/core/frontend/stella/UnittestUtility.cpp --------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the STELLA utitlity functions.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/frontend/stella/Utility.h"
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>
#include <gtest/gtest.h>
#include <memory>

using namespace serialbox;

TEST(STELLAUtility, BoostToStdSharedPointerConversion) {

  std::shared_ptr<int> std_ptr = std::make_shared<int>(5);
  ASSERT_EQ(std_ptr.use_count(), 1);

  boost::shared_ptr<int> boost_ptr = boost::make_shared<int>(5);
  ASSERT_EQ(boost_ptr.use_count(), 1);

  // std -> boost
  {
    {
      // Create new shared user
      boost::shared_ptr<int> ptr = stella::internal::make_shared_ptr(std_ptr);
      EXPECT_EQ(std_ptr.use_count(), 2);
      EXPECT_EQ(*ptr, 5);

      // Release original pointer
      std_ptr.reset();
      EXPECT_EQ(ptr.use_count(), 1);
      EXPECT_EQ(*ptr, 5);
    }
    EXPECT_EQ(std_ptr.use_count(), 0);
  }

  // boost -> std
  {
    {
      // Create new shared user      
      std::shared_ptr<int> ptr = stella::internal::make_shared_ptr(boost_ptr);
      EXPECT_EQ(boost_ptr.use_count(), 2);

      // Release original pointer      
      boost_ptr.reset();
      EXPECT_EQ(ptr.use_count(), 1);
      EXPECT_EQ(*ptr, 5);
    }
    EXPECT_EQ(boost_ptr.use_count(), 0);
  }
}
