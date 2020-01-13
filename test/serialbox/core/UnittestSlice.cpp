//===-- serialbox/core/UnittestSlice.cpp --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the slicing.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/Slice.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(SliceTest, Construction) {
  //
  // Empty
  //
  Slice slice0((Slice::Empty()));
  ASSERT_TRUE(slice0.sliceTriples().empty());

  //
  // Python [:]
  //
  Slice slice1;

  ASSERT_EQ(slice1.sliceTriples().size(), 1);
  ASSERT_EQ(slice1.sliceTriples()[0].start, 0);
  ASSERT_EQ(slice1.sliceTriples()[0].stop, -1);
  ASSERT_EQ(slice1.sliceTriples()[0].step, 1);

  //
  // Python [0:10:2]
  //
  Slice slice2(0, 10, 2);
  ASSERT_EQ(slice2.sliceTriples().size(), 1);
  ASSERT_EQ(slice2.sliceTriples()[0].start, 0);
  ASSERT_EQ(slice2.sliceTriples()[0].stop, 10);
  ASSERT_EQ(slice2.sliceTriples()[0].step, 2);

  //
  // Python [:][1:10][:][1:2:1]
  //
  Slice slice3 = Slice()(1, 10)()(1, 2, 1);
  ASSERT_EQ(slice3.sliceTriples().size(), 4);
  
  ASSERT_EQ(slice3.sliceTriples()[0].start, 0);
  ASSERT_EQ(slice3.sliceTriples()[0].stop, -1);
  ASSERT_EQ(slice3.sliceTriples()[0].step, 1);
  
  ASSERT_EQ(slice3.sliceTriples()[1].start, 1);
  ASSERT_EQ(slice3.sliceTriples()[1].stop, 10);
  ASSERT_EQ(slice3.sliceTriples()[1].step, 1);
  
  ASSERT_EQ(slice3.sliceTriples()[2].start, 0);
  ASSERT_EQ(slice3.sliceTriples()[2].stop, -1);
  ASSERT_EQ(slice3.sliceTriples()[2].step, 1);
  
  ASSERT_EQ(slice3.sliceTriples()[3].start, 1);
  ASSERT_EQ(slice3.sliceTriples()[3].stop, 2);
  ASSERT_EQ(slice3.sliceTriples()[3].step, 1);
}
