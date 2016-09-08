//===-- Unittest/UnittestStorageViewGridTools.cpp -----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the StorageView interoperating with GridTools.
///
//===------------------------------------------------------------------------------------------===//

#include "STELLA.h"
#include "serialbox/Core/StorageView.h"
#include "serialbox/Core/STELLA/StorageView.h"
#include <gtest/gtest.h>
#include <memory>

#ifdef SERIALBOX_HAS_STELLA

namespace {

class StorageViewSTELLATest : public testing::Test {
public:
  // -----------------------------------------------------------------------------------------------
  // Dimensions
  // -----------------------------------------------------------------------------------------------
  int dim1;
  int dim2;
  int dim3;
  int dim4;

protected:
  virtual void SetUp() override {
  }

  virtual void TearDown() override {}
};

} // anonymous namespace

TEST_F(StorageViewSTELLATest, Construction) {}

TEST_F(StorageViewSTELLATest, Iterator) {}

#endif
