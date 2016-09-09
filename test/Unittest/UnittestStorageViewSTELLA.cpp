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
#include "serialbox/Core/STELLA/StorageView.h"
#include "serialbox/Core/StorageView.h"
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

  // -----------------------------------------------------------------------------------------------
  // Boundaries
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<IJBoundary> ij_boundary;
  std::unique_ptr<KBoundary> k_boundary;

  // -----------------------------------------------------------------------------------------------
  // Calculation Domain
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<IJKSize> ijk_calculationDomain;

  // -----------------------------------------------------------------------------------------------
  // Fields
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<IJKRealField> ijk_field;

  std::unique_ptr<IJRealField> ij_field;
  std::unique_ptr<IKRealField> ik_field;
  std::unique_ptr<JKRealField> jk_field;

  std::unique_ptr<IRealField> i_field;
  std::unique_ptr<JRealField> j_field;
  std::unique_ptr<KRealField> k_field;

protected:
  virtual void SetUp() override {}

  virtual void TearDown() override {}
};

} // anonymous namespace

TEST_F(StorageViewSTELLATest, Construction) {}

TEST_F(StorageViewSTELLATest, Iterator) {}

#endif
