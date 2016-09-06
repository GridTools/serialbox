//===-- Unittest/UnittestStorageView.cpp --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the dummy Storage.
///
//===------------------------------------------------------------------------------------------===//

#include "Storage.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

namespace {

class StorageTest : public testing::Test {
public:
  // Dimensions
  int dim1;
  int dim2;
  int dim3;

  // Padding
  int pad1_left;
  int pad1_right;

  int pad2_left;
  int pad2_right;

  int pad3_left;
  int pad3_right;

  // --- 1D ---
  std::shared_ptr<Storage<double>> storage_1d;
  std::shared_ptr<Storage<double>> storage_1d_padded;

  // --- 2D ---
  std::shared_ptr<Storage<double>> storage_2d_col_major;
  std::shared_ptr<Storage<double>> storage_2d_col_major_padded;

  std::shared_ptr<Storage<double>> storage_2d_row_major;
  std::shared_ptr<Storage<double>> storage_2d_row_major_padded;

  // --- 3D ---
  std::shared_ptr<Storage<double>> storage_3d_col_major;
  std::shared_ptr<Storage<double>> storage_3d_col_major_padded;

  std::shared_ptr<Storage<double>> storage_3d_row_major;
  std::shared_ptr<Storage<double>> storage_3d_row_major_padded;

protected:
  virtual void SetUp() override {
    using il = std::initializer_list<int>;
    using ipl = std::initializer_list<std::pair<int, int>>;

    // Dimensions
    dim1 = 2;
    dim2 = 3;
    dim3 = 4;

    // Padding
    pad1_left = 1;
    pad1_right = 2;

    pad2_left = 3;
    pad2_right = 1;

    pad3_left = 0;
    pad3_right = 1;
    
    auto colMajor = Storage<double>::ColMajor;
    auto rowMajor = Storage<double>::RowMajor;

    // --- 1D ---
    storage_1d = std::make_shared<Storage<double>>(colMajor, il{dim1});
    storage_1d_padded = std::make_shared<Storage<double>>(
        colMajor, il{dim1}, ipl{std::pair<int, int>(pad1_left, pad1_right)});

    // --- 2D ---
    storage_2d_col_major = std::make_shared<Storage<double>>(colMajor, il{dim1, dim2});
    storage_2d_col_major_padded = std::make_shared<Storage<double>>(
        colMajor, il{dim1, dim2}, ipl{std::pair<int, int>(pad1_left, pad1_right),
                                      std::pair<int, int>(pad2_left, pad2_right)});

    storage_2d_row_major = std::make_shared<Storage<double>>(rowMajor, il{dim1, dim2});
    storage_2d_row_major_padded = std::make_shared<Storage<double>>(
        rowMajor, il{dim1, dim2}, ipl{std::pair<int, int>(pad1_left, pad1_right),
                                      std::pair<int, int>(pad2_left, pad2_right)});

    // --- 3D ---
    storage_3d_col_major = std::make_shared<Storage<double>>(colMajor, il{dim1, dim2, dim3});
    storage_3d_col_major_padded = std::make_shared<Storage<double>>(
        colMajor, il{dim1, dim2, dim3},
        ipl{std::pair<int, int>(pad1_left, pad1_right), std::pair<int, int>(pad2_left, pad2_right),
            std::pair<int, int>(pad3_left, pad3_right)});

    storage_3d_row_major = std::make_shared<Storage<double>>(rowMajor, il{dim1, dim2, dim3});
    storage_3d_row_major_padded = std::make_shared<Storage<double>>(
        rowMajor, il{dim1, dim2, dim3},
        ipl{std::pair<int, int>(pad1_left, pad1_right), std::pair<int, int>(pad2_left, pad2_right),
            std::pair<int, int>(pad3_left, pad3_right)});
  }

  virtual void TearDown() override {}
};

} // anonymous namespace

TEST_F(StorageTest, Dimension) {

  // --- 1D ---
  EXPECT_EQ(storage_1d->dims[0], dim1);
  EXPECT_EQ(storage_1d_padded->dims[0], dim1);

  // --- 2D ---
  EXPECT_EQ(storage_2d_row_major->dims[0], dim1);
  EXPECT_EQ(storage_2d_row_major->dims[1], dim2);

  EXPECT_EQ(storage_2d_col_major->dims[0], dim1);
  EXPECT_EQ(storage_2d_col_major->dims[1], dim2);

  EXPECT_EQ(storage_2d_col_major_padded->dims[0], dim1);
  EXPECT_EQ(storage_2d_col_major_padded->dims[1], dim2);

  EXPECT_EQ(storage_2d_row_major_padded->dims[0], dim1);
  EXPECT_EQ(storage_2d_row_major_padded->dims[1], dim2);

  // --- 3D ---
  EXPECT_EQ(storage_3d_row_major->dims[0], dim1);
  EXPECT_EQ(storage_3d_row_major->dims[1], dim2);
  EXPECT_EQ(storage_3d_row_major->dims[2], dim3);

  EXPECT_EQ(storage_3d_col_major->dims[0], dim1);
  EXPECT_EQ(storage_3d_col_major->dims[1], dim2);
  EXPECT_EQ(storage_3d_col_major->dims[2], dim3);

  EXPECT_EQ(storage_3d_col_major_padded->dims[0], dim1);
  EXPECT_EQ(storage_3d_col_major_padded->dims[1], dim2);
  EXPECT_EQ(storage_3d_col_major_padded->dims[2], dim3);

  EXPECT_EQ(storage_3d_row_major_padded->dims[0], dim1);
  EXPECT_EQ(storage_3d_row_major_padded->dims[1], dim2);
  EXPECT_EQ(storage_3d_row_major_padded->dims[2], dim3);
}

TEST_F(StorageTest, Strides) {
  // --- 1D ---
  EXPECT_EQ(storage_1d->strides[0], 1);
  EXPECT_EQ(storage_1d_padded->strides[0], 1);

  // --- 2D ---
  EXPECT_EQ(storage_2d_row_major->strides[0], dim2);
  EXPECT_EQ(storage_2d_row_major->strides[1], 1);

  EXPECT_EQ(storage_2d_row_major_padded->strides[0], dim2 + pad2_left + pad2_right);
  EXPECT_EQ(storage_2d_row_major_padded->strides[1], 1);

  EXPECT_EQ(storage_2d_col_major->strides[0], 1);
  EXPECT_EQ(storage_2d_col_major->strides[1], dim1);

  EXPECT_EQ(storage_2d_col_major_padded->strides[0], 1);
  EXPECT_EQ(storage_2d_col_major_padded->strides[1], dim1 + pad1_left + pad1_right);

  // --- 3D ---
  EXPECT_EQ(storage_3d_row_major->strides[0], dim3 * dim2);
  EXPECT_EQ(storage_3d_row_major->strides[1], dim3);
  EXPECT_EQ(storage_3d_row_major->strides[2], 1);

  EXPECT_EQ(storage_3d_row_major_padded->strides[0],
            (dim3 + pad3_left + pad3_right) * (dim2 + pad2_left + pad2_right));
  EXPECT_EQ(storage_3d_row_major_padded->strides[1], (dim3 + pad3_left + pad3_right));
  EXPECT_EQ(storage_3d_row_major_padded->strides[2], 1);

  EXPECT_EQ(storage_3d_col_major->strides[0], 1);
  EXPECT_EQ(storage_3d_col_major->strides[1], dim1);
  EXPECT_EQ(storage_3d_col_major->strides[2], dim1 * dim2);

  EXPECT_EQ(storage_3d_col_major_padded->strides[0], 1);
  EXPECT_EQ(storage_3d_col_major_padded->strides[1], (dim1 + pad1_left + pad1_right));
  EXPECT_EQ(storage_3d_col_major_padded->strides[2],
            (dim1 + pad1_left + pad1_right) * (dim2 + pad2_left + pad2_right));
}

TEST_F(StorageTest, Access) {
  // --- 1D ---
  EXPECT_DOUBLE_EQ(storage_1d->at({0}), 0);
  EXPECT_DOUBLE_EQ(storage_1d->at({dim1 - 1}), dim1 - 1);

  EXPECT_DOUBLE_EQ(storage_1d_padded->at({0}), pad1_left);
  EXPECT_DOUBLE_EQ(storage_1d_padded->at({dim1 - 1}), pad1_left + dim1 - 1);

  // --- 2D ---
  EXPECT_DOUBLE_EQ(storage_2d_col_major->at({0, 0}), 0);
  EXPECT_DOUBLE_EQ(storage_2d_col_major->at({1, 0}), 1);
  EXPECT_DOUBLE_EQ(storage_2d_col_major->at({dim1 - 1, dim2 - 1}), dim1 * dim2 - 1);
  EXPECT_DOUBLE_EQ(storage_2d_row_major->at({0, 0}), 0);
  EXPECT_DOUBLE_EQ(storage_2d_row_major->at({0, 1}), 1);
  EXPECT_DOUBLE_EQ(storage_2d_row_major->at({dim1 - 1, dim2 - 1}), dim1 * dim2 - 1);

  EXPECT_DOUBLE_EQ(storage_2d_col_major_padded->at({0, 0}),
                   (pad1_left + dim1 + pad1_right) * pad2_left + pad1_left);
  EXPECT_DOUBLE_EQ(storage_2d_row_major_padded->at({0, 0}),
                   (pad2_left + dim2 + pad2_right) * pad1_left + pad2_left);

  // --- 3D ---
  EXPECT_DOUBLE_EQ(storage_3d_col_major->at({0, 0, 0}), 0);
  EXPECT_DOUBLE_EQ(storage_3d_col_major->at({dim1 - 1, dim2 - 1, dim3 - 1}),
                   dim1 * dim2 * dim3 - 1);
  EXPECT_DOUBLE_EQ(storage_3d_row_major->at({0, 0, 0}), 0);
  EXPECT_DOUBLE_EQ(storage_3d_row_major->at({dim1 - 1, dim2 - 1, dim3 - 1}),
                   dim1 * dim2 * dim3 - 1);

  EXPECT_DOUBLE_EQ(storage_3d_col_major_padded->at({0, 0, 0}),
                   (pad1_left + dim1 + pad1_right) * (pad2_left + dim2 + pad2_right) * pad3_left +
                       ((pad1_left + dim1 + pad1_right) * pad2_left) + pad1_left);

  EXPECT_DOUBLE_EQ(storage_3d_row_major_padded->at({0, 0, 0}),
                   ((pad2_left + dim2 + pad2_right) * (pad3_left + dim3 + pad3_right) * pad1_left) +
                       (pad3_left + dim3 + pad3_right) * pad2_left + pad3_left);
}
