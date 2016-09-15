//===-- Unittest/Cpp/UnittestStorageView.cpp ----------------------------------------*- C++ -*-===//
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
#include "serialbox/Core/STLExtras.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

namespace {

template <class T>
class StorageTest : public testing::Test {
public:
  // -----------------------------------------------------------------------------------------------
  // Dimensions
  // -----------------------------------------------------------------------------------------------
  int dim1;
  int dim2;
  int dim3;

  // -----------------------------------------------------------------------------------------------
  // Padding
  // -----------------------------------------------------------------------------------------------
  int pad1_left;
  int pad1_right;

  int pad2_left;
  int pad2_right;

  int pad3_left;
  int pad3_right;

  // -----------------------------------------------------------------------------------------------
  // 1D
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<Storage<T>> storage_1d;
  std::unique_ptr<Storage<T>> storage_1d_padded;

  // -----------------------------------------------------------------------------------------------
  // 2D
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<Storage<T>> storage_2d_col_major;
  std::unique_ptr<Storage<T>> storage_2d_col_major_padded;

  std::unique_ptr<Storage<T>> storage_2d_row_major;
  std::unique_ptr<Storage<T>> storage_2d_row_major_padded;

  // -----------------------------------------------------------------------------------------------
  // 3D
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<Storage<T>> storage_3d_col_major;
  std::unique_ptr<Storage<T>> storage_3d_col_major_padded;

  std::unique_ptr<Storage<T>> storage_3d_row_major;
  std::unique_ptr<Storage<T>> storage_3d_row_major_padded;

protected:
  virtual void SetUp() override {
    dim1 = 2;
    dim2 = 3;
    dim3 = 4;

    pad1_left = 1;
    pad1_right = 2;

    pad2_left = 3;
    pad2_right = 1;

    pad3_left = 0;
    pad3_right = 1;

    auto colMajor = Storage<T>::ColMajor;
    auto rowMajor = Storage<T>::RowMajor;

    storage_1d = make_unique<Storage<T>>(colMajor, Dims{dim1});
    storage_1d_padded =
        make_unique<Storage<T>>(colMajor, Dims{dim1}, Padding{{pad1_left, pad1_right}});

    storage_2d_col_major = make_unique<Storage<T>>(colMajor, Dims{dim1, dim2});
    storage_2d_col_major_padded = make_unique<Storage<T>>(
        colMajor, Dims{dim1, dim2}, Padding{{pad1_left, pad1_right}, {pad2_left, pad2_right}});

    storage_2d_row_major = make_unique<Storage<T>>(rowMajor, Dims{dim1, dim2});
    storage_2d_row_major_padded = make_unique<Storage<T>>(
        rowMajor, Dims{dim1, dim2}, Padding{{pad1_left, pad1_right}, {pad2_left, pad2_right}});

    storage_3d_col_major = make_unique<Storage<T>>(colMajor, Dims{dim1, dim2, dim3});
    storage_3d_col_major_padded = make_unique<Storage<T>>(
        colMajor, Dims{dim1, dim2, dim3},
        Padding{{pad1_left, pad1_right}, {pad2_left, pad2_right}, {pad3_left, pad3_right}});

    storage_3d_row_major = make_unique<Storage<T>>(rowMajor, Dims{dim1, dim2, dim3});
    storage_3d_row_major_padded = make_unique<Storage<T>>(
        rowMajor, Dims{dim1, dim2, dim3},
        Padding{{pad1_left, pad1_right}, {pad2_left, pad2_right}, {pad3_left, pad3_right}});
  }

  virtual void TearDown() override {}
};

using TestTypes = testing::Types<double, float, int, std::int64_t>;

} // anonymous namespace

TYPED_TEST_CASE(StorageTest, TestTypes);

TYPED_TEST(StorageTest, Dimension) {
  int dim1 = this->dim1, dim2 = this->dim2, dim3 = this->dim3;

  // -----------------------------------------------------------------------------------------------
  // 1D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_1d->dims()[0], dim1);
  EXPECT_EQ(this->storage_1d_padded->dims()[0], dim1);

  // -----------------------------------------------------------------------------------------------
  // 2D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_2d_row_major->dims()[0], dim1);
  EXPECT_EQ(this->storage_2d_row_major->dims()[1], dim2);

  EXPECT_EQ(this->storage_2d_col_major->dims()[0], dim1);
  EXPECT_EQ(this->storage_2d_col_major->dims()[1], dim2);

  EXPECT_EQ(this->storage_2d_col_major_padded->dims()[0], dim1);
  EXPECT_EQ(this->storage_2d_col_major_padded->dims()[1], dim2);

  EXPECT_EQ(this->storage_2d_row_major_padded->dims()[0], dim1);
  EXPECT_EQ(this->storage_2d_row_major_padded->dims()[1], dim2);

  // -----------------------------------------------------------------------------------------------
  // 3D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_3d_row_major->dims()[0], dim1);
  EXPECT_EQ(this->storage_3d_row_major->dims()[1], dim2);
  EXPECT_EQ(this->storage_3d_row_major->dims()[2], dim3);

  EXPECT_EQ(this->storage_3d_col_major->dims()[0], dim1);
  EXPECT_EQ(this->storage_3d_col_major->dims()[1], dim2);
  EXPECT_EQ(this->storage_3d_col_major->dims()[2], dim3);

  EXPECT_EQ(this->storage_3d_col_major_padded->dims()[0], dim1);
  EXPECT_EQ(this->storage_3d_col_major_padded->dims()[1], dim2);
  EXPECT_EQ(this->storage_3d_col_major_padded->dims()[2], dim3);

  EXPECT_EQ(this->storage_3d_row_major_padded->dims()[0], dim1);
  EXPECT_EQ(this->storage_3d_row_major_padded->dims()[1], dim2);
  EXPECT_EQ(this->storage_3d_row_major_padded->dims()[2], dim3);
}

TYPED_TEST(StorageTest, Strides) {
  int dim1 = this->dim1, dim2 = this->dim2, dim3 = this->dim3;
  int pad1_left = this->pad1_left, pad2_left = this->pad2_left, pad3_left = this->pad3_left;
  int pad1_right = this->pad1_right, pad2_right = this->pad2_right, pad3_right = this->pad3_right;

  // -----------------------------------------------------------------------------------------------
  // 1D
  // ------------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_1d->strides()[0], 1);
  EXPECT_EQ(this->storage_1d_padded->strides()[0], 1);

  // -----------------------------------------------------------------------------------------------
  // 2D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_2d_row_major->strides()[0], dim2);
  EXPECT_EQ(this->storage_2d_row_major->strides()[1], 1);

  EXPECT_EQ(this->storage_2d_row_major_padded->strides()[0], dim2 + pad2_left + pad2_right);
  EXPECT_EQ(this->storage_2d_row_major_padded->strides()[1], 1);

  EXPECT_EQ(this->storage_2d_col_major->strides()[0], 1);
  EXPECT_EQ(this->storage_2d_col_major->strides()[1], dim1);

  EXPECT_EQ(this->storage_2d_col_major_padded->strides()[0], 1);
  EXPECT_EQ(this->storage_2d_col_major_padded->strides()[1], dim1 + pad1_left + pad1_right);

  // -----------------------------------------------------------------------------------------------
  // 3D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_3d_row_major->strides()[0], dim3 * dim2);
  EXPECT_EQ(this->storage_3d_row_major->strides()[1], dim3);
  EXPECT_EQ(this->storage_3d_row_major->strides()[2], 1);

  EXPECT_EQ(this->storage_3d_row_major_padded->strides()[0],
            (dim3 + pad3_left + pad3_right) * (dim2 + pad2_left + pad2_right));
  EXPECT_EQ(this->storage_3d_row_major_padded->strides()[1], (dim3 + pad3_left + pad3_right));
  EXPECT_EQ(this->storage_3d_row_major_padded->strides()[2], 1);

  EXPECT_EQ(this->storage_3d_col_major->strides()[0], 1);
  EXPECT_EQ(this->storage_3d_col_major->strides()[1], dim1);
  EXPECT_EQ(this->storage_3d_col_major->strides()[2], dim1 * dim2);

  EXPECT_EQ(this->storage_3d_col_major_padded->strides()[0], 1);
  EXPECT_EQ(this->storage_3d_col_major_padded->strides()[1], (dim1 + pad1_left + pad1_right));
  EXPECT_EQ(this->storage_3d_col_major_padded->strides()[2],
            (dim1 + pad1_left + pad1_right) * (dim2 + pad2_left + pad2_right));
}

TYPED_TEST(StorageTest, Access) {
  int dim1 = this->dim1, dim2 = this->dim2, dim3 = this->dim3;
  int pad1_left = this->pad1_left, pad2_left = this->pad2_left, pad3_left = this->pad3_left;
  int pad1_right = this->pad1_right, pad2_right = this->pad2_right, pad3_right = this->pad3_right;
  // -----------------------------------------------------------------------------------------------
  // 1D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_1d->at(0), 0);
  EXPECT_EQ(this->storage_1d->at(dim1 - 1), dim1 - 1);

  EXPECT_EQ(this->storage_1d_padded->at(0), pad1_left);
  EXPECT_EQ(this->storage_1d_padded->at(dim1 - 1), pad1_left + dim1 - 1);

  // -----------------------------------------------------------------------------------------------
  // 2D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_2d_col_major->at(0, 0), 0);
  EXPECT_EQ(this->storage_2d_col_major->at(1, 0), 1);
  EXPECT_EQ(this->storage_2d_col_major->at(dim1 - 1, dim2 - 1), dim1 * dim2 - 1);
  EXPECT_EQ(this->storage_2d_row_major->at(0, 0), 0);
  EXPECT_EQ(this->storage_2d_row_major->at(0, 1), 1);
  EXPECT_EQ(this->storage_2d_row_major->at(dim1 - 1, dim2 - 1), dim1 * dim2 - 1);

  EXPECT_EQ(this->storage_2d_col_major_padded->at(0, 0),
            (pad1_left + dim1 + pad1_right) * pad2_left + pad1_left);
  EXPECT_EQ(this->storage_2d_row_major_padded->at(0, 0),
            (pad2_left + dim2 + pad2_right) * pad1_left + pad2_left);

  // -----------------------------------------------------------------------------------------------
  // 3D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(this->storage_3d_col_major->at(0, 0, 0), 0);
  EXPECT_EQ(this->storage_3d_col_major->at(dim1 - 1, dim2 - 1, dim3 - 1), dim1 * dim2 * dim3 - 1);
  EXPECT_EQ(this->storage_3d_row_major->at(0, 0, 0), 0);
  EXPECT_EQ(this->storage_3d_row_major->at(dim1 - 1, dim2 - 1, dim3 - 1), dim1 * dim2 * dim3 - 1);

  EXPECT_EQ(this->storage_3d_col_major_padded->at(0, 0, 0),
            (pad1_left + dim1 + pad1_right) * (pad2_left + dim2 + pad2_right) * pad3_left +
                ((pad1_left + dim1 + pad1_right) * pad2_left) + pad1_left);

  EXPECT_EQ(this->storage_3d_row_major_padded->at(0, 0, 0),
            ((pad2_left + dim2 + pad2_right) * (pad3_left + dim3 + pad3_right) * pad1_left) +
                (pad3_left + dim3 + pad3_right) * pad2_left + pad3_left);
}

TYPED_TEST(StorageTest, Verifictaion) {
  ASSERT_TRUE(Storage<TypeParam>::verify(*this->storage_1d, *this->storage_1d));
  ASSERT_TRUE(Storage<TypeParam>::verify(*this->storage_2d_col_major_padded,
                                         *this->storage_2d_col_major_padded));
  ASSERT_TRUE(Storage<TypeParam>::verify(*this->storage_3d_col_major_padded,
                                         *this->storage_3d_col_major_padded));

  Storage<TypeParam> storage_1d_copy(*this->storage_1d);
  ASSERT_TRUE(Storage<TypeParam>::verify(storage_1d_copy, *this->storage_1d));
  storage_1d_copy(this->dim1 - 1) = 151234;
  ASSERT_FALSE(Storage<TypeParam>::verify(storage_1d_copy, *this->storage_1d));

  Storage<TypeParam> storage_3d_copy(*this->storage_3d_col_major_padded);
  ASSERT_TRUE(Storage<TypeParam>::verify(storage_3d_copy, *this->storage_3d_col_major_padded));
  storage_3d_copy(1, 1, 2) = 125251;
  ASSERT_FALSE(Storage<TypeParam>::verify(storage_3d_copy, *this->storage_3d_col_major_padded));
}
