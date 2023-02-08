//===-- serialbox/core/UnittestStorageView.cpp --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the StorageView and StorageViewIterator.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/Exception.h"
#include "serialbox/core/StorageView.h"
#include "serialbox/core/Type.h"
#include "utility/Storage.h"
#include <boost/algorithm/string.hpp>
#include <cstring>
#include <gtest/gtest.h>
#include <numeric>

using namespace serialbox;
using namespace unittest;

namespace {

template <class T>
class StorageViewTest : public testing::Test {
public:
  // -----------------------------------------------------------------------------------------------
  // Dimensions
  // -----------------------------------------------------------------------------------------------
  int dim1;
  int dim2;
  int dim3;
  int dim4;
  int dim5;

  // -----------------------------------------------------------------------------------------------
  // Padding
  // -----------------------------------------------------------------------------------------------
  int pad1_left;
  int pad1_right;

  int pad2_left;
  int pad2_right;

  int pad3_left;
  int pad3_right;

  int pad4_left;
  int pad4_right;

  int pad5_left;
  int pad5_right;

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

  // -----------------------------------------------------------------------------------------------
  // 5D
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<Storage<T>> storage_5d_col_major_padded;
  std::unique_ptr<Storage<T>> storage_5d_row_major_padded;

protected:
  virtual void SetUp() override {
    dim1 = 2;
    dim2 = 3;
    dim3 = 4;
    dim4 = 2;
    dim5 = 2;

    pad1_left = 1;
    pad1_right = 2;

    pad2_left = 3;
    pad2_right = 1;

    pad3_left = 0;
    pad3_right = 1;

    pad4_left = 0;
    pad4_right = 0;

    pad5_left = 1;
    pad5_right = 0;

    auto colMajor = Storage<T>::ColMajor;
    auto rowMajor = Storage<T>::RowMajor;

    storage_1d = std::make_unique<Storage<T>>(colMajor, Dims{dim1});
    storage_1d_padded =
        std::make_unique<Storage<T>>(colMajor, Dims{dim1}, Padding{{pad1_left, pad1_right}});

    storage_2d_col_major = std::make_unique<Storage<T>>(colMajor, Dims{dim1, dim2});
    storage_2d_col_major_padded = std::make_unique<Storage<T>>(
        colMajor, Dims{dim1, dim2}, Padding{{pad1_left, pad1_right}, {pad2_left, pad2_right}});
    ;

    storage_2d_row_major = std::make_unique<Storage<T>>(rowMajor, Dims{dim1, dim2});
    storage_2d_row_major_padded = std::make_unique<Storage<T>>(
        rowMajor, Dims{dim1, dim2}, Padding{{pad1_left, pad1_right}, {pad2_left, pad2_right}});

    storage_3d_col_major = std::make_unique<Storage<T>>(colMajor, Dims{dim1, dim2, dim3});
    storage_3d_col_major_padded = std::make_unique<Storage<T>>(
        colMajor, Dims{dim1, dim2, dim3},
        Padding{{pad1_left, pad1_right}, {pad2_left, pad2_right}, {pad3_left, pad3_right}});

    storage_3d_row_major = std::make_unique<Storage<T>>(rowMajor, Dims{dim1, dim2, dim3});
    storage_3d_row_major_padded = std::make_unique<Storage<T>>(
        rowMajor, Dims{dim1, dim2, dim3},
        Padding{{pad1_left, pad1_right}, {pad2_left, pad2_right}, {pad3_left, pad3_right}});

    storage_5d_col_major_padded =
        std::make_unique<Storage<T>>(colMajor, Dims{dim1, dim2, dim3, dim4, dim5},
                                     Padding{{pad1_left, pad1_right},
                                             {pad2_left, pad2_right},
                                             {pad3_left, pad3_right},
                                             {pad4_left, pad4_right},
                                             {pad5_left, pad5_right}});

    storage_5d_row_major_padded =
        std::make_unique<Storage<T>>(rowMajor, Dims{dim1, dim2, dim3, dim4, dim5},
                                     Padding{{pad1_left, pad1_right},
                                             {pad2_left, pad2_right},
                                             {pad3_left, pad3_right},
                                             {pad4_left, pad4_right},
                                             {pad5_left, pad5_right}});
  }

  virtual void TearDown() override {}
};

using TestTypes = testing::Types<double, float, int, std::int64_t>;

} // anonymous namespace

TYPED_TEST_CASE(StorageViewTest, TestTypes);

TYPED_TEST(StorageViewTest, Construction) {
  int dim1 = this->dim1, dim2 = this->dim2;

  // Constructor
  auto sv_1d = this->storage_1d->toStorageView();
  EXPECT_EQ(static_cast<void*>(this->storage_1d->originPtr()),
            static_cast<void*>(sv_1d.originPtr()));
  EXPECT_EQ(this->storage_1d->strides(), sv_1d.strides());
  EXPECT_EQ(this->storage_1d->dims(), sv_1d.dims());
  EXPECT_EQ(sv_1d.bytesPerElement(), sizeof(typename Storage<TypeParam>::value_type));
  EXPECT_EQ(sv_1d.size(), dim1);
  EXPECT_EQ(sv_1d.sizeInBytes(), sv_1d.bytesPerElement() * dim1);

  // Copy constructor
  auto sv_2d_col_major = this->storage_2d_col_major->toStorageView();

  StorageView copy_sv_1d(sv_1d);
  ASSERT_EQ(copy_sv_1d, sv_1d);

  StorageView copy_sv_2d(sv_2d_col_major);
  ASSERT_EQ(copy_sv_2d, sv_2d_col_major);

  EXPECT_EQ(sv_2d_col_major.size(), dim1 * dim2);
  EXPECT_EQ(sv_2d_col_major.sizeInBytes(), sv_2d_col_major.bytesPerElement() * (dim1 * dim2));

  // Swap
  sv_1d.swap(sv_2d_col_major);
  EXPECT_EQ(sv_1d, copy_sv_2d);
  EXPECT_EQ(sv_2d_col_major, copy_sv_1d);

  swap(sv_1d, sv_2d_col_major);
  EXPECT_EQ(sv_1d, copy_sv_1d);
  EXPECT_EQ(sv_2d_col_major, copy_sv_2d);

  // Copy assignment
  sv_1d = sv_2d_col_major;
  EXPECT_EQ(sv_1d, sv_2d_col_major);
}

TYPED_TEST(StorageViewTest, IteratorConstruction) {
  auto sv_1d = this->storage_1d->toStorageView();
  auto sv_1d_pad = this->storage_1d_padded->toStorageView();
  auto sv_2d_col_major = this->storage_2d_col_major->toStorageView();
  auto sv_2d_col_major_pad = this->storage_2d_col_major_padded->toStorageView();
  auto sv_2d_row_major = this->storage_2d_row_major->toStorageView();
  auto sv_2d_row_major_pad = this->storage_2d_row_major_padded->toStorageView();
  auto sv_3d_col_major = this->storage_3d_col_major->toStorageView();
  auto sv_3d_col_major_pad = this->storage_3d_col_major_padded->toStorageView();
  auto sv_3d_row_major = this->storage_3d_row_major->toStorageView();
  auto sv_3d_row_major_pad = this->storage_3d_row_major_padded->toStorageView();
  auto sv_5d_col_major_pad = this->storage_5d_col_major_padded->toStorageView();
  auto sv_5d_row_major_pad = this->storage_5d_row_major_padded->toStorageView();

  EXPECT_NE(sv_1d.begin(), sv_1d.end());
  EXPECT_EQ(sv_1d.begin(), sv_1d.begin());
  EXPECT_EQ(sv_1d.end(), sv_1d.end());
  EXPECT_NE(sv_1d.begin(), sv_2d_col_major.begin());

  // Check if begin() points to the beginning of the data i.e skips any padding

  // -----------------------------------------------------------------------------------------------
  // 1D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(static_cast<void*>(sv_1d.begin().ptr()), static_cast<void*>(sv_1d.originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_1d.begin().ptr()),
            static_cast<void*>(this->storage_1d->originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_1d_pad.begin().ptr()),
            static_cast<void*>(this->storage_1d_padded->originPtr()));

  // -----------------------------------------------------------------------------------------------
  // 2D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(static_cast<void*>(sv_2d_col_major.begin().ptr()),
            static_cast<void*>(sv_2d_col_major.originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_2d_col_major.begin().ptr()),
            static_cast<void*>(this->storage_2d_col_major->originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_2d_col_major_pad.begin().ptr()),
            static_cast<void*>(this->storage_2d_col_major_padded->originPtr()));

  EXPECT_EQ(static_cast<void*>(sv_2d_row_major.begin().ptr()),
            static_cast<void*>(sv_2d_row_major.originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_2d_row_major.begin().ptr()),
            static_cast<void*>(this->storage_2d_row_major->originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_2d_row_major_pad.begin().ptr()),
            static_cast<void*>(this->storage_2d_row_major_padded->originPtr()));

  // -----------------------------------------------------------------------------------------------
  // 3D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(static_cast<void*>(sv_3d_col_major.begin().ptr()),
            static_cast<void*>(sv_3d_col_major.originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_3d_col_major.begin().ptr()),
            static_cast<void*>(this->storage_3d_col_major->originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_3d_col_major_pad.begin().ptr()),
            static_cast<void*>(this->storage_3d_col_major_padded->originPtr()));

  EXPECT_EQ(static_cast<void*>(sv_3d_row_major.begin().ptr()),
            static_cast<void*>(sv_3d_row_major.originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_3d_row_major.begin().ptr()),
            static_cast<void*>(this->storage_3d_row_major->originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_3d_row_major_pad.begin().ptr()),
            static_cast<void*>(this->storage_3d_row_major_padded->originPtr()));

  // -----------------------------------------------------------------------------------------------
  // 5D
  // -----------------------------------------------------------------------------------------------
  EXPECT_EQ(static_cast<void*>(sv_5d_col_major_pad.begin().ptr()),
            static_cast<void*>(this->storage_5d_col_major_padded->originPtr()));
  EXPECT_EQ(static_cast<void*>(sv_5d_row_major_pad.begin().ptr()),
            static_cast<void*>(this->storage_5d_row_major_padded->originPtr()));

  // Copy constructor
  auto it_sv_1d(sv_1d.begin());
  auto copy_it_sv_1d(it_sv_1d);
  EXPECT_EQ(copy_it_sv_1d, sv_1d.begin());

  auto it_sv_2d(sv_2d_col_major.begin());
  auto copy_it_sv_2d(it_sv_2d);
  EXPECT_EQ(copy_it_sv_2d, sv_2d_col_major.begin());

  // Copy assignment
  auto copy_assign_it_sv_1d = it_sv_1d;
  EXPECT_EQ(copy_assign_it_sv_1d, sv_1d.begin());

  // Swap
  it_sv_2d.swap(it_sv_1d);
  EXPECT_EQ(it_sv_1d, copy_it_sv_2d);
  EXPECT_EQ(it_sv_2d, copy_it_sv_1d);
}

TYPED_TEST(StorageViewTest, IteratorAdvance) {
  int dim1 = this->dim1, dim2 = this->dim2, dim3 = this->dim3, dim4 = this->dim4, dim5 = this->dim5;

// -------------------------------------------------------------------------------------------------
// 1D
// -------------------------------------------------------------------------------------------------
#define CHECK_1D(name)                                                                             \
  {                                                                                                \
    auto sv = this->name->toStorageView();                                                         \
    auto it = sv.begin();                                                                          \
    for(int i = 0; i < dim1; ++i, ++it)                                                            \
      ASSERT_EQ(it.template as<TypeParam>(), this->name->at(i));                                   \
  }

  CHECK_1D(storage_1d);
  CHECK_1D(storage_1d_padded);

#undef CHECK_1D

// -------------------------------------------------------------------------------------------------
// 2D
// -------------------------------------------------------------------------------------------------
#define CHECK_2D(name)                                                                             \
  {                                                                                                \
    auto sv = this->name->toStorageView();                                                         \
    auto it = sv.begin();                                                                          \
    for(int j = 0; j < dim2; ++j)                                                                  \
      for(int i = 0; i < dim1; ++i, ++it)                                                          \
        ASSERT_EQ(it.template as<TypeParam>(), this->name->at(i, j));                              \
  }

  CHECK_2D(storage_2d_col_major);
  CHECK_2D(storage_2d_row_major);
  CHECK_2D(storage_2d_col_major_padded);
  CHECK_2D(storage_2d_row_major_padded);

#undef CHECK_2D

// -------------------------------------------------------------------------------------------------
// 3D
// -------------------------------------------------------------------------------------------------
#define CHECK_3D(name)                                                                             \
  {                                                                                                \
    auto sv = this->name->toStorageView();                                                         \
    auto it = sv.begin();                                                                          \
    for(int k = 0; k < dim3; ++k)                                                                  \
      for(int j = 0; j < dim2; ++j)                                                                \
        for(int i = 0; i < dim1; ++i, ++it)                                                        \
          ASSERT_EQ(it.template as<TypeParam>(), this->name->at(i, j, k));                         \
  }

  CHECK_3D(storage_3d_col_major);
  CHECK_3D(storage_3d_row_major);
  CHECK_3D(storage_3d_col_major_padded);
  CHECK_3D(storage_3d_row_major_padded);

#undef CHECK_3D

// -------------------------------------------------------------------------------------------------
// 5D
// -------------------------------------------------------------------------------------------------
#define CHECK_5D(name)                                                                             \
  {                                                                                                \
    auto sv = this->name->toStorageView();                                                         \
    auto it = sv.begin();                                                                          \
    for(int m = 0; m < dim5; ++m)                                                                  \
      for(int l = 0; l < dim4; ++l)                                                                \
        for(int k = 0; k < dim3; ++k)                                                              \
          for(int j = 0; j < dim2; ++j)                                                            \
            for(int i = 0; i < dim1; ++i, ++it)                                                    \
              ASSERT_EQ(it.template as<TypeParam>(), this->name->at(i, j, k, l, m));               \
  }

  CHECK_5D(storage_5d_col_major_padded);
  CHECK_5D(storage_5d_row_major_padded);
#undef CHECK_5D
}

TYPED_TEST(StorageViewTest, isMemCopyable) {
  // 1D storages are always memcopyable
  EXPECT_TRUE(this->storage_1d->toStorageView().isMemCopyable());
  EXPECT_TRUE(this->storage_1d_padded->toStorageView().isMemCopyable());

  // Multi dimensional storages are only memcopyable if they are ColMajor and have no padding
  EXPECT_TRUE(this->storage_2d_col_major->toStorageView().isMemCopyable());
  EXPECT_FALSE(this->storage_2d_col_major_padded->toStorageView().isMemCopyable());
  EXPECT_FALSE(this->storage_2d_row_major->toStorageView().isMemCopyable());
  EXPECT_FALSE(this->storage_2d_row_major_padded->toStorageView().isMemCopyable());
}

TYPED_TEST(StorageViewTest, toString) {
  auto sv_1d = this->storage_1d->toStorageView();
  std::stringstream ss;

  // StorageView
  ss << sv_1d;
  EXPECT_TRUE(boost::algorithm::starts_with(ss.str(), "StorageView"));
  EXPECT_NE(ss.str().find("originPtr"), std::string::npos);
  EXPECT_NE(ss.str().find("type"), std::string::npos);
  EXPECT_NE(ss.str().find("dims"), std::string::npos);
  EXPECT_NE(ss.str().find("strides"), std::string::npos);
  ss.str("");
  ss.clear();

  // StorageViewIterator
  ss << sv_1d.begin();
  EXPECT_TRUE(boost::algorithm::starts_with(ss.str(), "StorageViewIterator"));
  EXPECT_NE(ss.str().find("originPtr"), std::string::npos);
  EXPECT_NE(ss.str().find("index"), std::string::npos);
  EXPECT_NE(ss.str().find("end"), std::string::npos);
  EXPECT_NE(ss.str().find("curPtr"), std::string::npos);
  EXPECT_NE(ss.str().find("dims"), std::string::npos);
  EXPECT_NE(ss.str().find("strides"), std::string::npos);
  EXPECT_NE(ss.str().find("bytesPerElement"), std::string::npos);
}

TEST(StorageViewSliceTest, Iteration) {
  using Storage = Storage<double>;

  int dim1 = 10, dim2 = 15, dim3 = 20;

  Storage storage_1d(Storage::RowMajor, {dim1}, Storage::sequential);
  Storage storage_2d(Storage::RowMajor, {dim1, dim2}, Storage::sequential);
  Storage storage_3d(Storage::RowMajor, {dim1, dim2, dim3}, {{3, 3}, {3, 3}, {3, 3}},
                     Storage::sequential);

  auto sv_1d = storage_1d.toStorageView();
  auto sv_2d = storage_2d.toStorageView();
  auto sv_3d = storage_3d.toStorageView();

  // -----------------------------------------------------------------------------------------------
  // 1D
  // -----------------------------------------------------------------------------------------------
  {
    sv_1d.setSlice(Slice());
    auto it = sv_1d.begin();
    for(int i = 0; i < dim1; ++i, ++it)
      ASSERT_EQ(it.as<double>(), storage_1d(i)) << "(i) = (" << i << ")";

    // Sliced StorageView are always treated as uncopyable
    ASSERT_FALSE(sv_1d.isMemCopyable());
  }

  {
    sv_1d.setSlice(Slice(0, 5, 2));
    auto it = sv_1d.begin();
    for(int i = 0; i < 5; i += 2, ++it)
      ASSERT_EQ(it.as<double>(), storage_1d(i)) << "(i) = (" << i << ")";
  }

  // -----------------------------------------------------------------------------------------------
  // 2D
  // -----------------------------------------------------------------------------------------------
  {
    sv_2d.setSlice(Slice()());
    auto it = sv_2d.begin();
    for(int j = 0; j < dim2; ++j)
      for(int i = 0; i < dim1; ++i, ++it)
        ASSERT_EQ(it.as<double>(), storage_2d(i, j)) << "(i,j) = (" << i << "," << j << ")";
  }

  {
    sv_2d.setSlice(Slice(1, 6, 2)(2, 6, 3));
    auto it = sv_2d.begin();
    for(int j = 2; j < 6; j += 3)
      for(int i = 1; i < 6; i += 2, ++it)
        ASSERT_EQ(it.as<double>(), storage_2d(i, j)) << "(i,j) = (" << i << "," << j << ")";
  }

  {
    sv_2d.setSlice(Slice()(2, 6, 3));
    auto it = sv_2d.begin();
    for(int j = 2; j < 6; j += 3)
      for(int i = 0; i < dim1; i += 1, ++it)
        ASSERT_EQ(it.as<double>(), storage_2d(i, j)) << "(i,j) = (" << i << "," << j << ")";
  }

  // -----------------------------------------------------------------------------------------------
  // 3D
  // -----------------------------------------------------------------------------------------------
  {
    sv_3d.setSlice(Slice()()());
    auto it = sv_3d.begin();
    for(int k = 0; k < dim3; ++k)
      for(int j = 0; j < dim2; ++j)
        for(int i = 0; i < dim1; ++i, ++it)
          ASSERT_EQ(it.as<double>(), storage_3d(i, j, k))
              << "(i,j,k) = (" << i << "," << j << "," << k << ")";
  }

  {
    sv_3d.setSlice(Slice()()(5, 10));
    auto it = sv_3d.begin();
    for(int k = 5; k < 10; ++k)
      for(int j = 0; j < dim2; ++j)
        for(int i = 0; i < dim1; ++i, ++it)
          ASSERT_EQ(it.as<double>(), storage_3d(i, j, k))
              << "(i,j,k) = (" << i << "," << j << "," << k << ")";
  }

  {
    sv_3d.setSlice(Slice(2, -1, 1)(2, 3)(5, 10));
    auto it = sv_3d.begin();
    for(int k = 5; k < 10; ++k)
      for(int j = 2; j < 3; ++j)
        for(int i = 2; i < dim1; ++i, ++it)
          ASSERT_EQ(it.as<double>(), storage_3d(i, j, k))
              << "(i,j,k) = (" << i << "," << j << "," << k << ")";
  }
}
