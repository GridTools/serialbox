//===-- serialbox/core/frontend/gridtools/UnittestStorageView.cpp -------------------*- C++ -*-===//
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

#include "utility/GridTools.h"
#include "serialbox/core/STLExtras.h"
#include "serialbox/core/StorageView.h"
#include <gtest/gtest.h>
#include <memory>

#ifdef SERIALBOX_HAS_GRIDTOOLS

#include "serialbox/core/frontend/gridtools/StorageViewHelper.h"

namespace {

template <class T>
class GridToolsStorageViewTest : public testing::Test {
public:
  using storage_types = serialbox::unittest::gridtools_storage_types<T>;

  // Dimensions
  int dim1;
  int dim2;
  int dim3;
  int dim4;

  // Meta Data
  typename storage_types::cpu_2d_real_meta_data_type cpu_2d_real_meta_data;
  typename storage_types::gpu_2d_real_meta_data_type gpu_2d_real_meta_data;
  typename storage_types::cpu_2d_meta_data_type cpu_2d_meta_data;
  typename storage_types::gpu_2d_meta_data_type gpu_2d_meta_data;
  typename storage_types::cpu_3d_meta_data_type cpu_3d_meta_data;
  typename storage_types::gpu_3d_meta_data_type gpu_3d_meta_data;
  typename storage_types::cpu_4d_meta_data_type cpu_4d_meta_data;
  typename storage_types::gpu_4d_meta_data_type gpu_4d_meta_data;

  // Storages
  typename storage_types::cpu_2d_real_storage_type cpu_2d_real_storage;
  typename storage_types::gpu_2d_real_storage_type gpu_2d_real_storage;
  typename storage_types::cpu_2d_storage_type cpu_2d_storage;
  typename storage_types::gpu_2d_storage_type gpu_2d_storage;
  typename storage_types::cpu_3d_storage_type cpu_3d_storage;
  typename storage_types::gpu_3d_storage_type gpu_3d_storage;
  typename storage_types::cpu_4d_storage_type cpu_4d_storage;
  typename storage_types::gpu_4d_storage_type gpu_4d_storage;

protected:
  GridToolsStorageViewTest()
      : dim1(2 + storage_types::halo1_left + storage_types::halo1_right),
        dim2(3 + storage_types::halo2_left + storage_types::halo2_right),
        dim3(4 + storage_types::halo3_left + storage_types::halo3_right),
        dim4(5 + storage_types::halo4_left + storage_types::halo4_right),
        cpu_2d_real_meta_data(dim1, dim2),                                       //
        gpu_2d_real_meta_data(dim1, dim2),                                       //
        cpu_2d_meta_data(dim1, dim2, 0),                                         //
        gpu_2d_meta_data(dim1, dim2, 0),                                         //
        cpu_3d_meta_data(dim1, dim2, dim3),                                      //
        gpu_3d_meta_data(dim1, dim2, dim3),                                      //
        cpu_4d_meta_data(dim1, dim2, dim3, dim4),                                //
        gpu_4d_meta_data(dim1, dim2, dim3, dim4),                                //
        cpu_2d_real_storage(cpu_2d_real_meta_data, "cpu_2d_real_storage", -1.0), //
        gpu_2d_real_storage(gpu_2d_real_meta_data, "gpu_2d_real_storage", -1.0), //
        cpu_2d_storage(cpu_2d_meta_data, "cpu_2d_storage", -1.0),                //
        gpu_2d_storage(gpu_2d_meta_data, "gpu_2d_storage", -1.0),                //
        cpu_3d_storage(cpu_3d_meta_data, "cpu_3d_storage", -1.0),                //
        gpu_3d_storage(gpu_3d_meta_data, "gpu_3d_storage", -1.0),                //
        cpu_4d_storage(cpu_4d_meta_data, "cpu_4d_storage", -1.0),                //
        gpu_4d_storage(gpu_4d_meta_data, "gpu_4d_storage", -1.0) {

    storage_types::init2DReal(cpu_2d_real_storage, dim1, dim2);
    storage_types::init2DReal(gpu_2d_real_storage, dim1, dim2);
    storage_types::init2D(cpu_2d_storage, dim1, dim2);
    storage_types::init2D(gpu_2d_storage, dim1, dim2);

    storage_types::init3D(cpu_3d_storage, dim1, dim2, dim3);
    storage_types::init3D(gpu_3d_storage, dim1, dim2, dim3);

    storage_types::init4D(cpu_4d_storage, dim1, dim2, dim3, dim4);
    storage_types::init4D(gpu_4d_storage, dim1, dim2, dim3, dim4);
  }
};

using TestTypes = testing::Types<double, float, int>;

} // anonymous namespace

TYPED_TEST_CASE(GridToolsStorageViewTest, TestTypes);

template <typename Storage>
serialbox::StorageView make_storage_view(const Storage& storage) {
  using namespace serialbox::gridtools;

  std::vector<int> dims(internal::get_dims(storage.meta_data()));
  std::vector<int> strides(internal::get_strides(storage, storage.meta_data()));
  void* originPtr = internal::get_origin_ptr(storage, storage.meta_data(), 0);

  return serialbox::StorageView(originPtr, serialbox::ToTypeID<typename Storage::value_type>::value,
                                std::move(dims), std::move(strides));
}

using namespace serialbox::gridtools;
#define GET_DIMS_STRIDES_ORIGIN_PTR(storage, prefix)                                               \
  std::vector<int> prefix##_dims(internal::get_dims(storage.meta_data()));                         \
  std::vector<int> prefix##_strides(internal::get_strides(storage, storage.meta_data()));          \
  void* prefix##_origin_ptr = internal::get_origin_ptr(storage, storage.meta_data(), 0);

TYPED_TEST(GridToolsStorageViewTest, Construction_2DRealCPU) {
  auto& cpu_2d_real_storage = this->cpu_2d_real_storage;
  auto& cpu_2d_real_meta_data = this->cpu_2d_real_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(cpu_2d_real_storage, cpu_2d_real)

  // Dimensions
  EXPECT_EQ(cpu_2d_real_dims[0], this->dim1);
  EXPECT_EQ(cpu_2d_real_dims[1], this->dim2);

  // Strides
  EXPECT_EQ(cpu_2d_real_strides[0], cpu_2d_real_meta_data.template strides<0>());
  EXPECT_EQ(cpu_2d_real_strides[1], cpu_2d_real_meta_data.template strides<1>());

  // Data
  EXPECT_EQ(cpu_2d_real_origin_ptr, static_cast<void*>(&cpu_2d_real_storage(0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_2DCPU) {
  // -----------------------------------------------------------------------------------------------
  // 2D CPU Storage
  // -----------------------------------------------------------------------------------------------
  auto& cpu_2d_storage = this->cpu_2d_storage;
  auto& cpu_2d_meta_data = this->cpu_2d_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(cpu_2d_storage, cpu_2d)

  // Dimensions
  EXPECT_EQ(cpu_2d_dims[0], this->dim1);
  EXPECT_EQ(cpu_2d_dims[1], this->dim2);
  EXPECT_EQ(cpu_2d_dims[2], 0);

  // Strides
  EXPECT_EQ(cpu_2d_strides[0], cpu_2d_meta_data.template strides<0>());
  EXPECT_EQ(cpu_2d_strides[1], cpu_2d_meta_data.template strides<1>());
  EXPECT_EQ(cpu_2d_strides[2], cpu_2d_meta_data.template strides<2>());

  // Data
  EXPECT_EQ(cpu_2d_origin_ptr, static_cast<void*>(&cpu_2d_storage(0, 0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_3DCPU) {
  auto& cpu_3d_storage = this->cpu_3d_storage;
  auto& cpu_3d_meta_data = this->cpu_3d_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(cpu_3d_storage, cpu_3d)

  // Dimensions
  EXPECT_EQ(cpu_3d_dims[0], this->dim1);
  EXPECT_EQ(cpu_3d_dims[1], this->dim2);
  EXPECT_EQ(cpu_3d_dims[2], this->dim3);

  // Strides
  EXPECT_EQ(cpu_3d_strides[0], cpu_3d_meta_data.template strides<0>());
  EXPECT_EQ(cpu_3d_strides[1], cpu_3d_meta_data.template strides<1>());
  EXPECT_EQ(cpu_3d_strides[2], cpu_3d_meta_data.template strides<2>());

  // Data
  EXPECT_EQ(cpu_3d_origin_ptr, static_cast<void*>(&cpu_3d_storage(0, 0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_4DCPU) {
  auto& cpu_4d_storage = this->cpu_4d_storage;
  auto& cpu_4d_meta_data = this->cpu_4d_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(cpu_4d_storage, cpu_4d)

  // Dimensions
  EXPECT_EQ(cpu_4d_dims[0], this->dim1);
  EXPECT_EQ(cpu_4d_dims[1], this->dim2);
  EXPECT_EQ(cpu_4d_dims[2], this->dim3);
  EXPECT_EQ(cpu_4d_dims[3], this->dim4);

  // Strides
  EXPECT_EQ(cpu_4d_strides[0], cpu_4d_meta_data.template strides<0>());
  EXPECT_EQ(cpu_4d_strides[1], cpu_4d_meta_data.template strides<1>());
  EXPECT_EQ(cpu_4d_strides[2], cpu_4d_meta_data.template strides<2>());
  EXPECT_EQ(cpu_4d_strides[3], cpu_4d_meta_data.template strides<3>());

  // Data
  EXPECT_EQ(cpu_4d_origin_ptr, static_cast<void*>(&cpu_4d_storage(0, 0, 0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_2DRealGPU) {
  auto& gpu_2d_real_storage = this->gpu_2d_real_storage;
  auto& gpu_2d_real_meta_data = this->gpu_2d_real_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(gpu_2d_real_storage, gpu_2d_real)

  // Dimensions
  EXPECT_EQ(gpu_2d_real_dims[0], this->dim1);
  EXPECT_EQ(gpu_2d_real_dims[1], this->dim2);

  // Strides
  EXPECT_EQ(gpu_2d_real_strides[0], gpu_2d_real_meta_data.template strides<0>());
  EXPECT_EQ(gpu_2d_real_strides[1], gpu_2d_real_meta_data.template strides<1>());

  // Data
  EXPECT_EQ(gpu_2d_real_origin_ptr, static_cast<void*>(&gpu_2d_real_storage(0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_2DGPU) {
  auto& gpu_2d_storage = this->gpu_2d_storage;
  auto& gpu_2d_meta_data = this->gpu_2d_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(gpu_2d_storage, gpu_2d)

  // Dimensions
  EXPECT_EQ(gpu_2d_dims[0], this->dim1);
  EXPECT_EQ(gpu_2d_dims[1], this->dim2);
  EXPECT_EQ(gpu_2d_dims[2], 0);

  // Strides
  EXPECT_EQ(gpu_2d_strides[0], gpu_2d_meta_data.template strides<0>());
  EXPECT_EQ(gpu_2d_strides[1], gpu_2d_meta_data.template strides<1>());
  EXPECT_EQ(gpu_2d_strides[2], gpu_2d_meta_data.template strides<2>());

  // Data
  EXPECT_EQ(gpu_2d_origin_ptr, static_cast<void*>(&gpu_2d_storage(0, 0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_3DGPU) {
  auto& gpu_3d_storage = this->gpu_3d_storage;
  auto& gpu_3d_meta_data = this->gpu_3d_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(gpu_3d_storage, gpu_3d)

  // Dimensions
  EXPECT_EQ(gpu_3d_dims[0], this->dim1);
  EXPECT_EQ(gpu_3d_dims[1], this->dim2);
  EXPECT_EQ(gpu_3d_dims[2], this->dim3);

  // Strides
  EXPECT_EQ(gpu_3d_strides[0], gpu_3d_meta_data.template strides<0>());
  EXPECT_EQ(gpu_3d_strides[1], gpu_3d_meta_data.template strides<1>());
  EXPECT_EQ(gpu_3d_strides[2], gpu_3d_meta_data.template strides<2>());

  // Data
  EXPECT_EQ(gpu_3d_origin_ptr, static_cast<void*>(&gpu_3d_storage(0, 0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_4DGPU) {
  auto& gpu_4d_storage = this->gpu_4d_storage;
  auto& gpu_4d_meta_data = this->gpu_4d_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(gpu_4d_storage, gpu_4d)

  // Dimensions
  EXPECT_EQ(gpu_4d_dims[0], this->dim1);
  EXPECT_EQ(gpu_4d_dims[1], this->dim2);
  EXPECT_EQ(gpu_4d_dims[2], this->dim3);
  EXPECT_EQ(gpu_4d_dims[3], this->dim4);

  // Strides
  EXPECT_EQ(gpu_4d_strides[0], gpu_4d_meta_data.template strides<0>());
  EXPECT_EQ(gpu_4d_strides[1], gpu_4d_meta_data.template strides<1>());
  EXPECT_EQ(gpu_4d_strides[2], gpu_4d_meta_data.template strides<2>());
  EXPECT_EQ(gpu_4d_strides[3], gpu_4d_meta_data.template strides<3>());

  // Data
  EXPECT_EQ(gpu_4d_origin_ptr, static_cast<void*>(&gpu_4d_storage(0, 0, 0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_2DRealCPU) {
  auto& cpu_2d_real_storage = this->cpu_2d_real_storage;
  serialbox::StorageView cpu_2d_real_storage_view = make_storage_view(cpu_2d_real_storage);

  auto cpu_2d_real_it = cpu_2d_real_storage_view.begin();
  for(int j = 0; j < this->dim2; ++j)
    for(int i = 0; i < this->dim1; ++i, ++cpu_2d_real_it) {
      ASSERT_EQ(cpu_2d_real_it.as<TypeParam>(), cpu_2d_real_storage(i, j));
    }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_2DCPU) {
  auto& cpu_2d_storage = this->cpu_2d_storage;
  serialbox::StorageView cpu_2d_storage_view = make_storage_view(cpu_2d_storage);

  auto cpu_2d_it = cpu_2d_storage_view.begin();
  for(int j = 0; j < this->dim2; ++j)
    for(int i = 0; i < this->dim1; ++i, ++cpu_2d_it) {
      ASSERT_EQ(cpu_2d_it.as<TypeParam>(), cpu_2d_storage(i, j, 0));
    }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_3DCPU) {
  auto& cpu_3d_storage = this->cpu_3d_storage;
  serialbox::StorageView cpu_3d_storage_view = make_storage_view(cpu_3d_storage);

  auto cpu_3d_it = cpu_3d_storage_view.begin();
  for(int k = 0; k < this->dim3; ++k)
    for(int j = 0; j < this->dim2; ++j)
      for(int i = 0; i < this->dim1; ++i, ++cpu_3d_it) {
        ASSERT_EQ(cpu_3d_it.as<TypeParam>(), cpu_3d_storage(i, j, k));
      }
}
TYPED_TEST(GridToolsStorageViewTest, Iterator_4DCPU) {

  auto& cpu_4d_storage = this->cpu_4d_storage;
  serialbox::StorageView cpu_4d_storage_view = make_storage_view(cpu_4d_storage);

  auto cpu_4d_it = cpu_4d_storage_view.begin();
  for(int l = 0; l < this->dim4; ++l)
    for(int k = 0; k < this->dim3; ++k)
      for(int j = 0; j < this->dim2; ++j)
        for(int i = 0; i < this->dim1; ++i, ++cpu_4d_it) {
          ASSERT_EQ(cpu_4d_it.as<TypeParam>(), cpu_4d_storage(i, j, k, l));
        }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_2DRealGPU) {
  auto& gpu_2d_real_storage = this->gpu_2d_real_storage;
  serialbox::StorageView gpu_2d_real_storage_view = make_storage_view(gpu_2d_real_storage);

  auto gpu_2d_real_it = gpu_2d_real_storage_view.begin();
  for(int j = 0; j < this->dim2; ++j)
    for(int i = 0; i < this->dim1; ++i, ++gpu_2d_real_it) {
      ASSERT_EQ(gpu_2d_real_it.as<TypeParam>(), gpu_2d_real_storage(i, j));
    }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_2DGPU) {
  auto& gpu_2d_storage = this->gpu_2d_storage;
  serialbox::StorageView gpu_2d_storage_view = make_storage_view(gpu_2d_storage);

  auto gpu_2d_it = gpu_2d_storage_view.begin();
  for(int j = 0; j < this->dim2; ++j)
    for(int i = 0; i < this->dim1; ++i, ++gpu_2d_it) {
      ASSERT_EQ(gpu_2d_it.as<TypeParam>(), gpu_2d_storage(i, j, 0));
    }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_3DGPU) {
  auto& gpu_3d_storage = this->gpu_3d_storage;
  serialbox::StorageView gpu_3d_storage_view = make_storage_view(gpu_3d_storage);

  auto gpu_3d_it = gpu_3d_storage_view.begin();
  for(int k = 0; k < this->dim3; ++k)
    for(int j = 0; j < this->dim2; ++j)
      for(int i = 0; i < this->dim1; ++i, ++gpu_3d_it) {
        ASSERT_EQ(gpu_3d_it.as<TypeParam>(), gpu_3d_storage(i, j, k));
      }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_4DGPU) {
  auto& gpu_4d_storage = this->gpu_4d_storage;
  serialbox::StorageView gpu_4d_storage_view = make_storage_view(gpu_4d_storage);

  auto gpu_4d_it = gpu_4d_storage_view.begin();
  for(int l = 0; l < this->dim4; ++l)
    for(int k = 0; k < this->dim3; ++k)
      for(int j = 0; j < this->dim2; ++j)
        for(int i = 0; i < this->dim1; ++i, ++gpu_4d_it) {
          ASSERT_EQ(gpu_4d_it.as<TypeParam>(), gpu_4d_storage(i, j, k, l));
        }
}

TYPED_TEST(GridToolsStorageViewTest, isMemCopyable) {
  EXPECT_FALSE(make_storage_view(this->cpu_2d_real_storage).isMemCopyable());
  EXPECT_FALSE(make_storage_view(this->gpu_2d_real_storage).isMemCopyable());
  EXPECT_FALSE(make_storage_view(this->cpu_2d_storage).isMemCopyable());
  EXPECT_FALSE(make_storage_view(this->gpu_2d_storage).isMemCopyable());
  EXPECT_FALSE(make_storage_view(this->cpu_3d_storage).isMemCopyable());
  EXPECT_FALSE(make_storage_view(this->gpu_3d_storage).isMemCopyable());
  EXPECT_FALSE(make_storage_view(this->cpu_4d_storage).isMemCopyable());
  EXPECT_FALSE(make_storage_view(this->gpu_4d_storage).isMemCopyable());

  // Create a memcopyable storage
  using layout_type = gridtools::layout_map<2, 1, 0>; // stride 1 on i (col-major)
  using meta_data_type = typename GridToolsStorageViewTest<
      TypeParam>::storage_types::storage_traits_type::template meta_storage_type<9, layout_type>;
  using storage_type = typename GridToolsStorageViewTest<TypeParam>::storage_types::
      storage_traits_type::template storage_type<TypeParam, meta_data_type>;

  meta_data_type meta_data(this->dim1, this->dim2, this->dim3);
  storage_type storage(meta_data, "storage", -1.0);
  EXPECT_TRUE(make_storage_view(storage).isMemCopyable());
}

#endif
