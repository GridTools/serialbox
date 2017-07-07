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
  int dim1_no_halo;
  int dim2_no_halo;
  int dim3_no_halo;
  int dim4_no_halo;
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
      : dim1_no_halo(2), dim2_no_halo(3), dim3_no_halo(4), dim4_no_halo(5),
        dim1(2 + storage_types::halo1_left + storage_types::halo1_right),
        dim2(3 + storage_types::halo2_left + storage_types::halo2_right),
        dim3(4 + storage_types::halo3_left + storage_types::halo3_right),
        dim4(5 + storage_types::halo4_left + storage_types::halo4_right),
        cpu_2d_real_meta_data(dim1_no_halo, dim2_no_halo),                        //
        gpu_2d_real_meta_data(dim1_no_halo, dim2_no_halo),                        //
        cpu_2d_meta_data(dim1_no_halo, dim2_no_halo, 1),                          //
        gpu_2d_meta_data(dim1_no_halo, dim2_no_halo, 1),                          //
        cpu_3d_meta_data(dim1_no_halo, dim2_no_halo, dim3_no_halo),               //
        gpu_3d_meta_data(dim1_no_halo, dim2_no_halo, dim3_no_halo),               //
        cpu_4d_meta_data(dim1_no_halo, dim2_no_halo, dim3_no_halo, dim4_no_halo), //
        gpu_4d_meta_data(dim1_no_halo, dim2_no_halo, dim3_no_halo, dim4_no_halo), //
        cpu_2d_real_storage(cpu_2d_real_meta_data, "cpu_2d_real_storage"),        //
        gpu_2d_real_storage(gpu_2d_real_meta_data, "gpu_2d_real_storage"),        //
        cpu_2d_storage(cpu_2d_meta_data, "cpu_2d_storage"),                       //
        gpu_2d_storage(gpu_2d_meta_data, "gpu_2d_storage"),                       //
        cpu_3d_storage(cpu_3d_meta_data, "cpu_3d_storage"),                       //
        gpu_3d_storage(gpu_3d_meta_data, "gpu_3d_storage"),                       //
        cpu_4d_storage(cpu_4d_meta_data, "cpu_4d_storage"),                       //
        gpu_4d_storage(gpu_4d_meta_data, "gpu_4d_storage") {
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

  std::vector<int> dims(internal::get_dims(*storage.get_storage_info_ptr()));
  std::vector<int> strides(internal::get_strides(storage, *storage.get_storage_info_ptr()));
  void* originPtr = internal::get_origin_ptr(storage, *storage.get_storage_info_ptr(), 0);

  return serialbox::StorageView(originPtr, serialbox::ToTypeID<typename Storage::data_t>::value,
                                std::move(dims), std::move(strides));
}

using namespace serialbox::gridtools;
#define GET_DIMS_STRIDES_ORIGIN_PTR(storage, prefix)                                               \
  std::vector<int> prefix##_dims(internal::get_dims(*storage.get_storage_info_ptr()));             \
  std::vector<int> prefix##_strides(                                                               \
      internal::get_strides(storage, *storage.get_storage_info_ptr()));                            \
  void* prefix##_origin_ptr = internal::get_origin_ptr(storage, *storage.get_storage_info_ptr(), 0);

TYPED_TEST(GridToolsStorageViewTest, Construction_2DRealCPU) {
  auto& cpu_2d_real_storage = this->cpu_2d_real_storage;
  auto& cpu_2d_real_meta_data = this->cpu_2d_real_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(cpu_2d_real_storage, cpu_2d_real)

  // Dimensions
  EXPECT_EQ(cpu_2d_real_dims[0], this->dim1);
  EXPECT_EQ(cpu_2d_real_dims[1], this->dim2);

  // Strides
  EXPECT_EQ(cpu_2d_real_strides[0], cpu_2d_real_meta_data.template stride<0>());
  EXPECT_EQ(cpu_2d_real_strides[1], cpu_2d_real_meta_data.template stride<1>());

  // Data
  auto view = make_host_view(cpu_2d_real_storage);
  EXPECT_EQ(cpu_2d_real_origin_ptr, static_cast<void*>(&view(0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_2DCPU) {
  auto& cpu_2d_storage = this->cpu_2d_storage;
  auto& cpu_2d_meta_data = this->cpu_2d_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(cpu_2d_storage, cpu_2d)

  // Dimensions
  EXPECT_EQ(cpu_2d_dims[0], this->dim1);
  EXPECT_EQ(cpu_2d_dims[1], this->dim2);
  EXPECT_EQ(cpu_2d_dims[2], 1);

  // Strides
  EXPECT_EQ(cpu_2d_strides[0], cpu_2d_meta_data.template stride<0>());
  EXPECT_EQ(cpu_2d_strides[1], cpu_2d_meta_data.template stride<1>());
  EXPECT_EQ(cpu_2d_strides[2], cpu_2d_meta_data.template stride<2>());

  // Data
  auto view = make_host_view(cpu_2d_storage);
  EXPECT_EQ(cpu_2d_origin_ptr, static_cast<void*>(&view(0, 0, 0)));
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
  EXPECT_EQ(cpu_3d_strides[0], cpu_3d_meta_data.template stride<0>());
  EXPECT_EQ(cpu_3d_strides[1], cpu_3d_meta_data.template stride<1>());
  EXPECT_EQ(cpu_3d_strides[2], cpu_3d_meta_data.template stride<2>());

  // Data
  auto view = make_host_view(cpu_3d_storage);
  EXPECT_EQ(cpu_3d_origin_ptr, static_cast<void*>(&view(0, 0, 0)));
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
  EXPECT_EQ(cpu_4d_strides[0], cpu_4d_meta_data.template stride<0>());
  EXPECT_EQ(cpu_4d_strides[1], cpu_4d_meta_data.template stride<1>());
  EXPECT_EQ(cpu_4d_strides[2], cpu_4d_meta_data.template stride<2>());
  EXPECT_EQ(cpu_4d_strides[3], cpu_4d_meta_data.template stride<3>());

  // Data
  auto view = make_host_view(cpu_4d_storage);
  EXPECT_EQ(cpu_4d_origin_ptr, static_cast<void*>(&view(0, 0, 0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_2DRealGPU) {
  auto& gpu_2d_real_storage = this->gpu_2d_real_storage;
  auto& gpu_2d_real_meta_data = this->gpu_2d_real_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(gpu_2d_real_storage, gpu_2d_real)

  // Dimensions
  EXPECT_EQ(gpu_2d_real_dims[0], this->dim1);
  EXPECT_EQ(gpu_2d_real_dims[1], this->dim2);

  // Strides
  EXPECT_EQ(gpu_2d_real_strides[0], gpu_2d_real_meta_data.template stride<0>());
  EXPECT_EQ(gpu_2d_real_strides[1], gpu_2d_real_meta_data.template stride<1>());

  // Data

  auto view = make_host_view(gpu_2d_real_storage);
  EXPECT_EQ(gpu_2d_real_origin_ptr, static_cast<void*>(&view(0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Construction_2DGPU) {
  auto& gpu_2d_storage = this->gpu_2d_storage;
  auto& gpu_2d_meta_data = this->gpu_2d_meta_data;

  GET_DIMS_STRIDES_ORIGIN_PTR(gpu_2d_storage, gpu_2d)

  // Dimensions
  EXPECT_EQ(gpu_2d_dims[0], this->dim1);
  EXPECT_EQ(gpu_2d_dims[1], this->dim2);
  EXPECT_EQ(gpu_2d_dims[2], 1);

  // Strides
  EXPECT_EQ(gpu_2d_strides[0], gpu_2d_meta_data.template stride<0>());
  EXPECT_EQ(gpu_2d_strides[1], gpu_2d_meta_data.template stride<1>());
  EXPECT_EQ(gpu_2d_strides[2], gpu_2d_meta_data.template stride<2>());

  // Data
  auto view = make_host_view(gpu_2d_storage);
  EXPECT_EQ(gpu_2d_origin_ptr, static_cast<void*>(&view(0, 0, 0)));
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
  EXPECT_EQ(gpu_3d_strides[0], gpu_3d_meta_data.template stride<0>());
  EXPECT_EQ(gpu_3d_strides[1], gpu_3d_meta_data.template stride<1>());
  EXPECT_EQ(gpu_3d_strides[2], gpu_3d_meta_data.template stride<2>());

  // Data
  auto view = make_host_view(gpu_3d_storage);
  EXPECT_EQ(gpu_3d_origin_ptr, static_cast<void*>(&view(0, 0, 0)));
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
  EXPECT_EQ(gpu_4d_strides[0], gpu_4d_meta_data.template stride<0>());
  EXPECT_EQ(gpu_4d_strides[1], gpu_4d_meta_data.template stride<1>());
  EXPECT_EQ(gpu_4d_strides[2], gpu_4d_meta_data.template stride<2>());
  EXPECT_EQ(gpu_4d_strides[3], gpu_4d_meta_data.template stride<3>());

  // Data
  auto view = make_host_view(gpu_4d_storage);
  EXPECT_EQ(gpu_4d_origin_ptr, static_cast<void*>(&view(0, 0, 0, 0)));
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_2DRealCPU) {
  auto& cpu_2d_real_storage = this->cpu_2d_real_storage;
  serialbox::StorageView cpu_2d_real_storage_view = make_storage_view(cpu_2d_real_storage);

  auto cpu_2d_real_it = cpu_2d_real_storage_view.begin();

  auto gt_view = make_host_view(cpu_2d_real_storage);
  for(int j = 0; j < this->dim2; ++j)
    for(int i = 0; i < this->dim1; ++i, ++cpu_2d_real_it) {
      ASSERT_EQ(cpu_2d_real_it.as<TypeParam>(), gt_view(i, j));
    }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_2DCPU) {
  auto& cpu_2d_storage = this->cpu_2d_storage;
  serialbox::StorageView cpu_2d_storage_view = make_storage_view(cpu_2d_storage);

  auto cpu_2d_it = cpu_2d_storage_view.begin();

  auto gt_view = make_host_view(cpu_2d_storage);
  for(int j = 0; j < this->dim2; ++j)
    for(int i = 0; i < this->dim1; ++i, ++cpu_2d_it) {
      ASSERT_EQ(cpu_2d_it.as<TypeParam>(), gt_view(i, j, 0));
    }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_3DCPU) {
  auto& cpu_3d_storage = this->cpu_3d_storage;
  serialbox::StorageView cpu_3d_storage_view = make_storage_view(cpu_3d_storage);

  auto cpu_3d_it = cpu_3d_storage_view.begin();

  auto gt_view = make_host_view(cpu_3d_storage);
  for(int k = 0; k < this->dim3; ++k)
    for(int j = 0; j < this->dim2; ++j)
      for(int i = 0; i < this->dim1; ++i, ++cpu_3d_it) {
        ASSERT_EQ(cpu_3d_it.as<TypeParam>(), gt_view(i, j, k));
      }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_4DCPU) {
  auto& cpu_4d_storage = this->cpu_4d_storage;
  serialbox::StorageView cpu_4d_storage_view = make_storage_view(cpu_4d_storage);

  auto cpu_4d_it = cpu_4d_storage_view.begin();

  auto gt_view = make_host_view(cpu_4d_storage);
  for(int l = 0; l < this->dim4; ++l)
    for(int k = 0; k < this->dim3; ++k)
      for(int j = 0; j < this->dim2; ++j)
        for(int i = 0; i < this->dim1; ++i, ++cpu_4d_it) {
          ASSERT_EQ(cpu_4d_it.as<TypeParam>(), gt_view(i, j, k, l));
        }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_2DRealGPU) {
  auto& gpu_2d_real_storage = this->gpu_2d_real_storage;
  serialbox::StorageView gpu_2d_real_storage_view = make_storage_view(gpu_2d_real_storage);

  auto gpu_2d_real_it = gpu_2d_real_storage_view.begin();

  auto gt_view = make_host_view(gpu_2d_real_storage);
  for(int j = 0; j < this->dim2; ++j)
    for(int i = 0; i < this->dim1; ++i, ++gpu_2d_real_it) {
      ASSERT_EQ(gpu_2d_real_it.as<TypeParam>(), gt_view(i, j));
    }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_2DGPU) {
  auto& gpu_2d_storage = this->gpu_2d_storage;
  serialbox::StorageView gpu_2d_storage_view = make_storage_view(gpu_2d_storage);

  auto gpu_2d_it = gpu_2d_storage_view.begin();

  auto gt_view = make_host_view(gpu_2d_storage);
  for(int j = 0; j < this->dim2; ++j)
    for(int i = 0; i < this->dim1; ++i, ++gpu_2d_it) {
      ASSERT_EQ(gpu_2d_it.as<TypeParam>(), gt_view(i, j, 0));
    }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_3DGPU) {
  auto& gpu_3d_storage = this->gpu_3d_storage;
  serialbox::StorageView gpu_3d_storage_view = make_storage_view(gpu_3d_storage);

  auto gpu_3d_it = gpu_3d_storage_view.begin();

  auto gt_view = make_host_view(gpu_3d_storage);
  for(int k = 0; k < this->dim3; ++k)
    for(int j = 0; j < this->dim2; ++j)
      for(int i = 0; i < this->dim1; ++i, ++gpu_3d_it) {
        ASSERT_EQ(gpu_3d_it.as<TypeParam>(), gt_view(i, j, k));
      }
}

TYPED_TEST(GridToolsStorageViewTest, Iterator_4DGPU) {
  auto& gpu_4d_storage = this->gpu_4d_storage;
  serialbox::StorageView gpu_4d_storage_view = make_storage_view(gpu_4d_storage);

  auto gpu_4d_it = gpu_4d_storage_view.begin();

  auto gt_view = make_host_view(gpu_4d_storage);
  for(int l = 0; l < this->dim4; ++l)
    for(int k = 0; k < this->dim3; ++k)
      for(int j = 0; j < this->dim2; ++j)
        for(int i = 0; i < this->dim1; ++i, ++gpu_4d_it) {
          ASSERT_EQ(gpu_4d_it.as<TypeParam>(), gt_view(i, j, k, l));
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
  using meta_data_type = typename GridToolsStorageViewTest<TypeParam>::storage_types::
      storage_traits_type::template custom_layout_storage_info_t<9, layout_type>;
  using storage_type = typename GridToolsStorageViewTest<TypeParam>::storage_types::
      storage_traits_type::template data_store_t<TypeParam, meta_data_type>;

  meta_data_type meta_data(this->dim1, this->dim2, this->dim3);
  storage_type storage(meta_data, -1.0, "storage");
  EXPECT_TRUE(make_storage_view(storage).isMemCopyable());
}

#endif
