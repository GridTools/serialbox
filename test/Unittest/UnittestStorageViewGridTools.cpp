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

#include "GridTools.h"
#include "serialbox/Core/StorageView.h"
#include "serialbox/Core/gridtools/StorageView.h"
#include <gtest/gtest.h>
#include <memory>

#ifdef SERIALBOX_HAS_GRIDTOOLS

namespace {

using storage_traits_type = gridtools::storage_traits<gridtools::enumtype::Host>;

static constexpr int cpu_alignment = 0;
static constexpr int gpu_alignment = 32;

//===------------------------------------------------------------------------------------------===//
//     Halos
//===------------------------------------------------------------------------------------------===//
static constexpr std::array<int, 4> halo_left{{1, 2, 3, 4}};
static constexpr std::array<int, 4> halo_right{{3, 4, 5, 6}};

// Alignment for left halo boundaries
using halo_2d_type = gridtools::halo<halo_left[0], halo_left[1]>;
using halo_3d_type = gridtools::halo<halo_left[0], halo_left[1], halo_left[2]>;
using halo_4d_type = gridtools::halo<halo_left[0], halo_left[1], halo_left[2], halo_left[3]>;

//===------------------------------------------------------------------------------------------===//
//     Layouts
//===------------------------------------------------------------------------------------------===//
using cpu_2d_real_layout_type = gridtools::layout_map<0, 1>; // stride 1 on j (row-major)
using gpu_2d_real_layout_type = gridtools::layout_map<1, 0>; // stride 1 on i (col-major)
using cpu_2d_layout_type = gridtools::layout_map<0, 1, -1>;  // stride 1 on j (row-major)
using gpu_2d_layout_type = gridtools::layout_map<1, 0, -1>;  // stride 1 on i (col-major)

using cpu_3d_layout_type = gridtools::layout_map<0, 1, 2>; // stride 1 on k (row-major)
using gpu_3d_layout_type = gridtools::layout_map<2, 1, 0>; // stride 1 on i (col-major)

using cpu_4d_layout_type = gridtools::layout_map<0, 1, 2, 3>; // stride 1 on l (row-major)
using gpu_4d_layout_type = gridtools::layout_map<3, 2, 1, 0>; // stride 1 on i (col-major)

//===------------------------------------------------------------------------------------------===//
//     Meta Data
//===------------------------------------------------------------------------------------------===//
using cpu_2d_real_meta_data_type =
    storage_traits_type::meta_storage_type<1, cpu_2d_real_layout_type, halo_2d_type,
                                           gridtools::aligned<cpu_alignment>>;
using gpu_2d_real_meta_data_type =
    storage_traits_type::meta_storage_type<2, gpu_2d_real_layout_type, halo_2d_type,
                                           gridtools::aligned<gpu_alignment>>;

using cpu_2d_meta_data_type =
    storage_traits_type::meta_storage_type<3, cpu_2d_layout_type, halo_3d_type,
                                           gridtools::aligned<cpu_alignment>>;
using gpu_2d_meta_data_type =
    storage_traits_type::meta_storage_type<4, gpu_2d_layout_type, halo_3d_type,
                                           gridtools::aligned<gpu_alignment>>;

using cpu_3d_meta_data_type =
    storage_traits_type::meta_storage_type<5, cpu_3d_layout_type, halo_3d_type,
                                           gridtools::aligned<cpu_alignment>>;
using gpu_3d_meta_data_type =
    storage_traits_type::meta_storage_type<6, gpu_3d_layout_type, halo_3d_type,
                                           gridtools::aligned<gpu_alignment>>;

using cpu_4d_meta_data_type =
    storage_traits_type::meta_storage_type<7, cpu_4d_layout_type, halo_4d_type,
                                           gridtools::aligned<cpu_alignment>>;
using gpu_4d_meta_data_type =
    storage_traits_type::meta_storage_type<8, gpu_4d_layout_type, halo_4d_type,
                                           gridtools::aligned<gpu_alignment>>;

//===------------------------------------------------------------------------------------------===//
//     Storage
//===------------------------------------------------------------------------------------------===//
using cpu_2d_real_storage_type =
    storage_traits_type::storage_type<double, cpu_2d_real_meta_data_type>;
using gpu_2d_real_storage_type =
    storage_traits_type::storage_type<double, gpu_2d_real_meta_data_type>;
using cpu_2d_storage_type = storage_traits_type::storage_type<double, cpu_2d_meta_data_type>;
using gpu_2d_storage_type = storage_traits_type::storage_type<double, gpu_2d_meta_data_type>;

using cpu_3d_storage_type = storage_traits_type::storage_type<double, cpu_3d_meta_data_type>;
using gpu_3d_storage_type = storage_traits_type::storage_type<double, gpu_3d_meta_data_type>;

using cpu_4d_storage_type = storage_traits_type::storage_type<double, cpu_4d_meta_data_type>;
using gpu_4d_storage_type = storage_traits_type::storage_type<double, gpu_4d_meta_data_type>;

class StorageViewGridToolsTest : public testing::Test {
public:
  // -----------------------------------------------------------------------------------------------
  // Dimensions
  // -----------------------------------------------------------------------------------------------
  int dim1;
  int dim2;
  int dim3;
  int dim4;

  // -----------------------------------------------------------------------------------------------
  // Meta Data
  // -----------------------------------------------------------------------------------------------
  std::shared_ptr<cpu_2d_real_meta_data_type> cpu_2d_real_meta_data_ptr;
  std::shared_ptr<gpu_2d_real_meta_data_type> gpu_2d_real_meta_data_ptr;
  std::shared_ptr<cpu_2d_meta_data_type> cpu_2d_meta_data_ptr;
  std::shared_ptr<gpu_2d_meta_data_type> gpu_2d_meta_data_ptr;
  std::shared_ptr<cpu_3d_meta_data_type> cpu_3d_meta_data_ptr;
  std::shared_ptr<gpu_3d_meta_data_type> gpu_3d_meta_data_ptr;
  std::shared_ptr<cpu_4d_meta_data_type> cpu_4d_meta_data_ptr;
  std::shared_ptr<gpu_4d_meta_data_type> gpu_4d_meta_data_ptr;

  // -----------------------------------------------------------------------------------------------
  // Storages
  // -----------------------------------------------------------------------------------------------
  std::shared_ptr<cpu_2d_real_storage_type> cpu_2d_real_storage_ptr;
  std::shared_ptr<gpu_2d_real_storage_type> gpu_2d_real_storage_ptr;
  std::shared_ptr<cpu_2d_storage_type> cpu_2d_storage_ptr;
  std::shared_ptr<gpu_2d_storage_type> gpu_2d_storage_ptr;
  std::shared_ptr<cpu_3d_storage_type> cpu_3d_storage_ptr;
  std::shared_ptr<gpu_3d_storage_type> gpu_3d_storage_ptr;
  std::shared_ptr<cpu_4d_storage_type> cpu_4d_storage_ptr;
  std::shared_ptr<gpu_4d_storage_type> gpu_4d_storage_ptr;

protected:
  virtual void SetUp() override {
    dim1 = 2 + halo_left[0] + halo_right[0];
    dim2 = 3 + halo_left[1] + halo_right[1];
    dim3 = 4 + halo_left[2] + halo_right[2];
    dim4 = 5 + halo_left[3] + halo_right[3];

    cpu_2d_real_meta_data_ptr = std::make_shared<cpu_2d_real_meta_data_type>(dim1, dim2);
    gpu_2d_real_meta_data_ptr = std::make_shared<gpu_2d_real_meta_data_type>(dim1, dim2);
    cpu_2d_meta_data_ptr = std::make_shared<cpu_2d_meta_data_type>(dim1, dim2, 0);
    gpu_2d_meta_data_ptr = std::make_shared<gpu_2d_meta_data_type>(dim1, dim2, 0);
    cpu_3d_meta_data_ptr = std::make_shared<cpu_3d_meta_data_type>(dim1, dim2, dim3);
    gpu_3d_meta_data_ptr = std::make_shared<gpu_3d_meta_data_type>(dim1, dim2, dim3);
    cpu_4d_meta_data_ptr = std::make_shared<cpu_4d_meta_data_type>(dim1, dim2, dim3, dim4);
    gpu_4d_meta_data_ptr = std::make_shared<gpu_4d_meta_data_type>(dim1, dim2, dim3, dim4);

    cpu_2d_real_storage_ptr = std::make_shared<cpu_2d_real_storage_type>(
        *cpu_2d_real_meta_data_ptr, "cpu_2d_real_storage", -1.0);
    gpu_2d_real_storage_ptr = std::make_shared<gpu_2d_real_storage_type>(
        *gpu_2d_real_meta_data_ptr, "gpu_2d_real_storage", -1.0);

    cpu_2d_storage_ptr =
        std::make_shared<cpu_2d_storage_type>(*cpu_2d_meta_data_ptr, "cpu_2d_storage", -1.0);
    gpu_2d_storage_ptr =
        std::make_shared<gpu_2d_storage_type>(*gpu_2d_meta_data_ptr, "gpu_2d_storage", -1.0);

    cpu_3d_storage_ptr =
        std::make_shared<cpu_3d_storage_type>(*cpu_3d_meta_data_ptr, "cpu_3d_storage", -1.0);
    gpu_3d_storage_ptr =
        std::make_shared<gpu_3d_storage_type>(*gpu_3d_meta_data_ptr, "gpu_3d_storage", -1.0);

    cpu_4d_storage_ptr =
        std::make_shared<cpu_4d_storage_type>(*cpu_4d_meta_data_ptr, "cpu_4d_storage", -1.0);
    gpu_4d_storage_ptr =
        std::make_shared<gpu_4d_storage_type>(*gpu_4d_meta_data_ptr, "gpu_4d_storage", -1.0);

    // 2D
    double val_2d = 0.0;
    for(int j = 0; j < dim2; ++j)
      for(int i = 0; i < dim1; ++i, val_2d += 1.0) {
        (*cpu_2d_real_storage_ptr)(i, j) = val_2d;
        (*gpu_2d_real_storage_ptr)(i, j) = val_2d;
        (*cpu_2d_storage_ptr)(i, j, 0) = val_2d;
        (*gpu_2d_storage_ptr)(i, j, 0) = val_2d;
      }

    // 3D
    double val_3d = 0.0;
    for(int k = 0; k < dim3; ++k)
      for(int j = 0; j < dim2; ++j)
        for(int i = 0; i < dim1; ++i, val_3d += 1.0) {
          (*cpu_3d_storage_ptr)(i, j, k) = val_3d;
          (*gpu_3d_storage_ptr)(i, j, k) = val_3d;
        }

    // 4D
    double val_4d = 0.0;
    for(int l = 0; l < dim4; ++l)
      for(int k = 0; k < dim3; ++k)
        for(int j = 0; j < dim2; ++j)
          for(int i = 0; i < dim1; ++i, val_4d += 1.0) {
            (*cpu_4d_storage_ptr)(i, j, k, l) = val_4d;
            (*gpu_4d_storage_ptr)(i, j, k, l) = val_4d;
          }
  }

  virtual void TearDown() override {}
};

} // anonymous namespace

template <typename Storage>
serialbox::StorageView make_storage_view(const Storage& storage) {
  using namespace serialbox::gridtools;

  std::vector<int> dims(internal::get_dims(storage));
  std::vector<int> strides(internal::get_strides(storage));
  std::vector<std::pair<int, int>> padding(internal::get_padding(storage));
  void* data = internal::get_data_pointer(storage, 0);

  return serialbox::StorageView(data, serialbox::ToTypeID<double>::value, std::move(dims),
                                std::move(strides), std::move(padding));
}

TEST_F(StorageViewGridToolsTest, Construction) {
  using namespace serialbox::gridtools;

  // -----------------------------------------------------------------------------------------------
  // 2D Real CPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& cpu_2d_real_storage = *cpu_2d_real_storage_ptr;
  auto& cpu_2d_real_meta_data = *cpu_2d_real_meta_data_ptr;

  std::vector<int> cpu_2d_real_dims(internal::get_dims(cpu_2d_real_storage));
  std::vector<int> cpu_2d_real_strides(internal::get_strides(cpu_2d_real_storage));
  std::vector<std::pair<int, int>> cpu_2d_real_padding(internal::get_padding(cpu_2d_real_storage));
  void* cpu_2d_real_data = internal::get_data_pointer(cpu_2d_real_storage, 0);

  // Dimensions
  EXPECT_EQ(cpu_2d_real_dims[0], cpu_2d_real_meta_data.template unaligned_dim<0>());
  EXPECT_EQ(cpu_2d_real_dims[1], cpu_2d_real_meta_data.template unaligned_dim<1>());

  // Strides
  EXPECT_EQ(cpu_2d_real_strides[0], cpu_2d_real_meta_data.template strides<0>());
  EXPECT_EQ(cpu_2d_real_strides[1], cpu_2d_real_meta_data.template strides<1>());

  // Padding
  EXPECT_EQ(cpu_2d_real_padding[0].first, 0);
  EXPECT_EQ(cpu_2d_real_padding[1].first, 0);
  EXPECT_EQ(cpu_2d_real_padding[0].second, 0);
  EXPECT_EQ(cpu_2d_real_padding[1].second, 0);

  // Data
  EXPECT_NE(cpu_2d_real_data, nullptr);

  // -----------------------------------------------------------------------------------------------
  // 2D CPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& cpu_2d_storage = *cpu_2d_storage_ptr;
  auto& cpu_2d_meta_data = *cpu_2d_meta_data_ptr;

  std::vector<int> cpu_2d_dims(internal::get_dims(cpu_2d_storage));
  std::vector<int> cpu_2d_strides(internal::get_strides(cpu_2d_storage));
  std::vector<std::pair<int, int>> cpu_2d_padding(internal::get_padding(cpu_2d_storage));
  void* cpu_2d_data = internal::get_data_pointer(cpu_2d_storage, 0);

  // Dimensions
  EXPECT_EQ(cpu_2d_dims[0], cpu_2d_meta_data.template unaligned_dim<0>());
  EXPECT_EQ(cpu_2d_dims[1], cpu_2d_meta_data.template unaligned_dim<1>());
  EXPECT_EQ(cpu_2d_dims[2], cpu_2d_meta_data.template unaligned_dim<2>());

  // Strides
  EXPECT_EQ(cpu_2d_strides[0], cpu_2d_meta_data.template strides<0>());
  EXPECT_EQ(cpu_2d_strides[1], cpu_2d_meta_data.template strides<1>());
  EXPECT_EQ(cpu_2d_strides[2], cpu_2d_meta_data.template strides<2>());

  // Padding
  EXPECT_EQ(cpu_2d_padding[0].first, 0);
  EXPECT_EQ(cpu_2d_padding[1].first, 0);
  EXPECT_EQ(cpu_2d_padding[2].first, 0);
  EXPECT_EQ(cpu_2d_padding[0].second, 0);
  EXPECT_EQ(cpu_2d_padding[1].second, 0);
  EXPECT_EQ(cpu_2d_padding[2].second, 0);

  // Data
  EXPECT_NE(cpu_2d_data, nullptr);

  // -----------------------------------------------------------------------------------------------
  // 3D CPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& cpu_3d_storage = *cpu_3d_storage_ptr;
  auto& cpu_3d_meta_data = *cpu_3d_meta_data_ptr;

  std::vector<int> cpu_3d_dims(internal::get_dims(cpu_3d_storage));
  std::vector<int> cpu_3d_strides(internal::get_strides(cpu_3d_storage));
  std::vector<std::pair<int, int>> cpu_3d_padding(internal::get_padding(cpu_3d_storage));
  void* cpu_3d_data = internal::get_data_pointer(cpu_3d_storage, 0);

  // Dimensions
  EXPECT_EQ(cpu_3d_dims[0], cpu_3d_meta_data.template unaligned_dim<0>());
  EXPECT_EQ(cpu_3d_dims[1], cpu_3d_meta_data.template unaligned_dim<1>());
  EXPECT_EQ(cpu_3d_dims[2], cpu_3d_meta_data.template unaligned_dim<2>());

  // Strides
  EXPECT_EQ(cpu_3d_strides[0], cpu_3d_meta_data.template strides<0>());
  EXPECT_EQ(cpu_3d_strides[1], cpu_3d_meta_data.template strides<1>());
  EXPECT_EQ(cpu_3d_strides[2], cpu_3d_meta_data.template strides<2>());

  // Padding
  EXPECT_EQ(cpu_3d_padding[0].first, 0);
  EXPECT_EQ(cpu_3d_padding[1].first, 0);
  EXPECT_EQ(cpu_3d_padding[2].first, 0);
  EXPECT_EQ(cpu_3d_padding[0].second, 0);
  EXPECT_EQ(cpu_3d_padding[1].second, 0);
  EXPECT_EQ(cpu_3d_padding[2].second, 0);

  // Data
  EXPECT_NE(cpu_3d_data, nullptr);

  // -----------------------------------------------------------------------------------------------
  // 4D CPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& cpu_4d_storage = *cpu_4d_storage_ptr;
  auto& cpu_4d_meta_data = *cpu_4d_meta_data_ptr;

  std::vector<int> cpu_4d_dims(internal::get_dims(cpu_4d_storage));
  std::vector<int> cpu_4d_strides(internal::get_strides(cpu_4d_storage));
  std::vector<std::pair<int, int>> cpu_4d_padding(internal::get_padding(cpu_4d_storage));
  void* cpu_4d_data = internal::get_data_pointer(cpu_4d_storage, 0);

  // Dimensions
  EXPECT_EQ(cpu_4d_dims[0], cpu_4d_meta_data.template unaligned_dim<0>());
  EXPECT_EQ(cpu_4d_dims[1], cpu_4d_meta_data.template unaligned_dim<1>());
  EXPECT_EQ(cpu_4d_dims[2], cpu_4d_meta_data.template unaligned_dim<2>());
  EXPECT_EQ(cpu_4d_dims[3], cpu_4d_meta_data.template unaligned_dim<3>());

  // Strides
  EXPECT_EQ(cpu_4d_strides[0], cpu_4d_meta_data.template strides<0>());
  EXPECT_EQ(cpu_4d_strides[1], cpu_4d_meta_data.template strides<1>());
  EXPECT_EQ(cpu_4d_strides[2], cpu_4d_meta_data.template strides<2>());
  EXPECT_EQ(cpu_4d_strides[3], cpu_4d_meta_data.template strides<3>());

  // Padding
  EXPECT_EQ(cpu_4d_padding[0].first, 0);
  EXPECT_EQ(cpu_4d_padding[1].first, 0);
  EXPECT_EQ(cpu_4d_padding[2].first, 0);
  EXPECT_EQ(cpu_4d_padding[3].first, 0);

  EXPECT_EQ(cpu_4d_padding[0].second, 0);
  EXPECT_EQ(cpu_4d_padding[1].second, 0);
  EXPECT_EQ(cpu_4d_padding[2].second, 0);
  EXPECT_EQ(cpu_4d_padding[3].second, 0);

  // Data
  EXPECT_NE(cpu_4d_data, nullptr);

  // -----------------------------------------------------------------------------------------------
  // 2D GPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& gpu_2d_real_storage = *gpu_2d_real_storage_ptr;
  auto& gpu_2d_real_meta_data = *gpu_2d_real_meta_data_ptr;

  std::vector<int> gpu_2d_real_dims(internal::get_dims(gpu_2d_real_storage));
  std::vector<int> gpu_2d_real_strides(internal::get_strides(gpu_2d_real_storage));
  std::vector<std::pair<int, int>> gpu_2d_real_padding(internal::get_padding(gpu_2d_real_storage));
  void* gpu_2d_real_data = internal::get_data_pointer(gpu_2d_real_storage, 0);

  // Dimensions
  EXPECT_EQ(gpu_2d_real_dims[0], gpu_2d_real_meta_data.template unaligned_dim<0>());
  EXPECT_EQ(gpu_2d_real_dims[1], gpu_2d_real_meta_data.template unaligned_dim<1>());

  // Strides
  EXPECT_EQ(gpu_2d_real_strides[0], gpu_2d_real_meta_data.template strides<0>());
  EXPECT_EQ(gpu_2d_real_strides[1], gpu_2d_real_meta_data.template strides<1>());

  // Padding
  EXPECT_EQ(gpu_2d_real_padding[0].first,
            (internal::has_stride_one<gpu_2d_real_meta_data_type::layout>(0)
                 ? (gpu_alignment - halo_left[0]) % gpu_alignment
                 : 0));
  EXPECT_EQ(gpu_2d_real_padding[1].first,
            (internal::has_stride_one<gpu_2d_real_meta_data_type::layout>(1)
                 ? (gpu_alignment - halo_left[1]) % gpu_alignment
                 : 0));

  EXPECT_EQ(
      gpu_2d_real_padding[0].second,
      (internal::has_stride_one<gpu_2d_real_meta_data_type::layout>(0)
           ? (gpu_2d_real_meta_data.template dim<0>() -
              gpu_2d_real_meta_data.template unaligned_dim<0>() - gpu_2d_real_padding[0].first)
           : 0));
  EXPECT_EQ(
      gpu_2d_real_padding[1].second,
      (internal::has_stride_one<gpu_2d_real_meta_data_type::layout>(1)
           ? (gpu_2d_real_meta_data.template dim<1>() -
              gpu_2d_real_meta_data.template unaligned_dim<1>() - gpu_2d_real_padding[1].first)
           : 0));

  // Data
  EXPECT_NE(gpu_2d_real_data, nullptr);

  // -----------------------------------------------------------------------------------------------
  // 2D GPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& gpu_2d_storage = *gpu_2d_storage_ptr;
  auto& gpu_2d_meta_data = *gpu_2d_meta_data_ptr;

  std::vector<int> gpu_2d_dims(internal::get_dims(gpu_2d_storage));
  std::vector<int> gpu_2d_strides(internal::get_strides(gpu_2d_storage));
  std::vector<std::pair<int, int>> gpu_2d_padding(internal::get_padding(gpu_2d_storage));
  void* gpu_2d_data = internal::get_data_pointer(gpu_2d_storage, 0);

  // Dimensions
  EXPECT_EQ(gpu_2d_dims[0], gpu_2d_meta_data.template unaligned_dim<0>());
  EXPECT_EQ(gpu_2d_dims[1], gpu_2d_meta_data.template unaligned_dim<1>());
  EXPECT_EQ(gpu_2d_dims[2], gpu_2d_meta_data.template unaligned_dim<2>());

  // Strides
  EXPECT_EQ(gpu_2d_strides[0], gpu_2d_meta_data.template strides<0>());
  EXPECT_EQ(gpu_2d_strides[1], gpu_2d_meta_data.template strides<1>());
  EXPECT_EQ(gpu_2d_strides[2], gpu_2d_meta_data.template strides<2>());

  // Padding
  EXPECT_EQ(gpu_2d_padding[0].first, (internal::has_stride_one<gpu_2d_meta_data_type::layout>(0)
                                          ? (gpu_alignment - halo_left[0]) % gpu_alignment
                                          : 0));
  EXPECT_EQ(gpu_2d_padding[1].first, (internal::has_stride_one<gpu_2d_meta_data_type::layout>(1)
                                          ? (gpu_alignment - halo_left[1]) % gpu_alignment
                                          : 0));
  EXPECT_EQ(gpu_2d_padding[2].first, (internal::has_stride_one<gpu_2d_meta_data_type::layout>(2)
                                          ? (gpu_alignment - halo_left[2]) % gpu_alignment
                                          : 0));

  EXPECT_EQ(gpu_2d_padding[0].second,
            (internal::has_stride_one<gpu_2d_meta_data_type::layout>(0)
                 ? (gpu_2d_meta_data.template dim<0>() -
                    gpu_2d_meta_data.template unaligned_dim<0>() - gpu_2d_padding[0].first)
                 : 0));
  EXPECT_EQ(gpu_2d_padding[1].second,
            (internal::has_stride_one<gpu_2d_meta_data_type::layout>(1)
                 ? (gpu_2d_meta_data.template dim<1>() -
                    gpu_2d_meta_data.template unaligned_dim<1>() - gpu_2d_padding[1].first)
                 : 0));
  EXPECT_EQ(gpu_2d_padding[2].second,
            (internal::has_stride_one<gpu_2d_meta_data_type::layout>(2)
                 ? (gpu_2d_meta_data.template dim<2>() -
                    gpu_2d_meta_data.template unaligned_dim<2>() - gpu_2d_padding[2].first)
                 : 0));

  // Data
  EXPECT_NE(gpu_2d_data, nullptr);

  // -----------------------------------------------------------------------------------------------
  // 3D GPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& gpu_3d_storage = *gpu_3d_storage_ptr;
  auto& gpu_3d_meta_data = *gpu_3d_meta_data_ptr;

  std::vector<int> gpu_3d_dims(internal::get_dims(gpu_3d_storage));
  std::vector<int> gpu_3d_strides(internal::get_strides(gpu_3d_storage));
  std::vector<std::pair<int, int>> gpu_3d_padding(internal::get_padding(gpu_3d_storage));
  void* gpu_3d_data = internal::get_data_pointer(gpu_3d_storage, 0);

  // Dimensions
  EXPECT_EQ(gpu_3d_dims[0], gpu_3d_meta_data.template unaligned_dim<0>());
  EXPECT_EQ(gpu_3d_dims[1], gpu_3d_meta_data.template unaligned_dim<1>());
  EXPECT_EQ(gpu_3d_dims[2], gpu_3d_meta_data.template unaligned_dim<2>());

  // Strides
  EXPECT_EQ(gpu_3d_strides[0], gpu_3d_meta_data.template strides<0>());
  EXPECT_EQ(gpu_3d_strides[1], gpu_3d_meta_data.template strides<1>());
  EXPECT_EQ(gpu_3d_strides[2], gpu_3d_meta_data.template strides<2>());

  // Padding
  EXPECT_EQ(gpu_3d_padding[0].first, (internal::has_stride_one<gpu_3d_meta_data_type::layout>(0)
                                          ? (gpu_alignment - halo_left[0]) % gpu_alignment
                                          : 0));
  EXPECT_EQ(gpu_3d_padding[1].first, (internal::has_stride_one<gpu_3d_meta_data_type::layout>(1)
                                          ? (gpu_alignment - halo_left[1]) % gpu_alignment
                                          : 0));
  EXPECT_EQ(gpu_3d_padding[2].first, (internal::has_stride_one<gpu_3d_meta_data_type::layout>(2)
                                          ? (gpu_alignment - halo_left[2]) % gpu_alignment
                                          : 0));

  EXPECT_EQ(gpu_3d_padding[0].second,
            (internal::has_stride_one<gpu_3d_meta_data_type::layout>(0)
                 ? (gpu_3d_meta_data.template dim<0>() -
                    gpu_3d_meta_data.template unaligned_dim<0>() - gpu_3d_padding[0].first)
                 : 0));
  EXPECT_EQ(gpu_3d_padding[1].second,
            (internal::has_stride_one<gpu_3d_meta_data_type::layout>(1)
                 ? (gpu_3d_meta_data.template dim<1>() -
                    gpu_3d_meta_data.template unaligned_dim<1>() - gpu_3d_padding[1].first)
                 : 0));
  EXPECT_EQ(gpu_3d_padding[2].second,
            (internal::has_stride_one<gpu_3d_meta_data_type::layout>(2)
                 ? (gpu_3d_meta_data.template dim<2>() -
                    gpu_3d_meta_data.template unaligned_dim<2>() - gpu_3d_padding[2].first)
                 : 0));

  // Data
  EXPECT_NE(gpu_3d_data, nullptr);

  // -----------------------------------------------------------------------------------------------
  // 4D GPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& gpu_4d_storage = *gpu_4d_storage_ptr;
  auto& gpu_4d_meta_data = *gpu_4d_meta_data_ptr;

  std::vector<int> gpu_4d_dims(internal::get_dims(gpu_4d_storage));
  std::vector<int> gpu_4d_strides(internal::get_strides(gpu_4d_storage));
  std::vector<std::pair<int, int>> gpu_4d_padding(internal::get_padding(gpu_4d_storage));
  void* gpu_4d_data = internal::get_data_pointer(gpu_4d_storage, 0);

  // Dimensions
  EXPECT_EQ(gpu_4d_dims[0], gpu_4d_meta_data.template unaligned_dim<0>());
  EXPECT_EQ(gpu_4d_dims[1], gpu_4d_meta_data.template unaligned_dim<1>());
  EXPECT_EQ(gpu_4d_dims[2], gpu_4d_meta_data.template unaligned_dim<2>());
  EXPECT_EQ(gpu_4d_dims[3], gpu_4d_meta_data.template unaligned_dim<3>());

  // Strides
  EXPECT_EQ(gpu_4d_strides[0], gpu_4d_meta_data.template strides<0>());
  EXPECT_EQ(gpu_4d_strides[1], gpu_4d_meta_data.template strides<1>());
  EXPECT_EQ(gpu_4d_strides[2], gpu_4d_meta_data.template strides<2>());
  EXPECT_EQ(gpu_4d_strides[3], gpu_4d_meta_data.template strides<3>());

  // Padding
  EXPECT_EQ(gpu_4d_padding[0].first, (internal::has_stride_one<gpu_4d_meta_data_type::layout>(0)
                                          ? (gpu_alignment - halo_left[0]) % gpu_alignment
                                          : 0));
  EXPECT_EQ(gpu_4d_padding[1].first, (internal::has_stride_one<gpu_4d_meta_data_type::layout>(1)
                                          ? (gpu_alignment - halo_left[1]) % gpu_alignment
                                          : 0));
  EXPECT_EQ(gpu_4d_padding[2].first, (internal::has_stride_one<gpu_4d_meta_data_type::layout>(2)
                                          ? (gpu_alignment - halo_left[2]) % gpu_alignment
                                          : 0));
  EXPECT_EQ(gpu_4d_padding[3].first, (internal::has_stride_one<gpu_4d_meta_data_type::layout>(3)
                                          ? (gpu_alignment - halo_left[3]) % gpu_alignment
                                          : 0));

  EXPECT_EQ(gpu_4d_padding[0].second,
            (internal::has_stride_one<gpu_4d_meta_data_type::layout>(0)
                 ? (gpu_4d_meta_data.template dim<0>() -
                    gpu_4d_meta_data.template unaligned_dim<0>() - gpu_4d_padding[0].first)
                 : 0));
  EXPECT_EQ(gpu_4d_padding[1].second,
            (internal::has_stride_one<gpu_4d_meta_data_type::layout>(1)
                 ? (gpu_4d_meta_data.template dim<1>() -
                    gpu_4d_meta_data.template unaligned_dim<1>() - gpu_4d_padding[1].first)
                 : 0));
  EXPECT_EQ(gpu_4d_padding[2].second,
            (internal::has_stride_one<gpu_4d_meta_data_type::layout>(2)
                 ? (gpu_4d_meta_data.template dim<2>() -
                    gpu_4d_meta_data.template unaligned_dim<2>() - gpu_4d_padding[2].first)
                 : 0));
  EXPECT_EQ(gpu_4d_padding[3].second,
            (internal::has_stride_one<gpu_4d_meta_data_type::layout>(3)
                 ? (gpu_4d_meta_data.template dim<3>() -
                    gpu_4d_meta_data.template unaligned_dim<3>() - gpu_4d_padding[3].first)
                 : 0));

  // Data
  EXPECT_NE(gpu_4d_data, nullptr);
}

TEST_F(StorageViewGridToolsTest, Iterator) {

  // -----------------------------------------------------------------------------------------------
  // 2D Real CPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& cpu_2d_real_storage = *cpu_2d_real_storage_ptr;
  serialbox::StorageView cpu_2d_real_storage_view = make_storage_view(cpu_2d_real_storage);

  auto cpu_2d_real_it = cpu_2d_real_storage_view.begin();
  for(int j = 0; j < dim2; ++j)
    for(int i = 0; i < dim1; ++i, ++cpu_2d_real_it) {
      ASSERT_DOUBLE_EQ(cpu_2d_real_it.as<double>(), cpu_2d_real_storage(i, j));
    }

  // -----------------------------------------------------------------------------------------------
  // 2D CPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& cpu_2d_storage = *cpu_2d_storage_ptr;
  serialbox::StorageView cpu_2d_storage_view = make_storage_view(cpu_2d_storage);

  auto cpu_2d_it = cpu_2d_storage_view.begin();
  for(int j = 0; j < dim2; ++j)
    for(int i = 0; i < dim1; ++i, ++cpu_2d_it) {
      ASSERT_DOUBLE_EQ(cpu_2d_it.as<double>(), cpu_2d_storage(i, j, 0));
    }

  // -----------------------------------------------------------------------------------------------
  // 3D CPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& cpu_3d_storage = *cpu_3d_storage_ptr;
  serialbox::StorageView cpu_3d_storage_view = make_storage_view(cpu_3d_storage);

  auto cpu_3d_it = cpu_3d_storage_view.begin();
  for(int k = 0; k < dim3; ++k)
    for(int j = 0; j < dim2; ++j)
      for(int i = 0; i < dim1; ++i, ++cpu_3d_it) {
        ASSERT_DOUBLE_EQ(cpu_3d_it.as<double>(), cpu_3d_storage(i, j, k));
      }

  // -----------------------------------------------------------------------------------------------
  // 4D CPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& cpu_4d_storage = *cpu_4d_storage_ptr;
  serialbox::StorageView cpu_4d_storage_view = make_storage_view(cpu_4d_storage);

  auto cpu_4d_it = cpu_4d_storage_view.begin();
  for(int l = 0; l < dim4; ++l)
    for(int k = 0; k < dim3; ++k)
      for(int j = 0; j < dim2; ++j)
        for(int i = 0; i < dim1; ++i, ++cpu_4d_it) {
          ASSERT_DOUBLE_EQ(cpu_4d_it.as<double>(), cpu_4d_storage(i, j, k, l));
        }
  
  // -----------------------------------------------------------------------------------------------
  // 2D Real GPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& gpu_2d_real_storage = *gpu_2d_real_storage_ptr;
  serialbox::StorageView gpu_2d_real_storage_view = make_storage_view(gpu_2d_real_storage);

  auto gpu_2d_real_it = gpu_2d_real_storage_view.begin();
  for(int j = 0; j < dim2; ++j)
    for(int i = 0; i < dim1; ++i, ++gpu_2d_real_it) {
      ASSERT_DOUBLE_EQ(gpu_2d_real_it.as<double>(), gpu_2d_real_storage(i, j));
    }
  
  // -----------------------------------------------------------------------------------------------
  // 2D GPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& gpu_2d_storage = *gpu_2d_storage_ptr;
  serialbox::StorageView gpu_2d_storage_view = make_storage_view(gpu_2d_storage);

  auto gpu_2d_it = gpu_2d_storage_view.begin();
  for(int j = 0; j < dim2; ++j)
    for(int i = 0; i < dim1; ++i, ++gpu_2d_it) {
      ASSERT_DOUBLE_EQ(gpu_2d_it.as<double>(), gpu_2d_storage(i, j, 0));
    }

  // -----------------------------------------------------------------------------------------------
  // 3D GPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& gpu_3d_storage = *gpu_3d_storage_ptr;
  serialbox::StorageView gpu_3d_storage_view = make_storage_view(gpu_3d_storage);

  auto gpu_3d_it = gpu_3d_storage_view.begin();
  for(int k = 0; k < dim3; ++k)
    for(int j = 0; j < dim2; ++j)
      for(int i = 0; i < dim1; ++i, ++gpu_3d_it) {
        ASSERT_DOUBLE_EQ(gpu_3d_it.as<double>(), gpu_3d_storage(i, j, k));
      }

  // -----------------------------------------------------------------------------------------------
  // 4D GPU Storage
  // -----------------------------------------------------------------------------------------------

  auto& gpu_4d_storage = *gpu_4d_storage_ptr;
  serialbox::StorageView gpu_4d_storage_view = make_storage_view(gpu_4d_storage);

  auto gpu_4d_it = gpu_4d_storage_view.begin();
  for(int l = 0; l < dim4; ++l)
    for(int k = 0; k < dim3; ++k)
      for(int j = 0; j < dim2; ++j)
        for(int i = 0; i < dim1; ++i, ++gpu_4d_it) {
          ASSERT_DOUBLE_EQ(gpu_4d_it.as<double>(), gpu_4d_storage(i, j, k, l));
        }
}

TEST_F(StorageViewGridToolsTest, isMemCopyable) {
  EXPECT_FALSE(make_storage_view(*cpu_2d_real_storage_ptr).isMemCopyable());
  EXPECT_FALSE(make_storage_view(*gpu_2d_real_storage_ptr).isMemCopyable());
  EXPECT_FALSE(make_storage_view(*cpu_2d_storage_ptr).isMemCopyable());
  EXPECT_FALSE(make_storage_view(*gpu_2d_storage_ptr).isMemCopyable());
  EXPECT_FALSE(make_storage_view(*cpu_3d_storage_ptr).isMemCopyable());
  EXPECT_FALSE(make_storage_view(*gpu_3d_storage_ptr).isMemCopyable());
  EXPECT_FALSE(make_storage_view(*cpu_4d_storage_ptr).isMemCopyable());
  EXPECT_FALSE(make_storage_view(*gpu_4d_storage_ptr).isMemCopyable());

  // Create a memcopyable stroage
  using layout_type = gridtools::layout_map<2, 1, 0>; // stride 1 on i (col-major)
  using meta_data_type = storage_traits_type::meta_storage_type<9, layout_type>;
  using storage_type = storage_traits_type::storage_type<double, meta_data_type>;

  meta_data_type meta_data(dim1, dim2, dim3);
  storage_type storage(meta_data, "storage", -1.0);
  EXPECT_TRUE(make_storage_view(storage).isMemCopyable());
}

#endif
