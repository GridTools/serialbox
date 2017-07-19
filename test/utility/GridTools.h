//===-- utility/GridTools.h ---------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file includes all the necessary gridtools headers and defines storage types.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UTILITY_GRIDTOOLS_H
#define SERIALBOX_UTILITY_GRIDTOOLS_H

#include "utility/Config.h"

#ifdef SERIALBOX_HAS_GRIDTOOLS

#define STRUCTURED_GRIDS
#define SUPPRESS_MESSAGES

#include "gridtools.hpp"
#include "storage/storage-facility.hpp"

namespace serialbox {

namespace unittest {

/// \brief Define CPU and GPU storage types parameterized on the value type T
template <class T>
struct gridtools_storage_types {
  using value_type = T;

  using storage_traits_type = gridtools::storage_traits<gridtools::enumtype::Host>;

  static constexpr int gpu_alignment = 32;

  //===----------------------------------------------------------------------------------------===//
  //     Halos
  //===----------------------------------------------------------------------------------------===//
  static constexpr int halo1_left = 1;
  static constexpr int halo2_left = 2;
  static constexpr int halo3_left = 3;
  static constexpr int halo4_left = 4;

  static constexpr int halo1_right = halo1_left;
  static constexpr int halo2_right = halo2_left;
  static constexpr int halo3_right = halo3_left;
  static constexpr int halo4_right = halo4_left;

  // Alignment for left halo boundaries
  using halo_2d_type = gridtools::halo<halo1_left, halo2_left>;
  using halo_3d_type = gridtools::halo<halo1_left, halo2_left, halo3_left>;
  using halo_4d_type = gridtools::halo<halo1_left, halo2_left, halo3_left, halo4_left>;

  // Layout maps to use GPU layouts with GPU compilation of gridtools disabled.
  // TODO should be replaced by a real GPU test (a CUDA compiler would be required)
  using gpu_2d_real_layout_type = gridtools::layout_map<1, 0>; // stride 1 on i (col-major)
  using gpu_2d_layout_type = gridtools::layout_map<1, 0, -1>;
  using gpu_3d_layout_type = gridtools::layout_map<2, 1, 0>;
  using gpu_4d_layout_type = gridtools::layout_map<3, 2, 1, 0>;

  // Storage Info
  using cpu_2d_real_meta_data_type = storage_traits_type::storage_info_t<1, 2, halo_2d_type>;
  using gpu_2d_real_meta_data_type =
      gridtools::host_storage_info<2, gpu_2d_real_layout_type, halo_2d_type,
                                   gridtools::alignment<gpu_alignment>>;
  using cpu_2d_meta_data_type =
      storage_traits_type::special_storage_info_t<3, gridtools::selector<1, 1, 0>, halo_3d_type>;
  using gpu_2d_meta_data_type = gridtools::host_storage_info<4, gpu_2d_layout_type, halo_3d_type,
                                                             gridtools::alignment<gpu_alignment>>;
  using cpu_3d_meta_data_type = storage_traits_type::storage_info_t<5, 3, halo_3d_type>;
  using gpu_3d_meta_data_type = gridtools::host_storage_info<6, gpu_3d_layout_type, halo_3d_type,
                                                             gridtools::alignment<gpu_alignment>>;
  using cpu_4d_meta_data_type = storage_traits_type::storage_info_t<7, 4, halo_4d_type>;
  using gpu_4d_meta_data_type = gridtools::host_storage_info<8, gpu_4d_layout_type, halo_4d_type,
                                                             gridtools::alignment<gpu_alignment>>;

  // Storage
  using cpu_2d_real_storage_type = storage_traits_type::data_store_t<T, cpu_2d_real_meta_data_type>;
  using gpu_2d_real_storage_type = storage_traits_type::data_store_t<T, gpu_2d_real_meta_data_type>;
  using cpu_2d_storage_type = storage_traits_type::data_store_t<T, cpu_2d_meta_data_type>;
  using gpu_2d_storage_type = storage_traits_type::data_store_t<T, gpu_2d_meta_data_type>;
  using cpu_3d_storage_type = storage_traits_type::data_store_t<T, cpu_3d_meta_data_type>;
  using gpu_3d_storage_type = storage_traits_type::data_store_t<T, gpu_3d_meta_data_type>;
  using cpu_4d_storage_type = storage_traits_type::data_store_t<T, cpu_4d_meta_data_type>;
  using gpu_4d_storage_type = storage_traits_type::data_store_t<T, gpu_4d_meta_data_type>;

  template <typename Storage>
  static void init2DReal(Storage& storage, int dim1, int dim2) {
    T val_2d = 0.0;
    for(int j = 0; j < dim2; ++j)
      for(int i = 0; i < dim1; ++i, val_2d += 1.0) {
        auto view = make_host_view(storage);
        view(i, j) = val_2d;
      }
  }

  template <typename Storage>
  static void init2D(Storage& storage, int dim1, int dim2) {
    T val_2d = 0.0;
    for(int j = 0; j < dim2; ++j)
      for(int i = 0; i < dim1; ++i, val_2d += 1.0) {
        auto view = make_host_view(storage);
        view(i, j, 0) = val_2d;
      }
  }

  template <typename Storage>
  static void init3D(Storage& storage, int dim1, int dim2, int dim3) {
    T val_3d = 0.0;
    for(int k = 0; k < dim3; ++k)
      for(int j = 0; j < dim2; ++j)
        for(int i = 0; i < dim1; ++i, val_3d += 1.0) {
          auto view = make_host_view(storage);
          view(i, j, k) = val_3d;
        }
  }

  template <typename Storage>
  static void init4D(Storage& storage, int dim1, int dim2, int dim3, int dim4) {
    T val_4d = 0.0;
    for(int l = 0; l < dim4; ++l)
      for(int k = 0; k < dim3; ++k)
        for(int j = 0; j < dim2; ++j)
          for(int i = 0; i < dim1; ++i, val_4d += 1.0) {
            auto view = make_host_view(storage);
            view(i, j, k, l) = val_4d;
          }
  }
};

} // namespace unittest

} // namespace serialbox

#endif

#endif
