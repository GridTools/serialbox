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

#define CXX11_ENABLED
#define STRUCTURED_GRIDS
#define SUPPRESS_MESSAGES

#include "gridtools.hpp"
#include "storage/storage-facility.hpp"

namespace serialbox {

namespace unittest {

/// \brief Define CPU and GPU storage types parametrized on the value type T
template <class T>
struct gridtools_storage_types {
  using value_type = T;

  using storage_traits_type = gridtools::storage_traits<gridtools::enumtype::Host>;

  static constexpr int cpu_alignment = 0;
  static constexpr int gpu_alignment = 32;

  //===----------------------------------------------------------------------------------------===//
  //     Halos
  //===----------------------------------------------------------------------------------------===//
  static constexpr int halo1_left = 1;
  static constexpr int halo2_left = 2;
  static constexpr int halo3_left = 3;
  static constexpr int halo4_left = 4;
  
  static constexpr int halo1_right = 3;
  static constexpr int halo2_right = 4;
  static constexpr int halo3_right = 5;
  static constexpr int halo4_right = 6;

  // Alignment for left halo boundaries
  using halo_2d_type = gridtools::halo<halo1_left, halo2_left>;
  using halo_3d_type = gridtools::halo<halo1_left, halo2_left, halo3_left>;
  using halo_4d_type = gridtools::halo<halo1_left, halo2_left, halo3_left, halo4_left>;

  //===----------------------------------------------------------------------------------------===//
  //     Layouts
  //===----------------------------------------------------------------------------------------===//
  using cpu_2d_real_layout_type = gridtools::layout_map<0, 1>; // stride 1 on j (row-major)
  using gpu_2d_real_layout_type = gridtools::layout_map<1, 0>; // stride 1 on i (col-major)
  using cpu_2d_layout_type = gridtools::layout_map<0, 1, -1>;  // stride 1 on j (row-major)
  using gpu_2d_layout_type = gridtools::layout_map<1, 0, -1>;  // stride 1 on i (col-major)

  using cpu_3d_layout_type = gridtools::layout_map<0, 1, 2>; // stride 1 on k (row-major)
  using gpu_3d_layout_type = gridtools::layout_map<2, 1, 0>; // stride 1 on i (col-major)

  using cpu_4d_layout_type = gridtools::layout_map<0, 1, 2, 3>; // stride 1 on l (row-major)
  using gpu_4d_layout_type = gridtools::layout_map<3, 2, 1, 0>; // stride 1 on i (col-major)

  //===----------------------------------------------------------------------------------------===//
  //     Meta Data
  //===----------------------------------------------------------------------------------------===//
  
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

  //===----------------------------------------------------------------------------------------===//
  //     Storage
  //===----------------------------------------------------------------------------------------===//
  using cpu_2d_real_storage_type = storage_traits_type::storage_type<T, cpu_2d_real_meta_data_type>;
  using gpu_2d_real_storage_type = storage_traits_type::storage_type<T, gpu_2d_real_meta_data_type>;
  using cpu_2d_storage_type = storage_traits_type::storage_type<T, cpu_2d_meta_data_type>;
  using gpu_2d_storage_type = storage_traits_type::storage_type<T, gpu_2d_meta_data_type>;

  using cpu_3d_storage_type = storage_traits_type::storage_type<T, cpu_3d_meta_data_type>;
  using gpu_3d_storage_type = storage_traits_type::storage_type<T, gpu_3d_meta_data_type>;

  using cpu_4d_storage_type = storage_traits_type::storage_type<T, cpu_4d_meta_data_type>;
  using gpu_4d_storage_type = storage_traits_type::storage_type<T, gpu_4d_meta_data_type>;
};

} // namespace unittest

} // namespace serialbox

#endif

#endif
