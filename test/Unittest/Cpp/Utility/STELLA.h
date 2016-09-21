//===-- Unittest/Cpp/Utility/STELLA.h -----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file includes all the necessary STELLA headers and defines storage types.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UNITTEST_CPP_UTILITY_STELLA_H
#define SERIALBOX_UNITTEST_CPP_UTILITY_STELLA_H

#include "Utility/CppConfig.h"

#ifdef SERIALBOX_HAS_STELLA

#include "SharedInfrastructure.h"

namespace serialbox {

namespace unittest {

/// \brief Define CPU and GPU storage types parametrized on the value type T
template <class T>
struct STELLAStorageTypes {
  using value_type = T;

  //===----------------------------------------------------------------------------------------===//
  //     Alignment
  //===----------------------------------------------------------------------------------------===//
  using cpu_alignment = DataFieldAlignment<cDimK, 1>;
  using cpu_ijk_boundary = DataFieldIJBoundary<-cNumBoundaryLines, cNumBoundaryLines,
                                               -cNumBoundaryLines, cNumBoundaryLines>;

  using gpu_alignment = DataFieldAlignment<cDimI, 2 * cCacheLineSize / sizeof(T)>;
  using gpu_ijk_boundary = DataFieldIJBoundary<-cNumBoundaryLines, cNumBoundaryLines,
                                               -cNumBoundaryLines, cNumBoundaryLines>;

  //===----------------------------------------------------------------------------------------===//
  //     CPU Storage
  //===----------------------------------------------------------------------------------------===//
  using cpu_jik_storage =
      DataFieldOpenMP<T,
                      DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::JIK, cpu_alignment>>;

  using cpu_ji_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::JI, cpu_alignment>>;
  using cpu_ik_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::IK, cpu_alignment>>;
  using cpu_jk_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::JK, cpu_alignment>>;

  using cpu_i_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::I, cpu_alignment>>;
  using cpu_j_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::J, cpu_alignment>>;
  using cpu_k_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::K, cpu_alignment>>;

  //===----------------------------------------------------------------------------------------===//
  //     GPU Storage
  //===----------------------------------------------------------------------------------------===//
  using gpu_kji_storage =
      DataFieldOpenMP<T,
                      DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::KJI, gpu_alignment>>;

  using gpu_ji_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::JI, gpu_alignment>>;
  using gpu_ki_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::KI, gpu_alignment>>;
  using gpu_kj_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::KJ, gpu_alignment>>;

  using gpu_i_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::I, gpu_alignment>>;
  using gpu_j_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::J, gpu_alignment>>;
  using gpu_k_storage =
      DataFieldOpenMP<T, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::K, gpu_alignment>>;
};

} // namespace unittest

} // namespace serialbox

#endif

#endif
