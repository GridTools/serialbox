//===-- Unittest/STELLA.h -----------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file includes all the necessary STELLA headers.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UNITTEST_STELLA_H
#define SERIALBOX_UNITTEST_STELLA_H

#include "serialbox/Core/Compiler.h"

#ifdef SERIALBOX_HAS_STELLA

#include "SharedInfrastructure.h"

//===------------------------------------------------------------------------------------------===//
//     Alignment
//===------------------------------------------------------------------------------------------===//
using cpu_alignment = DataFieldAlignment<cDimK, 1>;
using cpu_ijk_boundary = DataFieldIJBoundary<-cNumBoundaryLines, cNumBoundaryLines,
                                             -cNumBoundaryLines, cNumBoundaryLines>;

using gpu_alignment = DataFieldAlignment<cDimI, 2 * cCacheLineSize / sizeof(Real)>;
using gpu_ijk_boundary = DataFieldIJBoundary<-cNumBoundaryLines, cNumBoundaryLines,
                                             -cNumBoundaryLines, cNumBoundaryLines>;

//===------------------------------------------------------------------------------------------===//
//     CPU Storage
//===------------------------------------------------------------------------------------------===//
using cpu_ijk_storage =
    DataFieldOpenMP<Real,
                    DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::JIK, cpu_alignment>>;

using cpu_ji_storage =
    DataFieldOpenMP<Real,
                    DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::JI, cpu_alignment>>;
using cpu_ik_storage =
    DataFieldOpenMP<Real,
                    DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::IK, cpu_alignment>>;
using cpu_jk_storage =
    DataFieldOpenMP<Real,
                    DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::JK, cpu_alignment>>;

using cpu_i_storage =
    DataFieldOpenMP<Real, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::I, cpu_alignment>>;
using cpu_j_storage =
    DataFieldOpenMP<Real, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::J, cpu_alignment>>;
using cpu_k_storage =
    DataFieldOpenMP<Real, DataFieldStorageFormat<cpu_ijk_boundary, StorageOrder::K, cpu_alignment>>;

static_assert(std::is_same<cpu_ijk_storage, IJKRealField>::value,
              "Storage definitions inconsistent");

//===------------------------------------------------------------------------------------------===//
//     GPU Storage
//===------------------------------------------------------------------------------------------===//
using gpu_ijk_storage =
    DataFieldOpenMP<Real,
                    DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::JIK, gpu_alignment>>;

using gpu_ji_storage =
    DataFieldOpenMP<Real,
                    DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::JI, gpu_alignment>>;
using gpu_ik_storage =
    DataFieldOpenMP<Real,
                    DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::IK, gpu_alignment>>;
using gpu_jk_storage =
    DataFieldOpenMP<Real,
                    DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::JK, gpu_alignment>>;

using gpu_i_storage =
    DataFieldOpenMP<Real, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::I, gpu_alignment>>;
using gpu_j_storage =
    DataFieldOpenMP<Real, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::J, gpu_alignment>>;
using gpu_k_storage =
    DataFieldOpenMP<Real, DataFieldStorageFormat<gpu_ijk_boundary, StorageOrder::K, gpu_alignment>>;

#endif

#endif
