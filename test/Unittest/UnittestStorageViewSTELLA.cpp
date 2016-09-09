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

#include "STELLA.h"
#include "serialbox/Core/STLExtras.h"
#include "serialbox/Core/StorageView.h"
#include <gtest/gtest.h>
#include <memory>

#ifdef SERIALBOX_HAS_STELLA

#include "serialbox/Core/Frontend/STELLA/StorageViewHelper.h"

namespace {

using serialbox::make_unique;

class StorageViewSTELLATest : public testing::Test {
public:
  // -----------------------------------------------------------------------------------------------
  // Dimensions
  // -----------------------------------------------------------------------------------------------
  int dim1;
  int dim2;
  int dim3;

  // -----------------------------------------------------------------------------------------------
  // Boundaries
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<KBoundary> k_boundary;

  // -----------------------------------------------------------------------------------------------
  // Calculation Domain
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<IJKSize> cpu_jik_size;
  std::unique_ptr<IJKSize> cpu_ji_size;
  std::unique_ptr<IJKSize> cpu_ik_size;
  std::unique_ptr<IJKSize> cpu_jk_size;
  std::unique_ptr<IJKSize> cpu_i_size;
  std::unique_ptr<IJKSize> cpu_j_size;
  std::unique_ptr<IJKSize> cpu_k_size;

  std::unique_ptr<IJKSize> gpu_kji_size;
  std::unique_ptr<IJKSize> gpu_ji_size;
  std::unique_ptr<IJKSize> gpu_ki_size;
  std::unique_ptr<IJKSize> gpu_kj_size;
  std::unique_ptr<IJKSize> gpu_i_size;
  std::unique_ptr<IJKSize> gpu_j_size;
  std::unique_ptr<IJKSize> gpu_k_size;

  // -----------------------------------------------------------------------------------------------
  // Fields
  // -----------------------------------------------------------------------------------------------
  std::unique_ptr<cpu_jik_storage> cpu_jik_field;
  std::unique_ptr<cpu_ji_storage> cpu_ji_field;
  std::unique_ptr<cpu_ik_storage> cpu_ik_field;
  std::unique_ptr<cpu_jk_storage> cpu_jk_field;
  std::unique_ptr<cpu_i_storage> cpu_i_field;
  std::unique_ptr<cpu_j_storage> cpu_j_field;
  std::unique_ptr<cpu_k_storage> cpu_k_field;

  std::unique_ptr<gpu_kji_storage> gpu_kji_field;
  std::unique_ptr<gpu_ji_storage> gpu_ji_field;
  std::unique_ptr<gpu_ki_storage> gpu_ki_field;
  std::unique_ptr<gpu_kj_storage> gpu_kj_field;
  std::unique_ptr<gpu_i_storage> gpu_i_field;
  std::unique_ptr<gpu_j_storage> gpu_j_field;
  std::unique_ptr<gpu_k_storage> gpu_k_field;

protected:
  virtual void SetUp() override {
    dim1 = 2;
    dim2 = 3;
    dim3 = 4;

    k_boundary = make_unique<KBoundary>();
    k_boundary->Init(0, 1);

    // CPU Size
    cpu_jik_size = make_unique<IJKSize>();
    cpu_jik_size->Init(dim1, dim2, dim3);

    cpu_ji_size = make_unique<IJKSize>();
    cpu_ji_size->Init(dim1, dim2, 1);

    cpu_ik_size = make_unique<IJKSize>();
    cpu_ik_size->Init(dim1, 1, dim3);

    cpu_jk_size = make_unique<IJKSize>();
    cpu_jk_size->Init(1, dim2, dim3);

    cpu_i_size = make_unique<IJKSize>();
    cpu_i_size->Init(dim1, 1, 1);

    cpu_j_size = make_unique<IJKSize>();
    cpu_j_size->Init(1, dim2, 1);

    cpu_k_size = make_unique<IJKSize>();
    cpu_k_size->Init(1, 1, dim3);

    // GPU Size
    gpu_kji_size = make_unique<IJKSize>();
    gpu_kji_size->Init(dim1, dim2, dim3);

    gpu_ji_size = make_unique<IJKSize>();
    gpu_ji_size->Init(dim1, dim2, 1);

    gpu_ki_size = make_unique<IJKSize>();
    gpu_ki_size->Init(dim1, 1, dim3);

    gpu_kj_size = make_unique<IJKSize>();
    gpu_kj_size->Init(1, dim2, dim3);

    gpu_i_size = make_unique<IJKSize>();
    gpu_i_size->Init(dim1, 1, 1);

    gpu_j_size = make_unique<IJKSize>();
    gpu_j_size->Init(1, dim2, 1);

    gpu_k_size = make_unique<IJKSize>();
    gpu_k_size->Init(1, 1, dim3);

    // CPU Storages
    cpu_jik_field = make_unique<cpu_jik_storage>();
    cpu_jik_field->Init("cpu_jik_field", *cpu_jik_size, *k_boundary);

    cpu_ji_field = make_unique<cpu_ji_storage>();
    cpu_ji_field->Init("cpu_ji_field", *cpu_ji_size, *k_boundary);

    cpu_ik_field = make_unique<cpu_ik_storage>();
    cpu_ik_field->Init("cpu_ik_field", *cpu_ik_size, *k_boundary);

    cpu_jk_field = make_unique<cpu_jk_storage>();
    cpu_jk_field->Init("cpu_jk_field", *cpu_jk_size, *k_boundary);

    cpu_i_field = make_unique<cpu_i_storage>();
    cpu_i_field->Init("cpu_i_field", *cpu_i_size, *k_boundary);

    cpu_j_field = make_unique<cpu_j_storage>();
    cpu_j_field->Init("cpu_j_field", *cpu_j_size, *k_boundary);

    cpu_k_field = make_unique<cpu_k_storage>();
    cpu_k_field->Init("cpu_k_field", *cpu_k_size, *k_boundary);

    // GPU Storages
    gpu_kji_field = make_unique<gpu_kji_storage>();
    gpu_kji_field->Init("gpu_kji_field", *gpu_kji_size, *k_boundary);

    gpu_ji_field = make_unique<gpu_ji_storage>();
    gpu_ji_field->Init("gpu_ji_field", *gpu_ji_size, *k_boundary);

    gpu_ki_field = make_unique<gpu_ki_storage>();
    gpu_ki_field->Init("gpu_ki_field", *gpu_ki_size, *k_boundary);

    gpu_kj_field = make_unique<gpu_kj_storage>();
    gpu_kj_field->Init("gpu_kj_field", *gpu_kj_size, *k_boundary);

    gpu_i_field = make_unique<gpu_i_storage>();
    gpu_i_field->Init("gpu_i_field", *gpu_i_size, *k_boundary);

    gpu_j_field = make_unique<gpu_j_storage>();
    gpu_j_field->Init("gpu_j_field", *gpu_j_size, *k_boundary);

    gpu_k_field = make_unique<gpu_k_storage>();
    gpu_k_field->Init("gpu_k_field", *gpu_k_size, *k_boundary);
    
    Real val = 0.0;
    const IJKBoundary& boundary = cpu_jik_field->boundary();
    for(int i = boundary.iMinusOffset(); i < (dim1 + boundary.iPlusOffset()); ++i)
      for(int j = boundary.jMinusOffset(); j < (dim2 + boundary.jPlusOffset()); ++j)
        for(int k = boundary.kMinusOffset(); k < (dim3 + +boundary.kPlusOffset()); ++k) {
          (*cpu_jik_field)(i, j, k) = val;
          (*cpu_ji_field)(i, j, 0) = val;
          (*cpu_ik_field)(i, 0, k) = val;
          (*cpu_jk_field)(0, j, k) = val;
          (*cpu_i_field)(i, 0, 0) = val;
          (*cpu_j_field)(0, j, 0) = val;
          (*cpu_k_field)(0, 0, k) = val;
    
          (*gpu_kji_field)(i, j, k) = val;
          (*gpu_ji_field)(i, j, 0) = val;
          (*gpu_ki_field)(i, 0, k) = val;
          (*gpu_kj_field)(0, j, k) = val;
          (*gpu_i_field)(i, 0, 0) = val;
          (*gpu_j_field)(0, j, 0) = val;
          (*gpu_k_field)(0, 0, k) = val;
        }

  }

  virtual void TearDown() override {}
};

} // anonymous namespace

TEST_F(StorageViewSTELLATest, Construction) {
//  using namespace serialbox::stella;
  
////    auto& field = *gpu_kji_field;
//  auto& field = *cpu_jik_field;

//  const IJKSize& size = field.storage().size();
//  std::cout << "Size: " << size.iSize() << " " << size.jSize() << " " << size.kSize() << std::endl;

//  const IJKSize& allocatedSize = field.storage().allocatedSize();
//  std::cout << "Allocated Size: " << allocatedSize.iSize() << " " << allocatedSize.jSize() << " "
//            << allocatedSize.kSize() << std::endl;

//  const IJKSize& paddedSize = field.storage().paddedSize();
//  std::cout << "Padded Size: " << paddedSize.iSize() << " " << paddedSize.jSize() << " "
//            << paddedSize.kSize() << std::endl;

//  const IJKIndex& originOffset = field.storage().originOffset();
//  std::cout << "origin offset: " << originOffset.iIndex() << " " << originOffset.jIndex() << " "
//            << originOffset.kIndex() << std::endl;

//  int rank = field.storage().rank();
//  std::cout << "Rank: " << rank << std::endl;

//  const IJKBoundary& boundary = field.boundary();
//  std::cout << "Boundary: " << std::endl;
//  std::cout << "  i: " << -boundary.iMinusOffset() << " " << boundary.iPlusOffset() << std::endl;
//  std::cout << "  j: " << -boundary.jMinusOffset() << " " << boundary.jPlusOffset() << std::endl;
//  std::cout << "  k: " << -boundary.kMinusOffset() << " " << boundary.kPlusOffset() << std::endl;

//  std::cout << "Data: " << field.storage().pStorageBase() << "   ("
//            << &field(-originOffset.iIndex(), -originOffset.jIndex(), -originOffset.kIndex()) << ")"
//            << std::endl;
  
//  std::cout << " ------ " << std::endl;
  
//  std::vector<int> dims(internal::getDims(field));
//  std::cout << dims[0] << std::endl;
//  std::cout << dims[1] << std::endl;
//  std::cout << dims[2] << std::endl;
  
//  std::vector<int> strides(internal::getStrides(field));
//  std::cout << strides[0] << std::endl;
//  std::cout << strides[1] << std::endl;
//  std::cout << strides[2] << std::endl;
}

TEST_F(StorageViewSTELLATest, Iterator) {}

#endif
