//===-- serialbox/core/frontend/stella/UnittestStorageView.cpp ----------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the StorageView interoperating with STELLA.
///
//===------------------------------------------------------------------------------------------===//

#include "utility/STELLA.h"
#include "serialbox/core/STLExtras.h"
#include "serialbox/core/StorageView.h"
#include <gtest/gtest.h>
#include <memory>

#ifdef SERIALBOX_HAS_STELLA

namespace {

template <class T>
class STELLAStorageViewTest : public testing::Test {
public:
  using StorageType = serialbox::unittest::STELLAStorageTypes<T>;

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
  std::unique_ptr<typename StorageType::cpu_jik_storage> cpu_jik_field_ptr;
  std::unique_ptr<typename StorageType::cpu_ji_storage> cpu_ji_field_ptr;
  std::unique_ptr<typename StorageType::cpu_ik_storage> cpu_ik_field_ptr;
  std::unique_ptr<typename StorageType::cpu_jk_storage> cpu_jk_field_ptr;
  std::unique_ptr<typename StorageType::cpu_i_storage> cpu_i_field_ptr;
  std::unique_ptr<typename StorageType::cpu_j_storage> cpu_j_field_ptr;
  std::unique_ptr<typename StorageType::cpu_k_storage> cpu_k_field_ptr;

  std::unique_ptr<typename StorageType::gpu_kji_storage> gpu_kji_field_ptr;
  std::unique_ptr<typename StorageType::gpu_ji_storage> gpu_ji_field_ptr;
  std::unique_ptr<typename StorageType::gpu_ki_storage> gpu_ki_field_ptr;
  std::unique_ptr<typename StorageType::gpu_kj_storage> gpu_kj_field_ptr;
  std::unique_ptr<typename StorageType::gpu_i_storage> gpu_i_field_ptr;
  std::unique_ptr<typename StorageType::gpu_j_storage> gpu_j_field_ptr;
  std::unique_ptr<typename StorageType::gpu_k_storage> gpu_k_field_ptr;

protected:
  virtual void SetUp() override {
    dim1 = 2;
    dim2 = 3;
    dim3 = 4;

    k_boundary = std::make_unique<KBoundary>();
    k_boundary->Init(0, 1);

    // CPU Size
    cpu_jik_size = std::make_unique<IJKSize>();
    cpu_jik_size->Init(dim1, dim2, dim3);

    cpu_ji_size = std::make_unique<IJKSize>();
    cpu_ji_size->Init(dim1, dim2, 1);

    cpu_ik_size = std::make_unique<IJKSize>();
    cpu_ik_size->Init(dim1, 1, dim3);

    cpu_jk_size = std::make_unique<IJKSize>();
    cpu_jk_size->Init(1, dim2, dim3);

    cpu_i_size = std::make_unique<IJKSize>();
    cpu_i_size->Init(dim1, 1, 1);

    cpu_j_size = std::make_unique<IJKSize>();
    cpu_j_size->Init(1, dim2, 1);

    cpu_k_size = std::make_unique<IJKSize>();
    cpu_k_size->Init(1, 1, dim3);

    // GPU Size
    gpu_kji_size = std::make_unique<IJKSize>();
    gpu_kji_size->Init(dim1, dim2, dim3);

    gpu_ji_size = std::make_unique<IJKSize>();
    gpu_ji_size->Init(dim1, dim2, 1);

    gpu_ki_size = std::make_unique<IJKSize>();
    gpu_ki_size->Init(dim1, 1, dim3);

    gpu_kj_size = std::make_unique<IJKSize>();
    gpu_kj_size->Init(1, dim2, dim3);

    gpu_i_size = std::make_unique<IJKSize>();
    gpu_i_size->Init(dim1, 1, 1);

    gpu_j_size = std::make_unique<IJKSize>();
    gpu_j_size->Init(1, dim2, 1);

    gpu_k_size = std::make_unique<IJKSize>();
    gpu_k_size->Init(1, 1, dim3);

    // CPU Storages
    cpu_jik_field_ptr = std::make_unique<typename StorageType::cpu_jik_storage>();
    cpu_jik_field_ptr->Init("cpu_jik_field", *cpu_jik_size, *k_boundary);

    cpu_ji_field_ptr = std::make_unique<typename StorageType::cpu_ji_storage>();
    cpu_ji_field_ptr->Init("cpu_ji_field", *cpu_ji_size, *k_boundary);

    cpu_ik_field_ptr = std::make_unique<typename StorageType::cpu_ik_storage>();
    cpu_ik_field_ptr->Init("cpu_ik_field", *cpu_ik_size, *k_boundary);

    cpu_jk_field_ptr = std::make_unique<typename StorageType::cpu_jk_storage>();
    cpu_jk_field_ptr->Init("cpu_jk_field", *cpu_jk_size, *k_boundary);

    cpu_i_field_ptr = std::make_unique<typename StorageType::cpu_i_storage>();
    cpu_i_field_ptr->Init("cpu_i_field", *cpu_i_size, *k_boundary);

    cpu_j_field_ptr = std::make_unique<typename StorageType::cpu_j_storage>();
    cpu_j_field_ptr->Init("cpu_j_field", *cpu_j_size, *k_boundary);

    cpu_k_field_ptr = std::make_unique<typename StorageType::cpu_k_storage>();
    cpu_k_field_ptr->Init("cpu_k_field", *cpu_k_size, *k_boundary);

    // GPU Storages
    gpu_kji_field_ptr = std::make_unique<typename StorageType::gpu_kji_storage>();
    gpu_kji_field_ptr->Init("gpu_kji_field", *gpu_kji_size, *k_boundary);

    gpu_ji_field_ptr = std::make_unique<typename StorageType::gpu_ji_storage>();
    gpu_ji_field_ptr->Init("gpu_ji_field", *gpu_ji_size, *k_boundary);

    gpu_ki_field_ptr = std::make_unique<typename StorageType::gpu_ki_storage>();
    gpu_ki_field_ptr->Init("gpu_ki_field", *gpu_ki_size, *k_boundary);

    gpu_kj_field_ptr = std::make_unique<typename StorageType::gpu_kj_storage>();
    gpu_kj_field_ptr->Init("gpu_kj_field", *gpu_kj_size, *k_boundary);

    gpu_i_field_ptr = std::make_unique<typename StorageType::gpu_i_storage>();
    gpu_i_field_ptr->Init("gpu_i_field", *gpu_i_size, *k_boundary);

    gpu_j_field_ptr = std::make_unique<typename StorageType::gpu_j_storage>();
    gpu_j_field_ptr->Init("gpu_j_field", *gpu_j_size, *k_boundary);

    gpu_k_field_ptr = std::make_unique<typename StorageType::gpu_k_storage>();
    gpu_k_field_ptr->Init("gpu_k_field", *gpu_k_size, *k_boundary);

    T val = 0.0;
    const IJKBoundary& boundary = cpu_jik_field_ptr->boundary();
    for(int i = boundary.iMinusOffset(); i < (dim1 + boundary.iPlusOffset()); ++i)
      for(int j = boundary.jMinusOffset(); j < (dim2 + boundary.jPlusOffset()); ++j)
        for(int k = boundary.kMinusOffset(); k < (dim3 + +boundary.kPlusOffset()); ++k) {
          (*cpu_jik_field_ptr)(i, j, k) = val;
          (*cpu_ji_field_ptr)(i, j, 0) = val;
          (*cpu_ik_field_ptr)(i, 0, k) = val;
          (*cpu_jk_field_ptr)(0, j, k) = val;
          (*cpu_i_field_ptr)(i, 0, 0) = val;
          (*cpu_j_field_ptr)(0, j, 0) = val;
          (*cpu_k_field_ptr)(0, 0, k) = val;

          (*gpu_kji_field_ptr)(i, j, k) = val;
          (*gpu_ji_field_ptr)(i, j, 0) = val;
          (*gpu_ki_field_ptr)(i, 0, k) = val;
          (*gpu_kj_field_ptr)(0, j, k) = val;
          (*gpu_i_field_ptr)(i, 0, 0) = val;
          (*gpu_j_field_ptr)(0, j, 0) = val;
          (*gpu_k_field_ptr)(0, 0, k) = val;

          val += 1.0;
        }
  }

  virtual void TearDown() override {}
};

template <typename TFieldType>
serialbox::StorageView makeStorageView(const TFieldType& dataField) {
  // Strides
  DataFieldStorageStrides<typename TFieldType::StorageFormat::StorageOrder> storageStrides;
  storageStrides.Init(dataField.storage().paddedSize());

  std::vector<int> strides(3);
  strides[0] = storageStrides.ComputeStride(1, 0, 0);
  strides[1] = storageStrides.ComputeStride(0, 1, 0);
  strides[2] = storageStrides.ComputeStride(0, 0, 1);

  // Dimension
  const IJKSize& size = dataField.storage().allocatedSize();

  std::vector<int> dims(3);
  dims[0] = size.iSize();
  dims[1] = size.jSize();
  dims[2] = size.kSize();

  // Origin ptr
  const IJKIndex& originOffset = dataField.storage().originOffset();
  void* originPtr = const_cast<void*>(static_cast<const void*>(
      &dataField(-originOffset.iIndex(), -originOffset.jIndex(), -originOffset.kIndex())));

  return serialbox::StorageView(originPtr,
                                serialbox::ToTypeID<typename TFieldType::ValueType>::value,
                                std::move(dims), std::move(strides));
}

using TestTypes = testing::Types<double, float, int>;

} // anonymous namespace

TYPED_TEST_CASE(STELLAStorageViewTest, TestTypes);

TYPED_TEST(STELLAStorageViewTest, Iterator) {
  int dim1 = this->dim1, dim2 = this->dim2, dim3 = this->dim3;

// -------------------------------------------------------------------------------------------------
// 1D I
// -------------------------------------------------------------------------------------------------

#define CHECK_1D_I(field_ptr)                                                                      \
  {                                                                                                \
    auto field = *this->field_ptr;                                                                 \
    serialbox::StorageView storage_view = makeStorageView(field);                                  \
    auto field_it = storage_view.begin();                                                          \
    const IJKBoundary& boundary = field.boundary();                                                \
    for(int i = boundary.iMinusOffset(); i < (dim1 + boundary.iPlusOffset()); ++i, ++field_it)     \
      ASSERT_EQ(field_it.as<TypeParam>(), field(i, 0, 0)) << field.name();                         \
  }

  CHECK_1D_I(cpu_i_field_ptr);
  CHECK_1D_I(gpu_i_field_ptr);

#undef CHECK_1D_I

// -------------------------------------------------------------------------------------------------
// 1D J
// -------------------------------------------------------------------------------------------------

#define CHECK_1D_J(field_ptr)                                                                      \
  {                                                                                                \
    auto field = *this->field_ptr;                                                                 \
    serialbox::StorageView storage_view = makeStorageView(field);                                  \
    auto field_it = storage_view.begin();                                                          \
    const IJKBoundary& boundary = field.boundary();                                                \
    for(int j = boundary.jMinusOffset(); j < (dim2 + boundary.jPlusOffset()); ++j, ++field_it)     \
      ASSERT_EQ(field_it.as<TypeParam>(), field(0, j, 0)) << field.name();                         \
  }

  CHECK_1D_J(cpu_j_field_ptr);
  CHECK_1D_J(gpu_j_field_ptr);

#undef CHECK_1D_J

// -------------------------------------------------------------------------------------------------
// 1D I
// -------------------------------------------------------------------------------------------------

#define CHECK_1D_K(field_ptr)                                                                      \
  {                                                                                                \
    auto field = *this->field_ptr;                                                                 \
    serialbox::StorageView storage_view = makeStorageView(field);                                  \
    auto field_it = storage_view.begin();                                                          \
    const IJKBoundary& boundary = field.boundary();                                                \
    for(int k = boundary.kMinusOffset(); k < (dim3 + boundary.kPlusOffset()); ++k, ++field_it)     \
      ASSERT_EQ(field_it.as<TypeParam>(), field(0, 0, k)) << field.name();                         \
  }

  CHECK_1D_K(cpu_k_field_ptr);
  CHECK_1D_K(gpu_k_field_ptr);

#undef CHECK_1D_K

// -------------------------------------------------------------------------------------------------
// 2D KJ
// -------------------------------------------------------------------------------------------------

#define CHECK_2D_KJ(field_ptr)                                                                     \
  {                                                                                                \
    auto field = *this->field_ptr;                                                                 \
    serialbox::StorageView storage_view = makeStorageView(field);                                  \
    auto field_it = storage_view.begin();                                                          \
    const IJKBoundary& boundary = field.boundary();                                                \
    for(int k = boundary.kMinusOffset(); k < (dim3 + +boundary.kPlusOffset()); ++k)                \
      for(int j = boundary.jMinusOffset(); j < (dim2 + boundary.jPlusOffset()); ++j, ++field_it)   \
        ASSERT_EQ(field_it.as<TypeParam>(), field(0, j, k)) << field.name();                       \
  }

  CHECK_2D_KJ(cpu_jk_field_ptr);
  CHECK_2D_KJ(gpu_kj_field_ptr);

#undef CHECK_2D_KJ

// -------------------------------------------------------------------------------------------------
// 2D IK
// -------------------------------------------------------------------------------------------------

#define CHECK_2D_IK(field_ptr)                                                                     \
  {                                                                                                \
    auto field = *this->field_ptr;                                                                 \
    serialbox::StorageView storage_view = makeStorageView(field);                                  \
    auto field_it = storage_view.begin();                                                          \
    const IJKBoundary& boundary = field.boundary();                                                \
    for(int k = boundary.kMinusOffset(); k < (dim3 + +boundary.kPlusOffset()); ++k)                \
      for(int i = boundary.iMinusOffset(); i < (dim1 + boundary.iPlusOffset()); ++i, ++field_it)   \
        ASSERT_EQ(field_it.as<TypeParam>(), field(i, 0, k)) << field.name();                       \
  }

  CHECK_2D_IK(cpu_ik_field_ptr);
  CHECK_2D_IK(gpu_ki_field_ptr);

#undef CHECK_2D_IK

// -------------------------------------------------------------------------------------------------
// 2D IJ
// -------------------------------------------------------------------------------------------------

#define CHECK_2D_IJ(field_ptr)                                                                     \
  {                                                                                                \
    auto field = *this->field_ptr;                                                                 \
    serialbox::StorageView storage_view = makeStorageView(field);                                  \
    auto field_it = storage_view.begin();                                                          \
    const IJKBoundary& boundary = field.boundary();                                                \
    for(int j = boundary.jMinusOffset(); j < (dim2 + boundary.jPlusOffset()); ++j)                 \
      for(int i = boundary.iMinusOffset(); i < (dim1 + boundary.iPlusOffset()); ++i, ++field_it)   \
        ASSERT_EQ(field_it.as<TypeParam>(), field(i, j, 0)) << field.name();                       \
  }

  CHECK_2D_IJ(cpu_ji_field_ptr);
  CHECK_2D_IJ(gpu_ji_field_ptr);

#undef CHECK_2D_IJ

// -------------------------------------------------------------------------------------------------
// 3D
// -------------------------------------------------------------------------------------------------

#define CHECK_3D(field_ptr)                                                                        \
  {                                                                                                \
    auto field = *this->field_ptr;                                                                 \
    serialbox::StorageView storage_view = makeStorageView(field);                                  \
    auto field_it = storage_view.begin();                                                          \
    const IJKBoundary& boundary = field.boundary();                                                \
    for(int k = boundary.kMinusOffset(); k < (dim3 + +boundary.kPlusOffset()); ++k)                \
      for(int j = boundary.jMinusOffset(); j < (dim2 + boundary.jPlusOffset()); ++j)               \
        for(int i = boundary.iMinusOffset(); i < (dim1 + boundary.iPlusOffset()); ++i, ++field_it) \
          ASSERT_EQ(field_it.as<TypeParam>(), field(i, j, k)) << field.name();                     \
  }

  CHECK_3D(cpu_jik_field_ptr);
  CHECK_3D(gpu_kji_field_ptr);

#undef CHECK_3D
}

#endif
