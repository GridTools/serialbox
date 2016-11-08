//===-- serialbox/core/UnittestSerializer.cpp ---------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This implements the unittests of the gridtools::serializer.
///
//===------------------------------------------------------------------------------------------===//

#include "utility/GridTools.h"
#include "utility/SerializerTestBase.h"
#include "utility/Storage.h"
#include "serialbox/core/frontend/gridtools/Serializer.h"
#include <gtest/gtest.h>
#include <memory>

using namespace serialbox::gridtools;
using namespace serialbox::unittest;

//===------------------------------------------------------------------------------------------===//
//     Utility tests
//===------------------------------------------------------------------------------------------===//

namespace {

class GridToolsSerializerUtilityTest : public SerializerUnittestBase {};

} // anonymous namespace

TEST_F(GridToolsSerializerUtilityTest, Construction) {
  serializer ser(open_mode::Write, directory->path().string(), "Field", "Binary");
  EXPECT_EQ(ser.mode(), open_mode::Write);
  EXPECT_EQ(ser.directory(), directory->path().string());
  EXPECT_EQ(ser.prefix(), "Field");
  EXPECT_EQ(ser.archive_name(), "Binary");
  EXPECT_EQ(ser.meta_data_file(), (directory->path() / "MetaData-Field.json").string());
}

TEST_F(GridToolsSerializerUtilityTest, SerializationStatus) {
  // Serialization is enabled by default
  ASSERT_FALSE(serializer::serializationStatus() < 0);

  // Disable serialization
  serializer::disableSerialization();
  ASSERT_EQ(serializer::serializationStatus(), -1);

  // Enabled serialization
  serializer::enableSerialization();
  ASSERT_EQ(serializer::serializationStatus(), 1);
}

TEST_F(GridToolsSerializerUtilityTest, AddMetainfo) {
  serializer ser(open_mode::Write, directory->path().string(), "Field", "Binary");

  // Add meta-info
  ASSERT_NO_THROW(ser.add_global_meta_info("key", 1));

  // Query meta-info
  EXPECT_EQ(ser.get_global_meta_info_as<int>("key"), 1);
  EXPECT_EQ(ser.global_meta_info().at("key").as<int>(), 1);

  // Check aliasing
  auto ser2 = ser;
  ser.add_global_meta_info("k", 5.0);
  ASSERT_TRUE(ser2.global_meta_info().has_key("k"));
}

TEST_F(GridToolsSerializerUtilityTest, RegisterSavepoints) {
  serializer ser(open_mode::Write, directory->path().string(), "Field", "Binary");

  // Register savepoint via copy constructor
  savepoint sp1("sp1");
  ASSERT_TRUE(ser.register_savepoint(sp1));
  ASSERT_FALSE(ser.register_savepoint(sp1));
  EXPECT_TRUE(ser.has_savepoint(sp1));

  // Register savepoint via perfect forwarding
  ASSERT_TRUE(ser.register_savepoint("sp2"));
  EXPECT_TRUE(ser.has_savepoint(savepoint("sp2")));

  // Savepoint vector
  const auto& savepoints = ser.savepoints();
  ASSERT_EQ(savepoints.size(), 2);
  EXPECT_EQ(savepoints[0], sp1);
  EXPECT_EQ(savepoints[1], savepoint("sp2"));

  // Add a new savepoint (forces reallocation of savepoint vector)
  ASSERT_TRUE(ser.register_savepoint("sp3"));

  const auto& new_savepoints = ser.savepoints();
  EXPECT_EQ(new_savepoints.size(), 3);
}

TEST_F(GridToolsSerializerUtilityTest, RegisterFields) {
  serializer ser(open_mode::Write, directory->path().string(), "Field", "Binary");

  // Register field_meta_info via copy constructor
  field_meta_info info1(type_id::Float32, std::vector<int>{10, 15});
  ser.register_field("field1", info1);
  ASSERT_TRUE(ser.has_field("field1"));
  EXPECT_EQ(ser.get_field_meta_info("field1"), info1);

  // Register field_meta_info via perfect forwarding
  ser.register_field("field2", type_id::Float64, std::vector<int>{10, 15, 80});
  ASSERT_TRUE(ser.has_field("field2"));
  EXPECT_EQ(ser.get_field_meta_info("field2"),
            field_meta_info(type_id::Float64, std::vector<int>{10, 15, 80}));

  // Add meta-information to field
  ser.add_meta_info_to_field("field1", "k", 2);
  auto info2 = ser.get_field_meta_info("field1");
  ASSERT_TRUE(info2.meta_info().has_key("k"));
  EXPECT_EQ(info2.meta_info().as<int>("k"), 2);

  // Fieldnames
  auto fieldnames = ser.fieldnames();
  ASSERT_EQ(fieldnames.size(), 2);
  EXPECT_TRUE(std::find(fieldnames.begin(), fieldnames.end(), "field1") != fieldnames.end());
  EXPECT_TRUE(std::find(fieldnames.begin(), fieldnames.end(), "field2") != fieldnames.end());

  // Register existing field -> Exception
  EXPECT_THROW(ser.register_field("field1", info1), exception);
}

#ifdef SERIALBOX_HAS_GRIDTOOLS

//===------------------------------------------------------------------------------------------===//
//     Read/Write tests
//===------------------------------------------------------------------------------------------===//

namespace {

template <class T>
class GridToolsReadWriteTest : public SerializerUnittestBase {
public:
  using Base = SerializerUnittestBase;

  using storage_types = serialbox::unittest::gridtools_storage_types<T>;

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
  std::unique_ptr<typename storage_types::cpu_2d_real_meta_data_type> cpu_2d_real_meta_data_ptr;
  std::unique_ptr<typename storage_types::gpu_2d_real_meta_data_type> gpu_2d_real_meta_data_ptr;
  std::unique_ptr<typename storage_types::cpu_2d_meta_data_type> cpu_2d_meta_data_ptr;
  std::unique_ptr<typename storage_types::gpu_2d_meta_data_type> gpu_2d_meta_data_ptr;
  std::unique_ptr<typename storage_types::cpu_3d_meta_data_type> cpu_3d_meta_data_ptr;
  std::unique_ptr<typename storage_types::gpu_3d_meta_data_type> gpu_3d_meta_data_ptr;
  std::unique_ptr<typename storage_types::cpu_4d_meta_data_type> cpu_4d_meta_data_ptr;
  std::unique_ptr<typename storage_types::gpu_4d_meta_data_type> gpu_4d_meta_data_ptr;

  // -----------------------------------------------------------------------------------------------
  // Storages
  // -----------------------------------------------------------------------------------------------

  // Input
  std::unique_ptr<typename storage_types::cpu_2d_real_storage_type> cpu_2d_real_storage_input_ptr;
  std::unique_ptr<typename storage_types::gpu_2d_real_storage_type> gpu_2d_real_storage_input_ptr;
  std::unique_ptr<typename storage_types::cpu_2d_storage_type> cpu_2d_storage_input_ptr;
  std::unique_ptr<typename storage_types::gpu_2d_storage_type> gpu_2d_storage_input_ptr;
  std::unique_ptr<typename storage_types::cpu_3d_storage_type> cpu_3d_storage_input_ptr;
  std::unique_ptr<typename storage_types::gpu_3d_storage_type> gpu_3d_storage_input_ptr;
  std::unique_ptr<typename storage_types::cpu_4d_storage_type> cpu_4d_storage_input_ptr;
  std::unique_ptr<typename storage_types::gpu_4d_storage_type> gpu_4d_storage_input_ptr;

  // Output
  std::unique_ptr<typename storage_types::cpu_2d_real_storage_type> cpu_2d_real_storage_output_ptr;
  std::unique_ptr<typename storage_types::gpu_2d_real_storage_type> gpu_2d_real_storage_output_ptr;
  std::unique_ptr<typename storage_types::cpu_2d_storage_type> cpu_2d_storage_output_ptr;
  std::unique_ptr<typename storage_types::gpu_2d_storage_type> gpu_2d_storage_output_ptr;
  std::unique_ptr<typename storage_types::cpu_3d_storage_type> cpu_3d_storage_output_ptr;
  std::unique_ptr<typename storage_types::gpu_3d_storage_type> gpu_3d_storage_output_ptr;
  std::unique_ptr<typename storage_types::cpu_4d_storage_type> cpu_4d_storage_output_ptr;
  std::unique_ptr<typename storage_types::gpu_4d_storage_type> gpu_4d_storage_output_ptr;

  virtual void SetUp() override {
    Base::SetUp();

    dim1 = 2 + storage_types::halo1_left + storage_types::halo1_right;
    dim2 = 3 + storage_types::halo2_left + storage_types::halo2_right;
    dim3 = 4 + storage_types::halo3_left + storage_types::halo3_right;
    dim4 = 5 + storage_types::halo4_left + storage_types::halo4_right;

    // ---------------------------------------------------------------------------------------------
    // Meta Data
    // ---------------------------------------------------------------------------------------------
    cpu_2d_real_meta_data_ptr =
        std::make_unique<typename storage_types::cpu_2d_real_meta_data_type>(dim1, dim2);
    gpu_2d_real_meta_data_ptr =
        std::make_unique<typename storage_types::gpu_2d_real_meta_data_type>(dim1, dim2);
    cpu_2d_meta_data_ptr =
        std::make_unique<typename storage_types::cpu_2d_meta_data_type>(dim1, dim2, 0);
    gpu_2d_meta_data_ptr =
        std::make_unique<typename storage_types::gpu_2d_meta_data_type>(dim1, dim2, 0);
    cpu_3d_meta_data_ptr =
        std::make_unique<typename storage_types::cpu_3d_meta_data_type>(dim1, dim2, dim3);
    gpu_3d_meta_data_ptr =
        std::make_unique<typename storage_types::gpu_3d_meta_data_type>(dim1, dim2, dim3);
    cpu_4d_meta_data_ptr =
        std::make_unique<typename storage_types::cpu_4d_meta_data_type>(dim1, dim2, dim3, dim4);
    gpu_4d_meta_data_ptr =
        std::make_unique<typename storage_types::gpu_4d_meta_data_type>(dim1, dim2, dim3, dim4);

    // ---------------------------------------------------------------------------------------------
    // Storages
    // ---------------------------------------------------------------------------------------------

    // Input
    cpu_2d_real_storage_input_ptr =
        std::make_unique<typename storage_types::cpu_2d_real_storage_type>(
            *cpu_2d_real_meta_data_ptr, "cpu_2d_real_storage_input", -1.0);
    gpu_2d_real_storage_input_ptr =
        std::make_unique<typename storage_types::gpu_2d_real_storage_type>(
            *gpu_2d_real_meta_data_ptr, "gpu_2d_real_storage_input", -1.0);

    cpu_2d_storage_input_ptr = std::make_unique<typename storage_types::cpu_2d_storage_type>(
        *cpu_2d_meta_data_ptr, "cpu_2d_storage_input", -1.0);
    gpu_2d_storage_input_ptr = std::make_unique<typename storage_types::gpu_2d_storage_type>(
        *gpu_2d_meta_data_ptr, "gpu_2d_storage_input", -1.0);

    cpu_3d_storage_input_ptr = std::make_unique<typename storage_types::cpu_3d_storage_type>(
        *cpu_3d_meta_data_ptr, "cpu_3d_storage_input", -1.0);
    gpu_3d_storage_input_ptr = std::make_unique<typename storage_types::gpu_3d_storage_type>(
        *gpu_3d_meta_data_ptr, "gpu_3d_storage_input", -1.0);

    cpu_4d_storage_input_ptr = std::make_unique<typename storage_types::cpu_4d_storage_type>(
        *cpu_4d_meta_data_ptr, "cpu_4d_storage_input", -1.0);
    gpu_4d_storage_input_ptr = std::make_unique<typename storage_types::gpu_4d_storage_type>(
        *gpu_4d_meta_data_ptr, "gpu_4d_storage_input", -1.0);

    // Output
    cpu_2d_real_storage_output_ptr =
        std::make_unique<typename storage_types::cpu_2d_real_storage_type>(
            *cpu_2d_real_meta_data_ptr, "cpu_2d_real_storage_output", -1.0);
    gpu_2d_real_storage_output_ptr =
        std::make_unique<typename storage_types::gpu_2d_real_storage_type>(
            *gpu_2d_real_meta_data_ptr, "gpu_2d_real_storage_output", -1.0);

    cpu_2d_storage_output_ptr = std::make_unique<typename storage_types::cpu_2d_storage_type>(
        *cpu_2d_meta_data_ptr, "cpu_2d_storage_output", -1.0);
    gpu_2d_storage_output_ptr = std::make_unique<typename storage_types::gpu_2d_storage_type>(
        *gpu_2d_meta_data_ptr, "gpu_2d_storage_output", -1.0);

    cpu_3d_storage_output_ptr = std::make_unique<typename storage_types::cpu_3d_storage_type>(
        *cpu_3d_meta_data_ptr, "cpu_3d_storage_output", -1.0);
    gpu_3d_storage_output_ptr = std::make_unique<typename storage_types::gpu_3d_storage_type>(
        *gpu_3d_meta_data_ptr, "gpu_3d_storage_output", -1.0);

    cpu_4d_storage_output_ptr = std::make_unique<typename storage_types::cpu_4d_storage_type>(
        *cpu_4d_meta_data_ptr, "cpu_4d_storage_output", -1.0);
    gpu_4d_storage_output_ptr = std::make_unique<typename storage_types::gpu_4d_storage_type>(
        *gpu_4d_meta_data_ptr, "gpu_4d_storage_output", -1.0);

    // 2D
    T val_2d = 0.0;
    for(int j = 0; j < dim2; ++j)
      for(int i = 0; i < dim1; ++i, val_2d += 1) {
        (*cpu_2d_real_storage_input_ptr)(i, j) = val_2d;
        (*gpu_2d_real_storage_input_ptr)(i, j) = val_2d;
        (*cpu_2d_storage_input_ptr)(i, j, 0) = val_2d;
        (*gpu_2d_storage_input_ptr)(i, j, 0) = val_2d;
      }

    // 3D
    T val_3d = 0.0;
    for(int k = 0; k < dim3; ++k)
      for(int j = 0; j < dim2; ++j)
        for(int i = 0; i < dim1; ++i, val_3d += 1) {
          (*cpu_3d_storage_input_ptr)(i, j, k) = val_3d;
          (*gpu_3d_storage_input_ptr)(i, j, k) = val_3d;
        }

    // 4D
    T val_4d = 0.0;
    for(int l = 0; l < dim4; ++l)
      for(int k = 0; k < dim3; ++k)
        for(int j = 0; j < dim2; ++j)
          for(int i = 0; i < dim1; ++i, val_4d += 1) {
            (*cpu_4d_storage_input_ptr)(i, j, k, l) = val_4d;
            (*gpu_4d_storage_input_ptr)(i, j, k, l) = val_4d;
          }
  }
};

using TestTypes = testing::Types<double, float, int>;

} // anonymous namespace

TYPED_TEST_CASE(GridToolsReadWriteTest, TestTypes);

TYPED_TEST(GridToolsReadWriteTest, WriteAndRead) {

  // ---------------------------------------------------------------------------------------------
  // Writing
  // ---------------------------------------------------------------------------------------------
  {
    serializer ser(open_mode::Write, this->directory->path().string(), "Field", "Binary");

    // Register fields
    type_id type = serialbox::ToTypeID<TypeParam>::value;
    ser.register_field("2d_real", type, std::vector<int>{this->dim1, this->dim2});

    // We set the empty dimension to 1 (gridtools sets it to 0 but this case should be handeled in 
    // the SerializerImpl)
    ser.register_field("2d", type, std::vector<int>{this->dim1, this->dim2, 1});

    ser.register_field("3d", type, std::vector<int>{this->dim1, this->dim2, this->dim3});

    ser.register_field("4d", type,
                       std::vector<int>{this->dim1, this->dim2, this->dim3, this->dim4});

    // Writing (implicitly register the savepoints)
    ser.write("2d_real", savepoint("cpu"), *this->cpu_2d_real_storage_input_ptr);
    ser.write("2d_real", savepoint("gpu"), *this->gpu_2d_real_storage_input_ptr);

    ser.write("2d", savepoint("cpu"), *this->cpu_2d_storage_input_ptr);
    ser.write("2d", savepoint("gpu"), *this->gpu_2d_storage_input_ptr);

    ser.write("3d", savepoint("cpu"), *this->cpu_3d_storage_input_ptr);
    ser.write("3d", savepoint("gpu"), *this->gpu_3d_storage_input_ptr);

    ser.write("4d", savepoint("cpu"), *this->cpu_4d_storage_input_ptr);
    ser.write("4d", savepoint("gpu"), *this->gpu_4d_storage_input_ptr);
  }

  // ---------------------------------------------------------------------------------------------
  // Reading
  // ---------------------------------------------------------------------------------------------
  {
    serializer ser(open_mode::Read, this->directory->path().string(), "Field", "Binary");

    const auto& savepoints = ser.savepoints();
    ASSERT_EQ(savepoints.size(), 2);
    ASSERT_TRUE(ser.has_savepoint(savepoint("cpu")));
    ASSERT_TRUE(ser.has_savepoint(savepoint("gpu")));

    ser.read("2d_real", savepoint("cpu"), *this->cpu_2d_real_storage_output_ptr);
    ser.read("2d_real", savepoint("gpu"), *this->gpu_2d_real_storage_output_ptr);

    ser.read("2d", savepoint("cpu"), *this->cpu_2d_storage_output_ptr);
    ser.read("2d", savepoint("gpu"), *this->gpu_2d_storage_output_ptr);

    ser.read("3d", savepoint("cpu"), *this->cpu_3d_storage_output_ptr);
    ser.read("3d", savepoint("gpu"), *this->gpu_3d_storage_output_ptr);

    ser.read("4d", savepoint("cpu"), *this->cpu_4d_storage_output_ptr);
    ser.read("4d", savepoint("gpu"), *this->gpu_4d_storage_output_ptr);
  }

  // ---------------------------------------------------------------------------------------------
  // Validate
  // ---------------------------------------------------------------------------------------------

  // 2D

  for(int i = 0; i < this->dim1; ++i)
    for(int j = 0; j < this->dim2; ++j)
      for(int k = 0; k < this->dim3; ++k)
        for(int l = 0; l < this->dim4; ++l) {

          // 2D (real) cpu
          ASSERT_EQ((*this->cpu_2d_real_storage_input_ptr)(i, j),
                    (*this->cpu_2d_real_storage_output_ptr)(i, j))
              << "(i,j) = (" << i << "," << j << ")";

          // 2D (real) gpu
          ASSERT_EQ((*this->gpu_2d_real_storage_input_ptr)(i, j),
                    (*this->gpu_2d_real_storage_output_ptr)(i, j))
              << "(i,j) = (" << i << "," << j << ")";

          // 2D cpu
          ASSERT_EQ((*this->cpu_2d_storage_input_ptr)(i, j, 0),
                    (*this->cpu_2d_storage_output_ptr)(i, j, 0))
              << "(i,j,0) = (" << i << "," << j << "," << 0 << ")";

          // 2D gpu
          ASSERT_EQ((*this->gpu_2d_storage_input_ptr)(i, j, 0),
                    (*this->gpu_2d_storage_output_ptr)(i, j, 0))
              << "(i,j,0) = (" << i << "," << j << "," << 0 << ")";

          // 3D cpu
          ASSERT_EQ((*this->cpu_3d_storage_input_ptr)(i, j, k),
                    (*this->cpu_3d_storage_output_ptr)(i, j, k))
              << "(i,j,k) = (" << i << "," << j << "," << k << ")";

          // 3D cpu
          ASSERT_EQ((*this->gpu_3d_storage_input_ptr)(i, j, k),
                    (*this->gpu_3d_storage_output_ptr)(i, j, k))
              << "(i,j,k) = (" << i << "," << j << "," << k << ")";

          // 4D cpu
          ASSERT_EQ((*this->cpu_4d_storage_input_ptr)(i, j, k, l),
                    (*this->cpu_4d_storage_output_ptr)(i, j, k, l))
              << "(i,j,k,l) = (" << i << "," << j << "," << k << "," << l << ")";

          // 4D gpu
          ASSERT_EQ((*this->gpu_4d_storage_input_ptr)(i, j, k, l),
                    (*this->gpu_4d_storage_output_ptr)(i, j, k, l))
              << "(i,j,k,l) = (" << i << "," << j << "," << k << "," << l << ")";
        }
}

TYPED_TEST(GridToolsReadWriteTest, ToAndFromFile) {
  using storage_types = serialbox::unittest::gridtools_storage_types<TypeParam>;

  typename storage_types::cpu_3d_storage_type storage_in(*this->cpu_3d_meta_data_ptr, "storage",
                                                         -1.0);
  typename storage_types::cpu_3d_storage_type storage_out(*this->cpu_3d_meta_data_ptr, "storage",
                                                          -1.0);
  // Fill data
  for(int i = 0; i < this->dim1; ++i)
    for(int j = 0; j < this->dim2; ++j)
      for(int k = 0; k < this->dim3; ++k)
        storage_in(i, j, k) = i*j*k;

  // Write and read from file
  serializer::to_file((this->directory->path() / "test.dat").string(), storage_in, "Binary");
  serializer::from_file((this->directory->path() / "test.dat").string(), storage_out, "Binary");

  // Verify
  for(int i = 0; i < this->dim1; ++i)
    for(int j = 0; j < this->dim2; ++j)
      for(int k = 0; k < this->dim3; ++k)
        ASSERT_EQ(storage_in(i, j, k), storage_out(i, j, k)) << "(i,j,k) = (" << i << "," << j
                                                             << "," << k << ")";
}

TYPED_TEST(GridToolsReadWriteTest, NonGridToolsWriteAndRead) {
  using Storage = serialbox::unittest::Storage<TypeParam>;
  Storage storage_input(Storage::ColMajor, {5, 2, 5}, Storage::random);
  Storage storage_output(Storage::ColMajor, {5, 2, 5});

  auto sv_input = storage_input.toStorageView();
  auto sv_output = storage_output.toStorageView();
  
  // Write
  {
    serializer ser(open_mode::Write, this->directory->path().string(), "Field", "Binary");
    auto type = serialbox::ToTypeID<TypeParam>::value;
    ser.register_field("storage", type, sv_input.dims());
    ser.write("storage", savepoint("sp"), sv_input.template originPtrAs<TypeParam>(),
              sv_input.strides());
  }

  // Read
  {
    serializer ser(open_mode::Read, this->directory->path().string(), "Field", "Binary");
    ser.read("storage", savepoint("sp"), sv_output.template originPtrAs<TypeParam>(),
             sv_output.strides());
  }

  // Verify
  ASSERT_TRUE(Storage::verify(storage_input, storage_output));
}

#endif // SERIALBOX_HAS_GRIDTOOLS
