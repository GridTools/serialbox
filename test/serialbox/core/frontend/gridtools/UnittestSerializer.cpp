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

#ifdef SERIALBOX_HAS_GRIDTOOLS
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

namespace {

template <class T>
class GridToolsReadWriteTest : public SerializerUnittestBase {
public:
  using Base = SerializerUnittestBase;

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
  // Input
  typename storage_types::cpu_2d_real_storage_type cpu_2d_real_storage_input;
  typename storage_types::gpu_2d_real_storage_type gpu_2d_real_storage_input;
  typename storage_types::cpu_2d_storage_type cpu_2d_storage_input;
  typename storage_types::gpu_2d_storage_type gpu_2d_storage_input;
  typename storage_types::cpu_3d_storage_type cpu_3d_storage_input;
  typename storage_types::gpu_3d_storage_type gpu_3d_storage_input;
  typename storage_types::cpu_4d_storage_type cpu_4d_storage_input;
  typename storage_types::gpu_4d_storage_type gpu_4d_storage_input;

  // Output
  typename storage_types::cpu_2d_real_storage_type cpu_2d_real_storage_output;
  typename storage_types::gpu_2d_real_storage_type gpu_2d_real_storage_output;
  typename storage_types::cpu_2d_storage_type cpu_2d_storage_output;
  typename storage_types::gpu_2d_storage_type gpu_2d_storage_output;
  typename storage_types::cpu_3d_storage_type cpu_3d_storage_output;
  typename storage_types::gpu_3d_storage_type gpu_3d_storage_output;
  typename storage_types::cpu_4d_storage_type cpu_4d_storage_output;
  typename storage_types::gpu_4d_storage_type gpu_4d_storage_output;

  GridToolsReadWriteTest()
      : dim1(2 + storage_types::halo1_left + storage_types::halo1_right),
        dim2(3 + storage_types::halo2_left + storage_types::halo2_right),
        dim3(4 + storage_types::halo3_left + storage_types::halo3_right),
        dim4(5 + storage_types::halo4_left + storage_types::halo4_right),
        cpu_2d_real_meta_data(dim1, dim2),                                             //
        gpu_2d_real_meta_data(dim1, dim2),                                             //
        cpu_2d_meta_data(dim1, dim2, 1),                                               //
        gpu_2d_meta_data(dim1, dim2, 1),                                               //
        cpu_3d_meta_data(dim1, dim2, dim3),                                            //
        gpu_3d_meta_data(dim1, dim2, dim3),                                            //
        cpu_4d_meta_data(dim1, dim2, dim3, dim4),                                      //
        gpu_4d_meta_data(dim1, dim2, dim3, dim4),                                      //
        cpu_2d_real_storage_input(cpu_2d_real_meta_data, "cpu_2d_real_storage_input"), //
        gpu_2d_real_storage_input(gpu_2d_real_meta_data, "gpu_2d_real_storage_input"), //
        cpu_2d_storage_input(cpu_2d_meta_data, "cpu_2d_storage_input"),                //
        gpu_2d_storage_input(gpu_2d_meta_data, "gpu_2d_storage_input"),                //
        cpu_3d_storage_input(cpu_3d_meta_data, "cpu_3d_storage_input"),                //
        gpu_3d_storage_input(gpu_3d_meta_data, "gpu_3d_storage_input"),                //
        cpu_4d_storage_input(cpu_4d_meta_data, "cpu_4d_storage_input"),                //
        gpu_4d_storage_input(gpu_4d_meta_data, "gpu_4d_storage_input"),
        cpu_2d_real_storage_output(cpu_2d_real_meta_data, "cpu_2d_real_storage_output"), //
        gpu_2d_real_storage_output(gpu_2d_real_meta_data, "gpu_2d_real_storage_output"), //
        cpu_2d_storage_output(cpu_2d_meta_data, "cpu_2d_storage_output"),                //
        gpu_2d_storage_output(gpu_2d_meta_data, "gpu_2d_storage_output"),                //
        cpu_3d_storage_output(cpu_3d_meta_data, "cpu_3d_storage_output"),                //
        gpu_3d_storage_output(gpu_3d_meta_data, "gpu_3d_storage_output"),                //
        cpu_4d_storage_output(cpu_4d_meta_data, "cpu_4d_storage_output"),                //
        gpu_4d_storage_output(gpu_4d_meta_data, "gpu_4d_storage_output") {
    storage_types::init2DReal(cpu_2d_real_storage_input, dim1, dim2);
    storage_types::init2DReal(gpu_2d_real_storage_input, dim1, dim2);
    storage_types::init2D(cpu_2d_storage_input, dim1, dim2);
    storage_types::init2D(gpu_2d_storage_input, dim1, dim2);

    storage_types::init3D(cpu_3d_storage_input, dim1, dim2, dim3);
    storage_types::init3D(gpu_3d_storage_input, dim1, dim2, dim3);

    storage_types::init4D(cpu_4d_storage_input, dim1, dim2, dim3, dim4);
    storage_types::init4D(gpu_4d_storage_input, dim1, dim2, dim3, dim4);
  }

  virtual void SetUp() override { Base::SetUp(); }

  template <typename Storage>
  void validate2DReal(Storage& in, Storage& out) {
    auto vin = make_host_view(in);
    auto vout = make_host_view(out);

    for(int i = 0; i < this->dim1; ++i)
      for(int j = 0; j < this->dim2; ++j) {
        ASSERT_EQ(vin(i, j), vout(i, j)) << "(i,j) = (" << i << "," << j << ")";
      }
  }
  template <typename Storage>
  void validate2D(Storage& in, Storage& out) {
    auto vin = make_host_view(in);
    auto vout = make_host_view(out);

    for(int i = 0; i < this->dim1; ++i)
      for(int j = 0; j < this->dim2; ++j) {
        ASSERT_EQ(vin(i, j, 0), vout(i, j, 0)) << "(i,j) = (" << i << "," << j << ")";
      }
  }
  template <typename Storage>
  void validate3D(Storage& in, Storage& out) {
    auto vin = make_host_view(in);
    auto vout = make_host_view(out);

    for(int i = 0; i < this->dim1; ++i)
      for(int j = 0; j < this->dim2; ++j)
        for(int k = 0; k < this->dim3; ++k) {
          ASSERT_EQ(vin(i, j, k), vout(i, j, k)) << "(i,j,k) = (" << i << "," << j << "," << k
                                                 << ")";
        }
  }
  template <typename Storage>
  void validate4D(Storage& in, Storage& out) {
    auto vin = make_host_view(in);
    auto vout = make_host_view(out);

    for(int i = 0; i < this->dim1; ++i)
      for(int j = 0; j < this->dim2; ++j)
        for(int k = 0; k < this->dim3; ++k)
          for(int l = 0; l < this->dim4; ++l) {
            ASSERT_EQ(vin(i, j, k, l), vout(i, j, k, l)) << "(i,j,k,l) = (" << i << "," << j << ","
                                                         << k << "," << l << ")";
          }
  }
};

using TestTypes = testing::Types<double, float, int>;

} // anonymous namespace

TYPED_TEST_CASE(GridToolsReadWriteTest, TestTypes);

TYPED_TEST(GridToolsReadWriteTest, WriteAndRead) {

  // Writing
  {
    serializer ser(open_mode::Write, this->directory->path().string(), "Field", "Binary");

    // Register fields
    type_id type = serialbox::ToTypeID<TypeParam>::value;
    ser.register_field("2d_real", type, std::vector<int>{this->dim1, this->dim2});

    // We set the empty dimension to 1 (gridtools sets it to 0 but this case should be handled in
    // the SerializerImpl)
    ser.register_field("2d", type, std::vector<int>{this->dim1, this->dim2, 1});

    ser.register_field("3d", type, std::vector<int>{this->dim1, this->dim2, this->dim3});

    ser.register_field("4d", type,
                       std::vector<int>{this->dim1, this->dim2, this->dim3, this->dim4});

    // Writing (implicitly register the savepoints)
    ser.write("2d_real", savepoint("cpu"), this->cpu_2d_real_storage_input);
    ser.write("2d_real", savepoint("gpu"), this->gpu_2d_real_storage_input);

    ser.write("2d", savepoint("cpu"), this->cpu_2d_storage_input);
    ser.write("2d", savepoint("gpu"), this->gpu_2d_storage_input);

    ser.write("3d", savepoint("cpu"), this->cpu_3d_storage_input);
    ser.write("3d", savepoint("gpu"), this->gpu_3d_storage_input);

    ser.write("4d", savepoint("cpu"), this->cpu_4d_storage_input);
    ser.write("4d", savepoint("gpu"), this->gpu_4d_storage_input);
  }

  // Reading
  {
    serializer ser(open_mode::Read, this->directory->path().string(), "Field", "Binary");

    const auto& savepoints = ser.savepoints();
    ASSERT_EQ(savepoints.size(), 2);
    ASSERT_TRUE(ser.has_savepoint(savepoint("cpu")));
    ASSERT_TRUE(ser.has_savepoint(savepoint("gpu")));

    ser.read("2d_real", savepoint("cpu"), this->cpu_2d_real_storage_output);
    ser.read("2d_real", savepoint("gpu"), this->gpu_2d_real_storage_output);

    ser.read("2d", savepoint("cpu"), this->cpu_2d_storage_output);
    ser.read("2d", savepoint("gpu"), this->gpu_2d_storage_output);

    ser.read("3d", savepoint("cpu"), this->cpu_3d_storage_output);
    ser.read("3d", savepoint("gpu"), this->gpu_3d_storage_output);

    ser.read("4d", savepoint("cpu"), this->cpu_4d_storage_output);
    ser.read("4d", savepoint("gpu"), this->gpu_4d_storage_output);
  }

  // Validate
  this->validate2DReal(this->cpu_2d_real_storage_input, this->cpu_2d_real_storage_output);
  this->validate2DReal(this->gpu_2d_real_storage_input, this->gpu_2d_real_storage_output);
  this->validate2D(this->cpu_2d_storage_input, this->cpu_2d_storage_output);
  this->validate2D(this->gpu_2d_storage_input, this->gpu_2d_storage_output);
  this->validate3D(this->cpu_3d_storage_input, this->cpu_3d_storage_output);
  this->validate3D(this->gpu_3d_storage_input, this->gpu_3d_storage_output);
  this->validate4D(this->cpu_4d_storage_input, this->cpu_4d_storage_output);
  this->validate4D(this->gpu_4d_storage_input, this->gpu_4d_storage_output);
}

TYPED_TEST(GridToolsReadWriteTest, ToAndFromFile) {
  using storage_types = serialbox::unittest::gridtools_storage_types<TypeParam>;

  typename storage_types::cpu_3d_storage_type storage_in(this->cpu_3d_meta_data, -1.0, "storage");
  typename storage_types::cpu_3d_storage_type storage_out(this->cpu_3d_meta_data, -1.0, "storage");
  // Fill data
  auto vin = make_host_view(storage_in);
  for(int i = 0; i < this->dim1; ++i)
    for(int j = 0; j < this->dim2; ++j)
      for(int k = 0; k < this->dim3; ++k)
        vin(i, j, k) = i * j * k;

  // Write and read from file
  serializer::to_file((this->directory->path() / "test.dat").string(), storage_in, "Binary");
  serializer::from_file((this->directory->path() / "test.dat").string(), storage_out, "Binary");

  // Verify
  auto vout = make_host_view(storage_in);
  for(int i = 0; i < this->dim1; ++i)
    for(int j = 0; j < this->dim2; ++j)
      for(int k = 0; k < this->dim3; ++k)
        ASSERT_EQ(vin(i, j, k), vout(i, j, k)) << "(i,j,k) = (" << i << "," << j << "," << k << ")";
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

TYPED_TEST(GridToolsReadWriteTest, NonGridToolsWriteGridToolsRead) {
  using Storage = serialbox::unittest::Storage<TypeParam>;
  Storage storage_in(Storage::RowMajor, {this->dim1, this->dim2, this->dim3}, Storage::random);

  using storage_types = serialbox::unittest::gridtools_storage_types<TypeParam>;
  typename storage_types::gpu_3d_storage_type storage_out(this->gpu_3d_meta_data, -1.0, "storage");

  auto sv_input = storage_in.toStorageView();

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
    ser.read("storage", savepoint("sp"), storage_out);
  }

  // Verify
  auto gt_view_out = make_host_view(storage_out);
  for(int i = 0; i < this->dim1; ++i)
    for(int j = 0; j < this->dim2; ++j)
      for(int k = 0; k < this->dim3; ++k)
        ASSERT_EQ(storage_in(i, j, k), gt_view_out(i, j, k)) << "(i,j,k) = (" << i << "," << j
                                                             << "," << k << ")";
}

TYPED_TEST(GridToolsReadWriteTest, GridToolsWriteNonGridToolsRead) {
  using storage_types = serialbox::unittest::gridtools_storage_types<TypeParam>;
  typename storage_types::gpu_3d_storage_type storage_in(this->gpu_3d_meta_data, -1.0, "storage");

  storage_types::init3D(storage_in, this->dim1, this->dim2, this->dim3);

  using Storage = serialbox::unittest::Storage<TypeParam>;
  Storage storage_out(Storage::RowMajor, {this->dim1, this->dim2, this->dim3}, Storage::random);

  auto sv_output = storage_out.toStorageView();

  // Write
  {
    serializer ser(open_mode::Write, this->directory->path().string(), "Field", "Binary");
    ser.write("storage", savepoint("sp"), storage_in);
  }

  // Read
  {
    serializer ser(open_mode::Read, this->directory->path().string(), "Field", "Binary");
    ser.read("storage", savepoint("sp"), sv_output.template originPtrAs<TypeParam>(),
             sv_output.strides());
  }

  // Verify
  auto gt_view_in = make_host_view(storage_in);
  for(int i = 0; i < this->dim1; ++i)
    for(int j = 0; j < this->dim2; ++j)
      for(int k = 0; k < this->dim3; ++k)
        ASSERT_EQ(gt_view_in(i, j, k), storage_out(i, j, k)) << "(i,j,k) = (" << i << "," << j
                                                             << "," << k << ")";
}

#endif // SERIALBOX_HAS_GRIDTOOLS
