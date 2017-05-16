//===-- serialbox-c/UnittestSerializer.cpp ------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface Serializer.
///
//===------------------------------------------------------------------------------------------===//

#include "utility/CInterfaceTestBase.h"
#include "utility/Storage.h"
#include "serialbox-c/FieldMetainfo.h"
#include "serialbox-c/Metainfo.h"
#include "serialbox-c/Savepoint.h"
#include "serialbox-c/Serializer.h"
#include <gtest/gtest.h>

namespace {

class CSerializerUtilityTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

static bool stringInArray(std::string str, serialboxArrayOfString_t* array) {
  return std::find_if(array->data, array->data + array->len, [&](const char* s) {
           return str == std::string(s);
         }) != (array->data + array->len);
}

TEST_F(CSerializerUtilityTest, Construction) {
  // -----------------------------------------------------------------------------------------------
  // Write
  // -----------------------------------------------------------------------------------------------
  {
    // Open fresh serializer and write meta data to disk
    serialboxSerializer_t* ser =
        serialboxSerializerCreate(Write, directory->path().c_str(), "Field", "Binary");
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    EXPECT_EQ(serialboxSerializerGetMode(ser), Write);
    EXPECT_STREQ(serialboxSerializerGetDirectory(ser), directory->path().c_str());
    EXPECT_STREQ(serialboxSerializerGetPrefix(ser), "Field");

    serialboxSerializerUpdateMetaData(ser);
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    serialboxSerializerDestroy(ser);
  }

  {
    // Directory does not exists (should be created by the Archive)
    serialboxSerializer_t* ser = serialboxSerializerCreate(
        Write, (directory->path() / "dir-is-created-from-write").c_str(), "Field", "Binary");
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    ASSERT_TRUE(boost::filesystem::exists(directory->path() / "dir-is-created-from-write"));

    serialboxSerializerUpdateMetaData(ser);
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    serialboxSerializerDestroy(ser);
  }

  // -----------------------------------------------------------------------------------------------
  // Read
  // -----------------------------------------------------------------------------------------------
  {
    // MetaData.json exists (from Writing part)
    serialboxSerializer_t* ser =
        serialboxSerializerCreate(Read, directory->path().c_str(), "Field", "Binary");
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    serialboxSerializerDestroy(ser);
  }

  {
    // Directory does not exist -> FatalError
    serialboxSerializer_t* ser = serialboxSerializerCreate(
        Read, (directory->path() / "not-a-dir").c_str(), "Field", "Binary");
    ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    serialboxSerializerDestroy(ser);
  }

  {
    // MetaData-prefix.json does not exist -> Exception
    boost::filesystem::remove((directory->path() / "dir-is-created-from-write") /
                              "MetaData-Field.json");

    serialboxSerializer_t* ser = serialboxSerializerCreate(
        Read, (directory->path() / "dir-is-created-from-write").c_str(), "Field", "Binary");
    ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    serialboxSerializerDestroy(ser);
  }

  // -----------------------------------------------------------------------------------------------
  // Append
  // -----------------------------------------------------------------------------------------------
  {
    // Construct from existing (empty) metaData
    serialboxSerializer_t* ser =
        serialboxSerializerCreate(Append, directory->path().c_str(), "Field", "Binary");
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    serialboxSerializerDestroy(ser);
  }

  {
    // Directory does not exists (should be created by the Archive)
    serialboxSerializer_t* ser = serialboxSerializerCreate(
        Append, (directory->path() / "dir-is-created-from-append").c_str(), "Field", "Binary");
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    ASSERT_TRUE(boost::filesystem::exists(directory->path() / "dir-is-created-from-append"));
    serialboxSerializerDestroy(ser);
  }
}

TEST_F(CSerializerUtilityTest, AddMetainfo) {
  serialboxSerializer_t* ser =
      serialboxSerializerCreate(Write, directory->path().c_str(), "Field", "Binary");

  // Add meta-information
  {
    serialboxMetainfo_t* metaInfo = serialboxSerializerGetGlobalMetainfo(ser);
    ASSERT_FALSE(metaInfo->ownsData);
    ASSERT_TRUE(serialboxMetainfoAddBoolean(metaInfo, "key", true));
    serialboxMetainfoDestroy(metaInfo);
  }

  // Query meta-information
  {
    serialboxMetainfo_t* metaInfo = serialboxSerializerGetGlobalMetainfo(ser);
    ASSERT_FALSE(metaInfo->ownsData);
    ASSERT_TRUE(serialboxMetainfoHasKey(metaInfo, "key"));
    EXPECT_EQ(serialboxMetainfoGetBoolean(metaInfo, "key"), true);
    serialboxMetainfoDestroy(metaInfo);
  }

  serialboxSerializerDestroy(ser);
}

TEST_F(CSerializerUtilityTest, RegisterSavepoints) {
  serialboxSerializer_t* ser =
      serialboxSerializerCreate(Write, directory->path().c_str(), "Field", "Binary");

  // Create Savepoints
  serialboxSavepoint_t* savepoint1 = serialboxSavepointCreate("savepoint1");
  serialboxMetainfoAddBoolean(serialboxSavepointGetMetainfo(savepoint1), "key", true);

  serialboxSavepoint_t* savepoint2 = serialboxSavepointCreate("savepoint2");

  // Add Savepoint to Serializer
  ASSERT_TRUE(serialboxSerializerAddSavepoint(ser, savepoint1));
  ASSERT_TRUE(serialboxSerializerAddSavepoint(ser, savepoint2));

  // Query Savepoints
  int numSavepoints = serialboxSerializerGetNumSavepoints(ser);
  ASSERT_EQ(numSavepoints, 2);

  serialboxSavepoint_t** savepoints = serialboxSerializerGetSavepointVector(ser);
  EXPECT_TRUE(serialboxSavepointEqual(savepoints[0], savepoint1));
  EXPECT_TRUE(serialboxSavepointEqual(savepoints[1], savepoint2));

  serialboxSavepointDestroy(savepoint1);
  serialboxSavepointDestroy(savepoint2);
  serialboxSerializerDestroySavepointVector(savepoints, numSavepoints);
  serialboxSerializerDestroy(ser);
}

TEST_F(CSerializerUtilityTest, RegisterFields) {
  serialboxSerializer_t* ser =
      serialboxSerializerCreate(Write, directory->path().c_str(), "Field", "Binary");

  // Create FieldMetainfoImpl
  int dims[3] = {10, 15, 20};
  serialboxFieldMetainfo_t* info = serialboxFieldMetainfoCreate(Float64, dims, 3);

  // Register field
  ASSERT_TRUE(serialboxSerializerAddField(ser, "field", info));
  ASSERT_FALSE(serialboxSerializerAddField(ser, "field", info));

  ASSERT_TRUE(serialboxSerializerHasField(ser, "field"));

  // Register field (old version)
  ASSERT_TRUE(
      serialboxSerializerAddField2(ser, "field2", Int32, 4, 42, 1, 1, 12, 1, 1, 0, 0, 0, 0, 2, 2));
  ASSERT_FALSE(
      serialboxSerializerAddField2(ser, "field2", Int32, 4, 42, 1, 1, 12, 1, 1, 0, 0, 0, 0, 2, 2));

  // Query fieldnames
  serialboxArrayOfString_t* fieldnames = serialboxSerializerGetFieldnames(ser);

  ASSERT_EQ(fieldnames->len, 2);
  EXPECT_TRUE(stringInArray("field", fieldnames));
  EXPECT_TRUE(stringInArray("field2", fieldnames));

  serialboxArrayOfStringDestroy(fieldnames);

  //
  // Get FieldMetainfoImpl of "field"
  //
  serialboxFieldMetainfo_t* infoField = serialboxSerializerGetFieldMetainfo(ser, "field");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  ASSERT_TRUE(serialboxFieldMetainfoEqual(info, infoField));

  //
  // Get FieldMetainfoImpl of "field2"
  //
  serialboxFieldMetainfo_t* infoField2 = serialboxSerializerGetFieldMetainfo(ser, "field2");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  // Number of dimension
  int numDimension = serialboxFieldMetainfoGetNumDimensions(infoField2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(numDimension, 4);

  // Dimensions
  const int* dimension = serialboxFieldMetainfoGetDimensions(infoField2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(dimension[0], 42);
  EXPECT_EQ(dimension[1], 1);
  EXPECT_EQ(dimension[2], 1);
  EXPECT_EQ(dimension[3], 12);

  // Type
  serialboxTypeID type = serialboxFieldMetainfoGetTypeID(infoField2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(type, Int32);

  // Meta information
  serialboxMetainfo_t* metaInfo = serialboxFieldMetainfoGetMetainfo(infoField2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxString_t name = serialboxMetainfoGetString(metaInfo, "__name");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_STREQ(name, "field2");
  std::free(name);

  serialboxString_t typeName = serialboxMetainfoGetString(metaInfo, "__elementtype");
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_STREQ(typeName, "int");
  std::free(typeName);

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__bytesperelement"), 4);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__rank"), 4);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__isize"), 42);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__jsize"), 1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__ksize"), 1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__lsize"), 12);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__iminushalosize"), 1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__iplushalosize"), 1);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__jminushalosize"), 0);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__jplushalosize"), 0);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__kminushalosize"), 0);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__kplushalosize"), 0);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__lminushalosize"), 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  EXPECT_EQ(serialboxMetainfoGetInt32(metaInfo, "__lplushalosize"), 2);
  ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

  serialboxMetainfoDestroy(metaInfo);

  // Get FieldMetainfoImpl of non-existing field -> NULL
  ASSERT_EQ(NULL, serialboxSerializerGetFieldMetainfo(ser, "X"));

  serialboxFieldMetainfoDestroy(info);
  serialboxFieldMetainfoDestroy(infoField);
  serialboxSerializerDestroy(ser);
}

TEST_F(CSerializerUtilityTest, StatelessSerialization) {
  using Storage = serialbox::unittest::Storage<double>;
  Storage storage_input(Storage::ColMajor, {5, 2, 5}, Storage::random);
  Storage storage_output(Storage::ColMajor, {5, 2, 5});

  // Create storage views for easier access
  serialbox::StorageView sv_input = storage_input.toStorageView();
  serialbox::StorageView sv_output = storage_output.toStorageView();

  // Write and read from file
  serialboxWriteToFile((directory->path() / "test.dat").c_str(), sv_input.originPtr(), Float64,
                       sv_input.dims().data(), sv_input.dims().size(), sv_input.strides().data(),
                       "field", "Binary");

  serialboxReadFromFile((directory->path() / "test.dat").c_str(), sv_output.originPtr(), Float64,
                        sv_output.dims().data(), sv_output.dims().size(),
                        sv_output.strides().data(), "field", "Binary");

  ASSERT_TRUE(Storage::verify(storage_input, storage_output));
}

namespace {

template <class T>
class CSerializerReadWriteTest : public serialbox::unittest::CInterfaceTestBase {};

using TestTypes = testing::Types<double, float, int, std::int64_t>;

} // anonymous namespace

TYPED_TEST_CASE(CSerializerReadWriteTest, TestTypes);

TYPED_TEST(CSerializerReadWriteTest, WriteAndRead) {

  // Default is enabled
  EXPECT_GT(serialboxSerializationStatus(), 0);

  serialboxDisableSerialization();
  EXPECT_EQ(serialboxSerializationStatus(), -1);

  serialboxEnableSerialization();
  EXPECT_EQ(serialboxSerializationStatus(), 1);

  // -----------------------------------------------------------------------------------------------
  // Preparation
  // -----------------------------------------------------------------------------------------------
  using Storage = serialbox::unittest::Storage<TypeParam>;

  // Prepare input data
  Storage u_0_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);
  Storage u_1_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);

  Storage v_0_input(Storage::ColMajor, {5, 1, 1}, Storage::random);
  Storage v_1_input(Storage::ColMajor, {5, 1, 1}, Storage::random);

  Storage field_6d_input(Storage::RowMajor, {2, 2, 1, 2, 1, 2}, Storage::random);

  // Prepare output
  Storage u_0_output(Storage::RowMajor, {5, 6, 7});
  Storage u_1_output(Storage::RowMajor, {5, 6, 7});

  Storage v_0_output(Storage::RowMajor, {5, 1, 1});
  Storage v_1_output(Storage::RowMajor, {5, 1, 1});

  Storage field_6d_output(Storage::RowMajor, {2, 2, 1, 2, 1, 2});

  // Savepoints
  serialboxSavepoint_t* savepoint1_t_1 = serialboxSavepointCreate("savepoint1");
  serialboxMetainfoAddBoolean(serialboxSavepointGetMetainfo(savepoint1_t_1), "time", int(1));

  serialboxSavepoint_t* savepoint1_t_2 = serialboxSavepointCreate("savepoint2");
  serialboxMetainfoAddBoolean(serialboxSavepointGetMetainfo(savepoint1_t_2), "time", int(2));

  serialboxSavepoint_t* savepoint_u_1 = serialboxSavepointCreate("savepoint_u_1");
  serialboxSavepoint_t* savepoint_v_1 = serialboxSavepointCreate("savepoint_v_1");
  serialboxSavepoint_t* savepoint_6d = serialboxSavepointCreate("savepoint_6d");

  // -----------------------------------------------------------------------------------------------
  // Writing / Appending
  // -----------------------------------------------------------------------------------------------
  //
  //  Savepoint     | MetaData   | Fields
  //  -------------------------------------
  //  savepoint1    | time: 1    | u_0, v_0
  //  savepoint1    | time: 2    | u_1, v_1
  //  savepoint_u_1 | -          | u_1
  //  savepoint_v_1 | -          | v_1
  //  savepoint_6d  | -          | field_6d
  //
  {
    serialboxSerializer_t* ser_write =
        serialboxSerializerCreate(Write, this->directory->path().c_str(), "Field", "Binary");

    // Register fields
    serialboxTypeID type = static_cast<serialboxTypeID>((int)serialbox::ToTypeID<TypeParam>::value);
    serialboxFieldMetainfo_t* info_u =
        serialboxFieldMetainfoCreate(type, u_0_input.dims().data(), u_0_input.dims().size());
    serialboxFieldMetainfo_t* info_v =
        serialboxFieldMetainfoCreate(type, v_0_input.dims().data(), v_0_input.dims().size());

    ASSERT_TRUE(serialboxSerializerAddField(ser_write, "u", info_u));
    ASSERT_TRUE(serialboxSerializerAddField(ser_write, "v", info_v));

    // Writing (implicitly register the savepoints)

    // u_0 at savepoint1_t_1
    serialboxSerializerWrite(ser_write, "u", savepoint1_t_1, (void*)u_0_input.originPtr(),
                             u_0_input.strides().data(), u_0_input.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    // v_0 at savepoint1_t_1
    serialboxSerializerWrite(ser_write, "v", savepoint1_t_1, (void*)v_0_input.originPtr(),
                             v_0_input.strides().data(), v_0_input.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    // u_1 at savepoint1_t_2
    serialboxSerializerWrite(ser_write, "u", savepoint1_t_2, (void*)u_1_input.originPtr(),
                             u_1_input.strides().data(), u_1_input.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    // v_1 at savepoint1_t_2
    serialboxSerializerWrite(ser_write, "v", savepoint1_t_2, (void*)v_1_input.originPtr(),
                             v_1_input.strides().data(), v_1_input.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    // u_1 at savepoint_u_1
    serialboxSerializerWrite(ser_write, "u", savepoint_u_1, (void*)u_1_input.originPtr(),
                             u_1_input.strides().data(), u_1_input.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    // v_1 at savepoint_v_1
    serialboxSerializerWrite(ser_write, "v", savepoint_v_1, (void*)v_1_input.originPtr(),
                             v_1_input.strides().data(), v_1_input.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    // Field does not exists -> FatalError
    serialboxSerializerWrite(ser_write, "XXX", savepoint_v_1, (void*)v_1_input.originPtr(),
                             v_1_input.strides().data(), v_1_input.strides().size());
    ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    // Inconsistent number of dimensions and strides -> FatalError
    serialboxSerializerWrite(ser_write, "u", savepoint_v_1, (void*)field_6d_input.originPtr(),
                             field_6d_input.strides().data(), field_6d_input.strides().size());
    ASSERT_TRUE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    serialboxFieldMetainfoDestroy(info_u);
    serialboxFieldMetainfoDestroy(info_v);
    serialboxSerializerDestroy(ser_write);
  }

  // Reopen serializer and append a data field
  {
    serialboxSerializer_t* ser_append =
        serialboxSerializerCreate(Append, this->directory->path().c_str(), "Field", "Binary");

    serialboxTypeID type = static_cast<serialboxTypeID>((int)serialbox::ToTypeID<TypeParam>::value);
    serialboxFieldMetainfo_t* info_field_6d = serialboxFieldMetainfoCreate(
        type, field_6d_input.dims().data(), field_6d_input.dims().size());
    ASSERT_TRUE(serialboxSerializerAddField(ser_append, "field_6d", info_field_6d));

    // field_6d at savepoint_6d
    serialboxSerializerWrite(ser_append, "field_6d", savepoint_6d,
                             (void*)field_6d_input.originPtr(), field_6d_input.strides().data(),
                             field_6d_input.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();

    serialboxFieldMetainfoDestroy(info_field_6d);
    serialboxSerializerDestroy(ser_append);
  }

  // -----------------------------------------------------------------------------------------------
  // Reading
  // -----------------------------------------------------------------------------------------------
  {
    serialboxSerializer_t* ser_read =
        serialboxSerializerCreate(Read, this->directory->path().c_str(), "Field", "Binary");

    // Check order of savepoints is correct
    int numSavepoints = serialboxSerializerGetNumSavepoints(ser_read);
    ASSERT_EQ(numSavepoints, 5);

    serialboxSavepoint_t** savepoints = serialboxSerializerGetSavepointVector(ser_read);

    EXPECT_TRUE(serialboxSavepointEqual(savepoints[0], savepoint1_t_1));
    EXPECT_TRUE(serialboxSavepointEqual(savepoints[1], savepoint1_t_2));
    EXPECT_TRUE(serialboxSavepointEqual(savepoints[2], savepoint_u_1));
    EXPECT_TRUE(serialboxSavepointEqual(savepoints[3], savepoint_v_1));
    EXPECT_TRUE(serialboxSavepointEqual(savepoints[4], savepoint_6d));

    EXPECT_TRUE(serialboxSerializerHasSavepoint(ser_read, savepoint1_t_1));

    serialboxSerializerDestroySavepointVector(savepoints, numSavepoints);

    // Check fields at savepoint
    serialboxArrayOfString_t* fieldsAtSavepoint =
        serialboxSerializerGetFieldnamesAtSavepoint(ser_read, savepoint1_t_1);

    ASSERT_EQ(fieldsAtSavepoint->len, 2);
    EXPECT_TRUE(stringInArray("u", fieldsAtSavepoint));
    EXPECT_TRUE(stringInArray("v", fieldsAtSavepoint));

    serialboxArrayOfStringDestroy(fieldsAtSavepoint);

    // Read

    // u_0 at savepoint1_t_1
    serialboxSerializerRead(ser_read, "u", savepoint1_t_1, (void*)u_0_output.originPtr(),
                            u_0_output.strides().data(), u_0_output.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    ASSERT_TRUE(Storage::verify(u_0_output, u_0_input));

    // v_0 at savepoint1_t_1
    serialboxSerializerRead(ser_read, "v", savepoint1_t_1, (void*)v_0_output.originPtr(),
                            v_0_output.strides().data(), v_0_output.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    ASSERT_TRUE(Storage::verify(v_0_output, v_0_input));

    // u_1 at savepoint1_t_2
    serialboxSerializerRead(ser_read, "u", savepoint1_t_2, (void*)u_1_output.originPtr(),
                            u_1_output.strides().data(), u_1_output.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));

    // v_1 at savepoint1_t_2
    serialboxSerializerRead(ser_read, "v", savepoint1_t_2, (void*)v_1_output.originPtr(),
                            v_1_output.strides().data(), v_1_output.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));

    // u_1 at savepoint_u_1
    serialboxSerializerRead(ser_read, "u", savepoint_u_1, (void*)u_1_output.originPtr(),
                            u_1_output.strides().data(), u_1_output.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));

    // v_1 at savepoint_v_1
    serialboxSerializerRead(ser_read, "v", savepoint_v_1, (void*)v_1_output.originPtr(),
                            v_1_output.strides().data(), v_1_output.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));

    // field_6d at savepoint_6d
    serialboxSerializerRead(ser_read, "field_6d", savepoint_6d, (void*)field_6d_output.originPtr(),
                            field_6d_output.strides().data(), field_6d_output.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
    ASSERT_TRUE(Storage::verify(field_6d_output, field_6d_input));

    serialboxSerializerDestroy(ser_read);
  }

  serialboxSavepointDestroy(savepoint1_t_1);
  serialboxSavepointDestroy(savepoint1_t_2);
  serialboxSavepointDestroy(savepoint_u_1);
  serialboxSavepointDestroy(savepoint_v_1);
  serialboxSavepointDestroy(savepoint_6d);
}

TYPED_TEST(CSerializerReadWriteTest, SliceWriteAndRead) {
  using Storage = serialbox::unittest::Storage<TypeParam>;

  int dim1 = 5;
  Storage storage_1d_input(Storage::RowMajor, {dim1}, Storage::sequential);
  Storage storage_1d_output(Storage::RowMajor, {dim1}, Storage::random);

  serialboxSavepoint_t* sp = serialboxSavepointCreate("sp");

  // Write
  {
    serialboxSerializer_t* ser_write =
        serialboxSerializerCreate(Write, this->directory->path().c_str(), "Field", "Binary");

    auto sv = storage_1d_input.toStorageView();

    // Register field
    serialboxTypeID type = static_cast<serialboxTypeID>((int)serialbox::ToTypeID<TypeParam>::value);
    serialboxFieldMetainfo_t* info_storage_1d = serialboxFieldMetainfoCreate(
        type, storage_1d_input.dims().data(), storage_1d_input.dims().size());
    ASSERT_TRUE(serialboxSerializerAddField(ser_write, "1d", info_storage_1d));

    // Write field
    serialboxSerializerWrite(ser_write, "1d", sp, (void*)sv.originPtr(), sv.strides().data(),
                             sv.strides().size());
    ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
  }

  // Read
  {
    serialboxSerializer_t* ser_read =
        serialboxSerializerCreate(Read, this->directory->path().c_str(), "Field", "Binary");

    {
      auto sv = storage_1d_output.toStorageView();
      
      int slice[] = {0, -1, 1};
      serialboxSerializerReadSliced(ser_read, "1d", sp, (void*)storage_1d_output.originPtr(),
                                    storage_1d_output.strides().data(),
                                    storage_1d_output.strides().size(),
                                    slice);
      ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
      ASSERT_TRUE(Storage::verify(storage_1d_input, storage_1d_output));
    }

    storage_1d_output.forEach(Storage::random);
    
    {
      auto sv = storage_1d_output.toStorageView();
      
      int slice[] = {0, -1, 2};
      serialboxSerializerReadSliced(ser_read, "1d", sp, (void*)storage_1d_output.originPtr(),
                                    storage_1d_output.strides().data(),
                                    storage_1d_output.strides().size(),
                                    slice);
      ASSERT_FALSE(this->hasErrorAndReset()) << this->getLastErrorMsg();
      ASSERT_EQ(storage_1d_input(0), storage_1d_output(0));
      ASSERT_EQ(storage_1d_input(2), storage_1d_output(2));
      ASSERT_EQ(storage_1d_input(4), storage_1d_output(4));
    }
  }
}
