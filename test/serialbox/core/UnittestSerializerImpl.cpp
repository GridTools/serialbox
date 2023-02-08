//===-- serialbox/core/UnittestSerializerImpl.cpp -----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the shared serializer implementation.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/Json.h"
#include "serialbox/core/SerializerImpl.h"
#include "utility/SerializerTestBase.h"
#include "utility/Storage.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

//===------------------------------------------------------------------------------------------===//
//     Utility tests
//===------------------------------------------------------------------------------------------===//

namespace {

class SerializerImplUtilityTest : public SerializerUnittestBase {};

} // anonymous namespace

TEST_F(SerializerImplUtilityTest, Construction) {
  // -----------------------------------------------------------------------------------------------
  // OpenModeKind::Write
  // -----------------------------------------------------------------------------------------------
  {
    // Open fresh serializer and write meta data to disk
    SerializerImpl s(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
    s.updateMetaData();
  }

  {
    // Directory does not exists (should be created by the Archive)
    SerializerImpl s(OpenModeKind::Append,
                     (directory->path() / "dir-is-created-from-write").string(), "Field", "Binary");
    ASSERT_TRUE(std::filesystem::exists(directory->path() / "dir-is-created-from-write"));
    s.updateMetaData();
  }

  // -----------------------------------------------------------------------------------------------
  // OpenModeKind::Read
  // -----------------------------------------------------------------------------------------------
  {
    // MetaData.json exists (from Writing part)
    SerializerImpl s(OpenModeKind::Read, directory->path().string(), "Field", "Binary");
  }

  {
    // Directory does not exist -> Exception
    ASSERT_THROW(SerializerImpl(OpenModeKind::Read, (directory->path() / "not-a-dir").string(),
                                "Field", "Binary"),
                 Exception);
  }

  {
    // MetaData-prefix.json does not exist -> Exception
    std::filesystem::remove((directory->path() / "dir-is-created-from-write") /
                            "MetaData-Field.json");
    ASSERT_THROW(SerializerImpl(OpenModeKind::Read,
                                (directory->path() / "dir-is-created-from-write").string(), "Field",
                                "Binary"),
                 Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // OpenModeKind::Append
  // -----------------------------------------------------------------------------------------------
  {
    // Construct from existing (empty) metaData
    SerializerImpl s(OpenModeKind::Append, directory->path().string(), "Field", "Binary");
  }

  {
    // Directory does not exists (should be created by the Archive)
    SerializerImpl s(OpenModeKind::Append,
                     (directory->path() / "dir-is-created-from-append").string(), "Field",
                     "Binary");
    ASSERT_TRUE(std::filesystem::exists(directory->path() / "dir-is-created-from-append"));
  }
}

TEST_F(SerializerImplUtilityTest, SerializationStatus) {
  // Serialization is enabled by default
  ASSERT_FALSE(SerializerImpl::serializationStatus() < 0);

  // Disable serialization
  SerializerImpl::disableSerialization();
  ASSERT_EQ(SerializerImpl::serializationStatus(), -1);

  // Enabled serialization
  SerializerImpl::enableSerialization();
  ASSERT_EQ(SerializerImpl::serializationStatus(), 1);
}

TEST_F(SerializerImplUtilityTest, AddMetainfo) {
  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "Field", "Binary");

  // Add some meta-info
  s.addGlobalMetainfo("bool", bool(true));
  s.addGlobalMetainfo("int32", int(32));
  s.addGlobalMetainfo("int64", std::int64_t(64));
  s.addGlobalMetainfo("float32", float(32.0f));
  s.addGlobalMetainfo("float64", double(64.0f));
  s.addGlobalMetainfo("string", "str"); // This has to go through the const char* specialization

  // Metainfo already exists -> Exception
  EXPECT_THROW(s.addGlobalMetainfo("string", "str"), Exception);

  // Query meta-info
  EXPECT_EQ(s.getGlobalMetainfoAs<bool>("bool"), bool(true));
  EXPECT_EQ(s.globalMetainfo().at("bool").as<bool>(), bool(true));
  ASSERT_THROW(s.getGlobalMetainfoAs<bool>("bool-not-present"), Exception);

  EXPECT_EQ(s.getGlobalMetainfoAs<int>("int32"), int(32));
  EXPECT_EQ(s.getGlobalMetainfoAs<std::int64_t>("int64"), std::int64_t(64));
  EXPECT_EQ(s.getGlobalMetainfoAs<float>("float32"), float(32.0f));
  EXPECT_EQ(s.getGlobalMetainfoAs<double>("float64"), double(64.0f));
  EXPECT_EQ(s.getGlobalMetainfoAs<std::string>("string"), "str");
}

TEST_F(SerializerImplUtilityTest, RegisterSavepoints) {
  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "Field", "Binary");

  SavepointImpl savepoint1("savepoint1");
  ASSERT_NO_THROW(savepoint1.addMetainfo("key1", "s1"));

  // Add savepoint by copying/moving
  ASSERT_TRUE(s.registerSavepoint(savepoint1));
  EXPECT_EQ(s.savepoints().size(), 1);
  EXPECT_EQ(*s.savepoints()[0], savepoint1);

  ASSERT_TRUE(s.addFieldToSavepoint(savepoint1, FieldID{"u", 0}));
  ASSERT_FALSE(s.addFieldToSavepoint(savepoint1, FieldID{"u", 0}));
  ASSERT_TRUE(s.addFieldToSavepoint(savepoint1, FieldID{"v", 1}));

  // Add empty savepoint given by name
  ASSERT_TRUE(s.registerSavepoint("savepoint2"));
  EXPECT_EQ(s.savepoints().size(), 2);
  EXPECT_EQ(*s.savepoints()[1], SavepointImpl("savepoint2"));

  // Add savepoint given by name and metainfo
  MetainfoMapImpl metaInfo;
  metaInfo.insert("key1", float(3));
  ASSERT_TRUE(s.registerSavepoint("savepoint3", metaInfo));
  EXPECT_EQ(s.savepoints().size(), 3);
  EXPECT_EQ(*s.savepoints()[2], SavepointImpl("savepoint3", metaInfo));

  // Add savepoint with same name but diffrent meta-info
  ASSERT_TRUE(s.registerSavepoint("savepoint4", metaInfo));
  EXPECT_EQ(s.savepoints().size(), 4);
  EXPECT_EQ(*s.savepoints()[3], SavepointImpl("savepoint4", metaInfo));

  // Add savepoint with same name and same meta-info -> Fail
  ASSERT_FALSE(s.registerSavepoint("savepoint4", metaInfo));
  EXPECT_EQ(s.savepoints().size(), 4);
}

TEST_F(SerializerImplUtilityTest, RegisterFields) {
  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "Field", "Binary");

  // Perfect forwarding
  MetainfoMapImpl metaInfoField1(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("field1_key1_str"))},
      {"key2", MetainfoValueImpl(double(3))}});
  s.registerField("field1", TypeID::Float64, std::vector<int>{20, 15, 20}, metaInfoField1);

  // Copy constructor
  MetainfoMapImpl metaInfoField2(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("field2_key1_str"))},
      {"key2", MetainfoValueImpl(double(4))}});
  s.registerField("field2",
                  FieldMetainfoImpl(TypeID::Float32, std::vector<int>{20, 15}, metaInfoField2));

  // Check field1
  ASSERT_TRUE(s.hasField("field1"));
  ASSERT_TRUE(s.fieldMap().hasField("field1"));
  EXPECT_EQ(s.fieldMap().getTypeOf("field1"), TypeID::Float64);
  EXPECT_EQ(s.fieldMap().getDimsOf("field1"), (std::vector<int>{20, 15, 20}));
  EXPECT_EQ(s.getFieldMetainfoImplOf("field1").metaInfo().at("key1").as<std::string>(),
            "field1_key1_str");
  EXPECT_EQ(s.getFieldMetainfoImplOf("field1").metaInfo().at("key2").as<double>(), 3);

  // Add additional meta-information to field1
  s.addFieldMetainfoImpl("field1", "key3", float(2));
  EXPECT_EQ(s.getFieldMetainfoImplOf("field1").metaInfo().at("key3").as<float>(), float(2));

  // Add additional meta-information to field1 with existing key -> Fail
  ASSERT_FALSE(s.addFieldMetainfoImpl("field1", "key3", int(5)));

  // Add additional meta-information to non-existing field -> Exception
  ASSERT_THROW(s.addFieldMetainfoImpl("fieldXXX", "key1", int(5)), Exception);

  // Check field2
  ASSERT_TRUE(s.hasField("field2"));
  ASSERT_TRUE(s.fieldMap().hasField("field2"));
  EXPECT_EQ(s.fieldMap().getTypeOf("field2"), TypeID::Float32);
  EXPECT_EQ(s.fieldMap().getDimsOf("field2"), (std::vector<int>{20, 15}));
  EXPECT_EQ(s.getFieldMetainfoImplOf("field2").metaInfo().at("key1").as<std::string>(),
            "field2_key1_str");
  EXPECT_EQ(s.getFieldMetainfoImplOf("field2").metaInfo().at("key2").as<double>(), 4);

  // Check fieldnames
  const auto& fields = s.fieldnames();
  EXPECT_TRUE(std::find(fields.begin(), fields.end(), std::string("field1")) != fields.end());
  EXPECT_TRUE(std::find(fields.begin(), fields.end(), std::string("field2")) != fields.end());
}

TEST_F(SerializerImplUtilityTest, WriteExceptions) {
  Storage<double> storage(Storage<double>::ColMajor, {5, 1, 1});
  Storage<float> storage_wrong_type(Storage<float>::ColMajor, {5, 1, 1});
  Storage<double> storage_wrong_dims1(Storage<double>::ColMajor, {5, 1});
  Storage<double> storage_wrong_dims2(Storage<double>::ColMajor, {3, 3, 3});

  StorageView sv = storage.toStorageView();
  StorageView sv_wrong_type = storage_wrong_type.toStorageView();
  StorageView sv_wrong_dims1 = storage_wrong_dims1.toStorageView();
  StorageView sv_wrong_dims2 = storage_wrong_dims2.toStorageView();

  SavepointImpl savepoint("savepoint");

  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
  s.updateMetaData();

  // Regsister field
  s.registerField("field", sv.type(), sv.dims());

  // Field is not registered -> Exception
  ASSERT_THROW(s.write("field-not-registered", savepoint, sv), Exception);

  // Field has wrong type -> Exception
  ASSERT_THROW(s.write("field", savepoint, sv_wrong_type), Exception);

  // Field has wrong dimensions -> Exception
  ASSERT_THROW(s.write("field", savepoint, sv_wrong_dims1), Exception);
  ASSERT_THROW(s.write("field", savepoint, sv_wrong_dims2), Exception);

  // Field has does already exist at savepoint -> Exception
  ASSERT_TRUE(s.registerSavepoint(savepoint));
  ASSERT_TRUE(s.savepointVector().addField(savepoint, FieldID{"field", 0}));
  ASSERT_THROW(s.write("field", savepoint, sv), Exception);

  // Wrong mode -> Exception
  ASSERT_THROW(s.read("field", savepoint, sv), Exception);

  {
    // Wrong mode -> Exception
    SerializerImpl s_read(OpenModeKind::Read, directory->path().string(), "Field", "Binary");
    ASSERT_THROW(s_read.write("field", savepoint, sv), Exception);
  }
}

TEST_F(SerializerImplUtilityTest, ReadExceptions) {
  Storage<double> storage(Storage<double>::ColMajor, {5, 1, 1});
  StorageView sv = storage.toStorageView();

  {
    SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
    s_write.registerField("field", sv.type(), sv.dims());
    s_write.updateMetaData();
  }

  SerializerImpl s(OpenModeKind::Read, directory->path().string(), "Field", "Binary");
  SavepointImpl savepoint("savepoint");

  // Savepoint does no exist -> Exception
  ASSERT_THROW(s.read("field", savepoint, sv), Exception);
}

//===------------------------------------------------------------------------------------------===//
//     JSON Serialization
//===------------------------------------------------------------------------------------------===//

TEST_F(SerializerImplUtilityTest, JSONSuccess) {
  // -----------------------------------------------------------------------------------------------
  // Writing
  // -----------------------------------------------------------------------------------------------
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");

  // Add some meta-info
  s_write.addGlobalMetainfo("bool", bool(true));
  s_write.addGlobalMetainfo("int32", int(32));
  s_write.addGlobalMetainfo("int64", std::int64_t(64));
  s_write.addGlobalMetainfo("float32", float(32.0f));
  s_write.addGlobalMetainfo("float64", double(64.0f));
  s_write.addGlobalMetainfo("string", "str");

  // Register fields
  MetainfoMapImpl metaInfoField1(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("field1_key1_str"))},
      {"key2", MetainfoValueImpl(double(3))}});
  s_write.registerField("field1", TypeID::Float64, std::vector<int>{20, 15, 20}, metaInfoField1);

  MetainfoMapImpl metaInfoField2(std::initializer_list<MetainfoMapImpl::value_type>{
      {"key1", MetainfoValueImpl(std::string("field2_key1_str"))},
      {"key2", MetainfoValueImpl(double(4))}});
  s_write.registerField(
      "field2", FieldMetainfoImpl(TypeID::Float32, std::vector<int>{20, 15}, metaInfoField2));

  // Add savepoints
  SavepointImpl savepoint1("savepoint");
  ASSERT_NO_THROW(savepoint1.addMetainfo("key1", "savepoint_key1_first"));
  SavepointImpl savepoint2("savepoint");
  ASSERT_NO_THROW(savepoint2.addMetainfo("key1", "savepoint_key1_second"));

  ASSERT_TRUE(s_write.registerSavepoint(savepoint1));
  ASSERT_TRUE(s_write.registerSavepoint(savepoint2));
  ASSERT_TRUE(s_write.registerSavepoint("different-savepoint"));

  // Add savepoints to field
  ASSERT_TRUE(s_write.addFieldToSavepoint(savepoint1, FieldID{"field1", 0}));
  ASSERT_TRUE(s_write.addFieldToSavepoint(savepoint1, FieldID{"field2", 0}));
  ASSERT_TRUE(s_write.addFieldToSavepoint(savepoint2, FieldID{"field1", 1}));
  ASSERT_TRUE(s_write.addFieldToSavepoint(savepoint2, FieldID{"field2", 1}));

  s_write.updateMetaData();

  // -----------------------------------------------------------------------------------------------
  // Reading
  // -----------------------------------------------------------------------------------------------

  // Open Reading Serializer and deserialize meta-data from JSON
  SerializerImpl s_read(OpenModeKind::Read, directory->path().string(), "Field", "Binary");

  // Global meta-info
  ASSERT_TRUE(s_read.globalMetainfo().hasKey("bool"));
  EXPECT_EQ(bool(s_read.globalMetainfo()["bool"]), true);

  ASSERT_TRUE(s_read.globalMetainfo().hasKey("int32"));
  EXPECT_EQ(int(s_read.globalMetainfo()["int32"]), int(32));

  ASSERT_TRUE(s_read.globalMetainfo().hasKey("int64"));
  EXPECT_EQ(std::int64_t(s_read.globalMetainfo()["int64"]), 64);

  ASSERT_TRUE(s_read.globalMetainfo().hasKey("float32"));
  EXPECT_EQ(float(s_read.globalMetainfo()["float32"]), 32.0f);

  ASSERT_TRUE(s_read.globalMetainfo().hasKey("float64"));
  EXPECT_EQ(double(s_read.globalMetainfo()["float64"]), 64.0);

  ASSERT_TRUE(s_read.globalMetainfo().hasKey("string"));
  EXPECT_EQ(s_read.globalMetainfo().at("string").as<std::string>(), "str");

  // Savepoints
  ASSERT_EQ(s_read.savepoints().size(), 3);

  const SavepointImpl& sp1 = *s_read.savepoints()[0];
  const SavepointImpl& sp2 = *s_read.savepoints()[1];
  const SavepointImpl& sp3 = *s_read.savepoints()[2];
  EXPECT_EQ(sp1.name(), "savepoint");
  EXPECT_EQ(sp2.name(), "savepoint");
  EXPECT_EQ(sp3.name(), "different-savepoint");

  EXPECT_EQ(s_read.getFieldIDAtSavepoint(sp1, "field1"), (FieldID{"field1", 0}));
  EXPECT_EQ(s_read.getFieldIDAtSavepoint(sp1, "field2"), (FieldID{"field2", 0}));
  EXPECT_EQ(s_read.getFieldIDAtSavepoint(sp2, "field1"), (FieldID{"field1", 1}));
  EXPECT_EQ(s_read.getFieldIDAtSavepoint(sp2, "field2"), (FieldID{"field2", 1}));

  // FieldMap
  ASSERT_TRUE(s_read.fieldMap().hasField("field1"));
  ASSERT_TRUE(s_read.fieldMap().hasField("field2"));

  ASSERT_EQ(s_read.fieldMap().getTypeOf("field1"), TypeID::Float64);
  ASSERT_EQ(s_read.fieldMap().getTypeOf("field2"), TypeID::Float32);

  ASSERT_EQ(s_read.fieldMap().getDimsOf("field1"), (std::vector<int>{20, 15, 20}));
  ASSERT_EQ(s_read.fieldMap().getDimsOf("field2"), (std::vector<int>{20, 15}));

  ASSERT_EQ(s_read.fieldMap().getMetainfoOf("field1").at("key1").as<std::string>(),
            "field1_key1_str");
  ASSERT_EQ(s_read.fieldMap().getMetainfoOf("field1").at("key2").as<double>(), 3.0);
  ASSERT_EQ(s_read.fieldMap().getMetainfoOf("field2").at("key1").as<std::string>(),
            "field2_key1_str");
  ASSERT_EQ(s_read.fieldMap().getMetainfoOf("field2").at("key2").as<double>(), 4.0);
}

TEST_F(SerializerImplUtilityTest, JSONFailEmpty) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
  s_write.updateMetaData();

  // Erase MetaData-prefix.json -> this produces a parser error
  std::ofstream ofs(s_write.metaDataFile().string(), std::ios::trunc);
  ofs.close();

  // Open with empty MetaData-preifx.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "Field", "Binary"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, JSONFailCorruputedVersion) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
  s_write.updateMetaData();

  // Read MetaData.json
  json::json j;
  std::ifstream ifs(s_write.metaDataFile().string());
  ifs >> j;
  ifs.close();

  // Corruput version
  j["serialbox_version"] = int(j["serialbox_version"]) + 1;

  // Write MetaData-prefix.json
  std::ofstream ofs(s_write.metaDataFile().string(), std::ios::trunc);
  ofs << j.dump(4) << std::endl;
  ofs.close();

  // Open with corruputed MetaData.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "Field", "Binary"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, JSONFailWrongPrefix) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
  s_write.updateMetaData();

  // Open with corruputed MetaData.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "X", "Binary"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, JSONFailNoVersion) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
  s_write.updateMetaData();

  // Read MetaData.json
  json::json j;
  std::ifstream ifs(s_write.metaDataFile().string());
  ifs >> j;
  ifs.close();

  // Remove version
  j.erase("serialbox_version");

  // Write MetaData-prefix.json
  std::ofstream ofs(s_write.metaDataFile().string(), std::ios::trunc);
  ofs << j.dump(4) << std::endl;
  ofs.close();

  // Open with corruputed MetaData.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "Field", "Binary"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, JSONFailNoPrefix) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
  s_write.updateMetaData();

  // Read MetaData-prefix.json
  json::json j;
  std::ifstream ifs(s_write.metaDataFile().string());
  ifs >> j;
  ifs.close();

  // Remove prefix
  j.erase("prefix");

  // Write MetaData-prefix.json
  std::ofstream ofs(s_write.metaDataFile().string(), std::ios::trunc);
  ofs << j.dump(4) << std::endl;
  ofs.close();

  // Open with corruputed MetaData-prefix.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "Field", "Binary"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, JSONFailCorruptedPrefix) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
  s_write.updateMetaData();

  // Read MetaData-prefix.json
  json::json j;
  std::ifstream ifs(s_write.metaDataFile().string());
  ifs >> j;
  ifs.close();

  j["prefix"] = "Field2";

  // Write MetaData-prefix.json
  std::ofstream ofs(s_write.metaDataFile().string(), std::ios::trunc);
  ofs << j.dump(4) << std::endl;
  ofs.close();

  // Open with corruputed MetaData-prefix.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "Field", "Binary"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, toString) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
  s_write.addGlobalMetainfo("key", 5);
  std::stringstream ss;
  ss << s_write;
  EXPECT_NE(ss.str().find("Write"), std::string::npos);
  EXPECT_NE(ss.str().find("Binary"), std::string::npos);
  EXPECT_NE(ss.str().find("Field"), std::string::npos);
  EXPECT_NE(ss.str().find("key"), std::string::npos);
}

#ifdef SERIALBOX_ASYNC_API
TEST_F(SerializerImplUtilityTest, AsyncRead) {
  using Storage = Storage<double>;
  Storage storage(Storage::ColMajor, {10, 15, 20}, Storage::random);

  Storage storage_1(Storage::ColMajor, {10, 15, 20});
  Storage storage_2(Storage::ColMajor, {10, 15, 20});
  Storage storage_3(Storage::ColMajor, {10, 15, 20});

  SavepointImpl sp("sp");

  // Write
  {
    SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "Field", "Binary");
    auto sv = storage.toStorageView();
    s_write.registerField("field", sv.type(), sv.dims());
    s_write.write("field", sp, sv);
  }

  // Read
  {
    SerializerImpl s_read(OpenModeKind::Read, directory->path().string(), "Field", "Binary");

    auto sv_1 = storage_1.toStorageView();
    auto sv_2 = storage_2.toStorageView();
    auto sv_3 = storage_3.toStorageView();

    s_read.readAsync("field", sp, sv_1);
    s_read.readAsync("field", sp, sv_2);
    s_read.readAsync("field", sp, sv_3);
    s_read.waitForAll();

    ASSERT_TRUE(Storage::verify(storage_1, storage));
    ASSERT_TRUE(Storage::verify(storage_2, storage));
    ASSERT_TRUE(Storage::verify(storage_3, storage));

    s_read.readAsync("field", sp, sv_1);
    s_read.readAsync("field", sp, sv_2);
    s_read.readAsync("field-XXX", sp, sv_3);
    ASSERT_THROW(s_read.waitForAll(), Exception);
  }
}
#endif

//===------------------------------------------------------------------------------------------===//
//     Read/Write tests
//===------------------------------------------------------------------------------------------===//

namespace {

template <class T>
class SerializerImplReadWriteTest : public SerializerUnittestBase {};

using TestTypes = testing::Types<double, float, int, std::int64_t>;

} // anonymous namespace

TYPED_TEST_CASE(SerializerImplReadWriteTest, TestTypes);

TYPED_TEST(SerializerImplReadWriteTest, WriteAndRead) {

  // -----------------------------------------------------------------------------------------------
  // Preparation
  // -----------------------------------------------------------------------------------------------
  using Storage = Storage<TypeParam>;

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
  SavepointImpl savepoint1_t_1("savepoint1");
  savepoint1_t_1.addMetainfo("time", int(1));
  SavepointImpl savepoint1_t_2("savepoint1");
  savepoint1_t_2.addMetainfo("time", int(2));
  SavepointImpl savepoint_u_1("savepoint_u_1");
  SavepointImpl savepoint_v_1("savepoint_v_1");
  SavepointImpl savepoint_6d("savepoint_6d");

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
    SerializerImpl s_write(OpenModeKind::Write, this->directory->path().string(), "Field",
                           "Binary");

    // StorageViews
    auto sv_u_0 = u_0_input.toStorageView();
    auto sv_u_1 = u_1_input.toStorageView();
    auto sv_v_0 = v_0_input.toStorageView();
    auto sv_v_1 = v_1_input.toStorageView();

    // Register fields
    s_write.registerField("u", sv_u_0.type(), sv_u_0.dims());
    s_write.registerField("v", sv_v_0.type(), sv_v_0.dims());

    // Writing (implicitly register the savepoints)
    s_write.write("u", savepoint1_t_1, sv_u_0);
    s_write.write("v", savepoint1_t_1, sv_v_0);
    s_write.write("u", savepoint1_t_2, sv_u_1);
    s_write.write("v", savepoint1_t_2, sv_v_1);
    s_write.write("u", savepoint_u_1, sv_u_1);
    s_write.write("v", savepoint_v_1, sv_v_1);
  }

  // Reopen serializer and append a data field
  {
    SerializerImpl s_app(OpenModeKind::Append, this->directory->path().string(), "Field", "Binary");

    auto sv_field_6d = field_6d_input.toStorageView();
    s_app.registerField("field_6d", sv_field_6d.type(), sv_field_6d.dims());

    // Writing (implicitly register the savepoint)
    s_app.write("field_6d", savepoint_6d, sv_field_6d);
  }

  // -----------------------------------------------------------------------------------------------
  // Reading
  // -----------------------------------------------------------------------------------------------
  {
    SerializerImpl s_read(OpenModeKind::Read, this->directory->path().string(), "Field", "Binary");

    // StorageViews
    auto sv_u_0 = u_0_output.toStorageView();
    auto sv_u_1 = u_1_output.toStorageView();
    auto sv_v_0 = v_0_output.toStorageView();
    auto sv_v_1 = v_1_output.toStorageView();
    auto sv_field_6d = field_6d_output.toStorageView();

    // Check fields exists
    ASSERT_TRUE(s_read.hasField("u"));
    ASSERT_EQ(s_read.getFieldMetainfoImplOf("u").dims(), (std::vector<int>{5, 6, 7}));

    ASSERT_TRUE(s_read.hasField("v"));
    ASSERT_EQ(s_read.getFieldMetainfoImplOf("v").dims(), (std::vector<int>{5, 1, 1}));

    ASSERT_TRUE(s_read.hasField("field_6d"));
    ASSERT_EQ(s_read.getFieldMetainfoImplOf("field_6d").dims(),
              (std::vector<int>{2, 2, 1, 2, 1, 2}));

    // Check order of savepoints is correct
    ASSERT_EQ(s_read.savepoints().size(), 5);

    EXPECT_EQ(*s_read.savepoints()[0], savepoint1_t_1);
    EXPECT_EQ(*s_read.savepoints()[1], savepoint1_t_2);
    EXPECT_EQ(*s_read.savepoints()[2], savepoint_u_1);
    EXPECT_EQ(*s_read.savepoints()[3], savepoint_v_1);
    EXPECT_EQ(*s_read.savepoints()[4], savepoint_6d);

    // Read
    s_read.read("u", savepoint1_t_1, sv_u_0);
    ASSERT_TRUE(Storage::verify(u_0_output, u_0_input));

    s_read.read("v", savepoint1_t_1, sv_v_0);
    ASSERT_TRUE(Storage::verify(v_0_output, v_0_input));

    s_read.read("u", savepoint1_t_2, sv_u_1);
    ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));

    s_read.read("v", savepoint1_t_2, sv_v_1);
    ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));

    s_read.read("u", savepoint_u_1, sv_u_1);
    ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));

    s_read.read("v", savepoint_v_1, sv_v_1);
    ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));

    s_read.read("field_6d", savepoint_6d, sv_field_6d);
    ASSERT_TRUE(Storage::verify(field_6d_output, field_6d_input));
  }
}

TYPED_TEST(SerializerImplReadWriteTest, SliceWriteAndRead) {
  using Storage = Storage<TypeParam>;

  int dim1 = 5;
  Storage storage_1d_input(Storage::RowMajor, {dim1}, Storage::sequential);
  Storage storage_1d_output(Storage::RowMajor, {dim1}, Storage::random);
  SavepointImpl sp("sp");

  // Write
  {
    SerializerImpl s_write(OpenModeKind::Write, this->directory->path().string(), "Field",
                           "Binary");

    auto sv = storage_1d_input.toStorageView();
    s_write.registerField("1d", sv.type(), sv.dims());
    s_write.write("1d", sp, sv);
  }

  // Read
  {
    SerializerImpl s_read(OpenModeKind::Read, this->directory->path().string(), "Field", "Binary");

    {
      auto sv = storage_1d_output.toStorageView();
      s_read.readSliced("1d", sp, sv, Slice());
      ASSERT_TRUE(Storage::verify(storage_1d_input, storage_1d_output));
    }

    storage_1d_output.forEach(Storage::random);

    {
      auto sv = storage_1d_output.toStorageView();
      s_read.readSliced("1d", sp, sv, Slice(0, -1, 2));
      ASSERT_EQ(storage_1d_input(0), storage_1d_output(0));
      ASSERT_EQ(storage_1d_input(2), storage_1d_output(2));
      ASSERT_EQ(storage_1d_input(4), storage_1d_output(4));
    }
  }

#ifdef SERIALBOX_HAS_NETCDF
  // Only Binary currently supports slicing
  {
    SerializerImpl s_write(OpenModeKind::Write, this->directory->path().string(), "Field2",
                           "NetCDF");
    s_write.updateMetaData();

    SerializerImpl s_read(OpenModeKind::Read, this->directory->path().string(), "Field2", "NetCDF");
    auto sv = storage_1d_output.toStorageView();

    ASSERT_THROW(s_read.readSliced("1d", sp, sv, Slice()), Exception);
  }
#endif
}

#ifdef SERIALBOX_RUN_LARGE_FILE_TESTS

TYPED_TEST(SerializerImplReadWriteTest, LargeFile) {
  using Storage = Storage<TypeParam>;

  // Allocate up to 4.1 GB storages. Note that binary archive creates a buffer which makes
  // it ~8.2 GB.
  std::cout << "[          ] Running large file tests ... " << std::flush;

  Storage field(Storage::RowMajor, {int((4.1 * (1 << 30)) / sizeof(TypeParam))},
                Storage::sequential);
  SavepointImpl savepoint("savepoint");

  // Write to disk
  {
    SerializerImpl ser(OpenModeKind::Write, this->directory->path().string(), "Field", "Binary");
    auto sv = field.toStorageView();
    ser.registerField("field", sv.type(), sv.dims());
    ser.write("field", savepoint, sv);
  }

  // Clear field
  field.forEach(Storage::random);

  // Read field from disk
  {
    SerializerImpl ser(OpenModeKind::Read, this->directory->path().string(), "Field", "Binary");
    auto sv = field.toStorageView();
    ser.read("field", savepoint, sv);
  }

  // Verify field is still sequential
  bool fieldsMatch = true;
  for(int i = 0; i < field.size(); ++i)
    if(SERIALBOX_BUILTIN_UNLIKELY(field(i) != i)) {
      fieldsMatch = false;
      break;
    }

  std::cout << (fieldsMatch ? "Done" : "FAILED") << std::endl;
  ASSERT_TRUE(fieldsMatch);
}

#endif
