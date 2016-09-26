//===-- Unittest/Cpp/UnittestSerializerImpl.cpp -------------------------------------*- C++ -*-===//
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

#include "Utility/FileUtility.h"
#include "Utility/Storage.h"
#include "serialbox/Core/SerializerImpl.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

//===------------------------------------------------------------------------------------------===//
//     Utility tests
//===------------------------------------------------------------------------------------------===//

class SerializerImplUtilityTest : public testing::Test {
public:
  std::shared_ptr<Directory> directory;

protected:
  virtual void SetUp() override {
    directory = std::make_shared<Directory>(UnittestEnvironment::getInstance().directory() /
                                            UnittestEnvironment::getInstance().testCaseName() /
                                            UnittestEnvironment::getInstance().testName());
  }

  virtual void TearDown() override { directory.reset(); }
};

TEST_F(SerializerImplUtilityTest, ConstructionOfEmptySerializer) {
  // -----------------------------------------------------------------------------------------------
  // OpenModeKind::Write
  // -----------------------------------------------------------------------------------------------
  {
    // Open fresh serializer and write meta data to disk
    SerializerImpl s(OpenModeKind::Write, directory->path().string(), "BinaryArchive");
    s.updateMetaData();
  }

  {
    // Non-empty directory -> Exception
    ASSERT_THROW((SerializerImpl(OpenModeKind::Write, directory->path().string(), "BinaryArchive")),
                 Exception);
  }

  {
    // Directory does not exists (should be created by the Archive)
    SerializerImpl s(OpenModeKind::Append,
                     (directory->path() / "dir-is-created-from-write").string(), "BinaryArchive");
    ASSERT_TRUE(boost::filesystem::exists(directory->path() / "dir-is-created-from-write"));
    s.updateMetaData();
  }

  // -----------------------------------------------------------------------------------------------
  // OpenModeKind::Read
  // -----------------------------------------------------------------------------------------------
  {
    // MetaData.json exists (from Writing part)
    SerializerImpl s(OpenModeKind::Read, directory->path().string(), "BinaryArchive");
  }

  {
    // Directory does not exist -> Exception
    ASSERT_THROW(SerializerImpl(OpenModeKind::Read, (directory->path() / "not-a-dir").string(),
                                "BinaryArchive"),
                 Exception);
  }

  {
    // MetaData.json does not exist -> Exception
    boost::filesystem::remove((directory->path() / "dir-is-created-from-write") /
                              SerializerImpl::SerializerMetaDataFile);
    ASSERT_THROW(SerializerImpl(OpenModeKind::Read,
                                (directory->path() / "dir-is-created-from-write").string(),
                                "BinaryArchive"),
                 Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // OpenModeKind::Append
  // -----------------------------------------------------------------------------------------------
  {
    // Construct from existing (empty) metaData
    SerializerImpl s(OpenModeKind::Append, directory->path().string(), "BinaryArchive");
  }

  {
    // Directory does not exists (should be created by the Archive)
    SerializerImpl s(OpenModeKind::Append,
                     (directory->path() / "dir-is-created-from-append").string(), "BinaryArchive");
    ASSERT_TRUE(boost::filesystem::exists(directory->path() / "dir-is-created-from-append"));
  }
}

TEST_F(SerializerImplUtilityTest, AddMetaInfo) {
  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "BinaryArchive");

  // Add some meta-info
  s.addGlobalMetaInfo("bool", bool(true));
  s.addGlobalMetaInfo("int32", int(32));
  s.addGlobalMetaInfo("int64", std::int64_t(64));
  s.addGlobalMetaInfo("float32", float(32.0f));
  s.addGlobalMetaInfo("float64", double(64.0f));
  s.addGlobalMetaInfo("string", "str"); // This has to go through the const char* specialization

  // Query meta-info
  EXPECT_EQ(s.getGlobalMetainfoAs<bool>("bool"), bool(true));
  EXPECT_EQ(s.globalMetaInfo().at("bool").as<bool>(), bool(true));
  ASSERT_THROW(s.getGlobalMetainfoAs<double>("bool"), Exception);
  ASSERT_THROW(s.getGlobalMetainfoAs<bool>("bool-not-present"), Exception);

  EXPECT_EQ(s.getGlobalMetainfoAs<int>("int32"), int(32));
  EXPECT_EQ(s.getGlobalMetainfoAs<std::int64_t>("int64"), std::int64_t(64));
  EXPECT_EQ(s.getGlobalMetainfoAs<float>("float32"), float(32.0f));
  EXPECT_EQ(s.getGlobalMetainfoAs<double>("float64"), double(64.0f));
  EXPECT_EQ(s.getGlobalMetainfoAs<std::string>("string"), "str");
}

TEST_F(SerializerImplUtilityTest, RegisterSavepoints) {
  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "BinaryArchive");

  Savepoint savepoint1("savepoint1");
  ASSERT_NO_THROW(savepoint1.addMetaInfo("key1", "s1"));

  // Add savepoint by copying/moving
  ASSERT_TRUE(s.registerSavepoint(savepoint1));
  EXPECT_EQ(s.savepoints().size(), 1);
  EXPECT_EQ(s.savepoints()[0], savepoint1);

  ASSERT_TRUE(s.addFieldToSavepoint(savepoint1, FieldID{"u", 0}));
  ASSERT_FALSE(s.addFieldToSavepoint(savepoint1, FieldID{"u", 0}));
  ASSERT_TRUE(s.addFieldToSavepoint(savepoint1, FieldID{"v", 1}));

  // Add empty savepoint given by name
  ASSERT_TRUE(s.registerSavepoint("savepoint2"));
  EXPECT_EQ(s.savepoints().size(), 2);
  EXPECT_EQ(s.savepoints()[1], Savepoint("savepoint2"));

  // Add savepoint given by name and metainfo
  MetaInfoMap metaInfo;
  metaInfo.insert("key1", float(3));
  ASSERT_TRUE(s.registerSavepoint("savepoint3", metaInfo));
  EXPECT_EQ(s.savepoints().size(), 3);
  EXPECT_EQ(s.savepoints()[2], Savepoint("savepoint3", metaInfo));

  // Add savepoint with same name but diffrent meta-info
  ASSERT_TRUE(s.registerSavepoint("savepoint4", metaInfo));
  EXPECT_EQ(s.savepoints().size(), 4);
  EXPECT_EQ(s.savepoints()[3], Savepoint("savepoint4", metaInfo));

  // Add savepoint with same name and same meta-info -> Fail
  ASSERT_FALSE(s.registerSavepoint("savepoint4", metaInfo));
  EXPECT_EQ(s.savepoints().size(), 4);
}

TEST_F(SerializerImplUtilityTest, RegisterFields) {
  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "BinaryArchive");

  // Perfect forwarding
  MetaInfoMap metaInfoField1(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("field1_key1_str"))}, {"key2", MetaInfoValue(double(3))}});
  s.registerField("field1", TypeID::Float64, std::vector<int>{20, 15, 20}, metaInfoField1);

  // Copy constructor
  MetaInfoMap metaInfoField2(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("field2_key1_str"))}, {"key2", MetaInfoValue(double(4))}});
  s.registerField("field2",
                  FieldMetaInfo(TypeID::Float32, std::vector<int>{20, 15}, metaInfoField2));

  // Check field1
  ASSERT_TRUE(s.hasField("field1"));
  ASSERT_TRUE(s.fieldMap().hasField("field1"));
  EXPECT_EQ(s.fieldMap().getTypeOf("field1"), TypeID::Float64);
  EXPECT_EQ(s.fieldMap().getDimsOf("field1"), (std::vector<int>{20, 15, 20}));
  EXPECT_EQ(s.getFieldMetaInfoOf("field1").metaInfo().at("key1").as<std::string>(),
            "field1_key1_str");
  EXPECT_EQ(s.getFieldMetaInfoOf("field1").metaInfo().at("key2").as<double>(), 3);

  // Add additional meta-information to field1
  s.addFieldMetaInfo("field1", "key3", float(2));
  EXPECT_EQ(s.getFieldMetaInfoOf("field1").metaInfo().at("key3").as<float>(), float(2));

  // Add additional meta-information to field1 with existing key -> Fail
  ASSERT_FALSE(s.addFieldMetaInfo("field1", "key3", int(5)));

  // Add additional meta-information to non-existing field -> Exception
  ASSERT_THROW(s.addFieldMetaInfo("fieldXXX", "key1", int(5)), Exception);

  // Check field2
  ASSERT_TRUE(s.hasField("field2"));
  ASSERT_TRUE(s.fieldMap().hasField("field2"));
  EXPECT_EQ(s.fieldMap().getTypeOf("field2"), TypeID::Float32);
  EXPECT_EQ(s.fieldMap().getDimsOf("field2"), (std::vector<int>{20, 15}));
  EXPECT_EQ(s.getFieldMetaInfoOf("field2").metaInfo().at("key1").as<std::string>(),
            "field2_key1_str");
  EXPECT_EQ(s.getFieldMetaInfoOf("field2").metaInfo().at("key2").as<double>(), 4);

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

  Savepoint savepoint("savepoint");

  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "BinaryArchive");
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

  {
    // Wrong mode -> Exception
    SerializerImpl s_read(OpenModeKind::Read, directory->path().string(), "BinaryArchive");
    ASSERT_THROW(s_read.write("field", savepoint, sv), Exception);
  }
}

TEST_F(SerializerImplUtilityTest, ReadExceptions) {
  Storage<double> storage(Storage<double>::ColMajor, {5, 1, 1});
  StorageView sv = storage.toStorageView();

  {
    SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "BinaryArchive");
    s_write.registerField("field", sv.type(), sv.dims());
    s_write.updateMetaData();
  }

  SerializerImpl s(OpenModeKind::Read, directory->path().string(), "BinaryArchive");
  Savepoint savepoint("savepoint");

  // Wrong mode -> Exception
  ASSERT_THROW(s.write("field", savepoint, sv), Exception);

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
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "BinaryArchive");

  // Add some meta-info
  s_write.addGlobalMetaInfo("bool", bool(true));
  s_write.addGlobalMetaInfo("int32", int(32));
  s_write.addGlobalMetaInfo("int64", std::int64_t(64));
  s_write.addGlobalMetaInfo("float32", float(32.0f));
  s_write.addGlobalMetaInfo("float64", double(64.0f));
  s_write.addGlobalMetaInfo("string", "str");

  // Register fields
  MetaInfoMap metaInfoField1(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("field1_key1_str"))}, {"key2", MetaInfoValue(double(3))}});
  s_write.registerField("field1", TypeID::Float64, std::vector<int>{20, 15, 20}, metaInfoField1);

  MetaInfoMap metaInfoField2(std::initializer_list<MetaInfoMap::value_type>{
      {"key1", MetaInfoValue(std::string("field2_key1_str"))}, {"key2", MetaInfoValue(double(4))}});
  s_write.registerField("field2",
                        FieldMetaInfo(TypeID::Float32, std::vector<int>{20, 15}, metaInfoField2));

  // Add savepoints
  Savepoint savepoint1("savepoint");
  ASSERT_NO_THROW(savepoint1.addMetaInfo("key1", "savepoint_key1_first"));
  Savepoint savepoint2("savepoint");
  ASSERT_NO_THROW(savepoint2.addMetaInfo("key1", "savepoint_key1_second"));

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
  SerializerImpl s_read(OpenModeKind::Read, directory->path().string(), "BinaryArchive");

  // Global meta-info
  ASSERT_TRUE(s_read.globalMetaInfo().hasKey("bool"));
  EXPECT_EQ(bool(s_read.globalMetaInfo()["bool"]), true);

  ASSERT_TRUE(s_read.globalMetaInfo().hasKey("int32"));
  EXPECT_EQ(int(s_read.globalMetaInfo()["int32"]), int(32));

  ASSERT_TRUE(s_read.globalMetaInfo().hasKey("int64"));
  EXPECT_EQ(std::int64_t(s_read.globalMetaInfo()["int64"]), 64);

  ASSERT_TRUE(s_read.globalMetaInfo().hasKey("float32"));
  EXPECT_EQ(float(s_read.globalMetaInfo()["float32"]), 32.0f);

  ASSERT_TRUE(s_read.globalMetaInfo().hasKey("float64"));
  EXPECT_EQ(double(s_read.globalMetaInfo()["float64"]), 64.0);

  ASSERT_TRUE(s_read.globalMetaInfo().hasKey("string"));
  EXPECT_EQ(s_read.globalMetaInfo().at("string").as<std::string>(), "str");

  // Savepoints
  ASSERT_EQ(s_read.savepoints().size(), 3);

  const Savepoint& sp1 = s_read.savepoints()[0];
  const Savepoint& sp2 = s_read.savepoints()[1];
  const Savepoint& sp3 = s_read.savepoints()[2];
  EXPECT_EQ(sp1.name(), "savepoint");
  EXPECT_EQ(sp2.name(), "savepoint");
  EXPECT_EQ(sp3.name(), "different-savepoint");

  EXPECT_EQ(s_read.getFieldOfSavepoint(sp1, "field1"), (FieldID{"field1", 0}));
  EXPECT_EQ(s_read.getFieldOfSavepoint(sp1, "field2"), (FieldID{"field2", 0}));
  EXPECT_EQ(s_read.getFieldOfSavepoint(sp2, "field1"), (FieldID{"field1", 1}));
  EXPECT_EQ(s_read.getFieldOfSavepoint(sp2, "field2"), (FieldID{"field2", 1}));

  // FieldMap
  ASSERT_TRUE(s_read.fieldMap().hasField("field1"));
  ASSERT_TRUE(s_read.fieldMap().hasField("field2"));

  ASSERT_EQ(s_read.fieldMap().getTypeOf("field1"), TypeID::Float64);
  ASSERT_EQ(s_read.fieldMap().getTypeOf("field2"), TypeID::Float32);

  ASSERT_EQ(s_read.fieldMap().getDimsOf("field1"), (std::vector<int>{20, 15, 20}));
  ASSERT_EQ(s_read.fieldMap().getDimsOf("field2"), (std::vector<int>{20, 15}));

  ASSERT_EQ(s_read.fieldMap().getMetaInfoOf("field1").at("key1").as<std::string>(),
            "field1_key1_str");
  ASSERT_EQ(s_read.fieldMap().getMetaInfoOf("field1").at("key2").as<double>(), 3.0);
  ASSERT_EQ(s_read.fieldMap().getMetaInfoOf("field2").at("key1").as<std::string>(),
            "field2_key1_str");
  ASSERT_EQ(s_read.fieldMap().getMetaInfoOf("field2").at("key2").as<double>(), 4.0);
}

TEST_F(SerializerImplUtilityTest, JSONFailEmpty) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "BinaryArchive");
  s_write.updateMetaData();

  // Erase MetaData.json -> this produces a parser error
  std::ofstream ofs((directory->path() / SerializerImpl::SerializerMetaDataFile).string(),
                    std::ios::trunc);
  ofs.close();

  // Open with empty MetaData.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "BinaryArchive"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, JSONFailCorruputedVersion) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "BinaryArchive");
  s_write.updateMetaData();

  // Read MetaData.json
  json::json j;
  std::ifstream ifs((directory->path() / SerializerImpl::SerializerMetaDataFile).string());
  ifs >> j;
  ifs.close();

  // Corruput version
  j["serialbox_version"] = int(j["serialbox_version"]) + 1;

  // Write MetaData.json
  std::ofstream ofs((directory->path() / SerializerImpl::SerializerMetaDataFile).string(),
                    std::ios::trunc);
  ofs << j.dump(4) << std::endl;
  ofs.close();

  // Open with corruputed MetaData.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "BinaryArchive"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, JSONFailNoVersion) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "BinaryArchive");
  s_write.updateMetaData();

  // Read MetaData.json
  json::json j;
  std::ifstream ifs((directory->path() / SerializerImpl::SerializerMetaDataFile).string());
  ifs >> j;
  ifs.close();

  // Remove version
  j.erase("serialbox_version");

  // Write MetaData.json
  std::ofstream ofs((directory->path() / SerializerImpl::SerializerMetaDataFile).string(),
                    std::ios::trunc);
  ofs << j.dump(4) << std::endl;
  ofs.close();

  // Open with corruputed MetaData.json
  ASSERT_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "BinaryArchive"),
               Exception);
}

TEST_F(SerializerImplUtilityTest, toString) {
  SerializerImpl s_write(OpenModeKind::Write, directory->path().string(), "BinaryArchive");
  std::stringstream ss;
  ss << s_write;
  EXPECT_NE(ss.str().find("mode"), std::string::npos);
  EXPECT_NE(ss.str().find("directory"), std::string::npos);
  EXPECT_NE(ss.str().find("FieldMap"), std::string::npos);
  EXPECT_NE(ss.str().find("MetaInfoMap"), std::string::npos);
  EXPECT_NE(ss.str().find("SavepointVector"), std::string::npos);
}

//===------------------------------------------------------------------------------------------===//
//     Read/Write tests
//===------------------------------------------------------------------------------------------===//

namespace {

template <class T>
class SerializerImplReadWriteTest : public testing::Test {
public:
  std::shared_ptr<Directory> directory;

protected:
  virtual void SetUp() override {
    directory = std::make_shared<Directory>(UnittestEnvironment::getInstance().directory() /
                                            UnittestEnvironment::getInstance().testCaseName() /
                                            UnittestEnvironment::getInstance().testName());
  }

  virtual void TearDown() override { directory.reset(); }
};

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
  Savepoint savepoint1_t_1("savepoint1");
  savepoint1_t_1.addMetaInfo("time", int(1));
  Savepoint savepoint1_t_2("savepoint1");
  savepoint1_t_2.addMetaInfo("time", int(2));
  Savepoint savepoint_u_1("savepoint_u_1");
  Savepoint savepoint_v_1("savepoint_v_1");
  Savepoint savepoint_6d("savepoint_6d");

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
    SerializerImpl s_write(OpenModeKind::Write, this->directory->path().string(), "BinaryArchive");

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
    SerializerImpl s_app(OpenModeKind::Append, this->directory->path().string(), "BinaryArchive");

    auto sv_field_6d = field_6d_input.toStorageView();
    s_app.registerField("field_6d", sv_field_6d.type(), sv_field_6d.dims());

    // Writing (implicitly register the savepoint)
    s_app.write("field_6d", savepoint_6d, sv_field_6d);
  }

  // -----------------------------------------------------------------------------------------------
  // Reading
  // -----------------------------------------------------------------------------------------------
  {
    SerializerImpl s_read(OpenModeKind::Read, this->directory->path().string(), "BinaryArchive");

    // StorageViews
    auto sv_u_0 = u_0_output.toStorageView();
    auto sv_u_1 = u_1_output.toStorageView();
    auto sv_v_0 = v_0_output.toStorageView();
    auto sv_v_1 = v_1_output.toStorageView();
    auto sv_field_6d = field_6d_output.toStorageView();
    
    // Check fields exists
    ASSERT_TRUE(s_read.hasField("u"));
    ASSERT_EQ(s_read.getFieldMetaInfoOf("u").dims(), (std::vector<int>{5, 6, 7}));

    ASSERT_TRUE(s_read.hasField("v"));
    ASSERT_EQ(s_read.getFieldMetaInfoOf("v").dims(), (std::vector<int>{5, 1, 1}));

    ASSERT_TRUE(s_read.hasField("field_6d"));
    ASSERT_EQ(s_read.getFieldMetaInfoOf("field_6d").dims(), (std::vector<int>{2, 2, 1, 2, 1, 2}));

    // Check order of savepoints is correct
    ASSERT_EQ(s_read.savepoints().size(), 5);
    EXPECT_EQ(s_read.savepoints(),
              (std::vector<Savepoint>{savepoint1_t_1, savepoint1_t_2, savepoint_u_1, savepoint_v_1,
                                      savepoint_6d}));
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
