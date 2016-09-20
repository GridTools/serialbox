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
#include "serialbox/Core/Archive/BinaryArchive.h"
#include "serialbox/Core/SerializerImpl.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

class SerializerImplTest : public testing::Test {
public:
  std::shared_ptr<Directory> directory;

  std::shared_ptr<std::vector<SavepointImpl>> savepoints;
  std::shared_ptr<FieldMap> fieldMap;
  std::shared_ptr<MetaInfoMap> globalMetaInfo;

protected:
  virtual void SetUp() override {
    directory = std::make_shared<Directory>(UnittestEnvironment::getInstance().directory() /
                                            UnittestEnvironment::getInstance().testCaseName() /
                                            UnittestEnvironment::getInstance().testName());

    // Savepoints
    savepoints = std::make_shared<std::vector<SavepointImpl>>();

    MetaInfoMap metaInfo1(std::initializer_list<MetaInfoMap::value_type>{
        {"key1", MetaInfoValue(std::string("Savepoint1_key1_str"))},
        {"key2", MetaInfoValue(double(1))}});
    savepoints->emplace_back("Savepoint1", metaInfo1,
                             std::vector<FieldID>{{"field1", 0}, {"field2", 0}});

    MetaInfoMap metaInfo2(std::initializer_list<MetaInfoMap::value_type>{
        {"key1", MetaInfoValue(std::string("Savepoint2_key1_str"))},
        {"key2", MetaInfoValue(double(2))}});
    savepoints->emplace_back("Savepoint2", metaInfo2,
                             std::vector<FieldID>{{"field1", 1}, {"field2", 1}});

    // Global meta-info
    globalMetaInfo = std::make_shared<MetaInfoMap>();
    globalMetaInfo->insert("bool", bool(true));
    globalMetaInfo->insert("int32", int(32));
    globalMetaInfo->insert("int64", std::int64_t(64));
    globalMetaInfo->insert("float32", float(32.0f));
    globalMetaInfo->insert("float64", double(64.0f));
    globalMetaInfo->insert("string", std::string("string"));

    // FieldMap
    fieldMap = std::make_shared<FieldMap>();

    MetaInfoMap metaInfoField1(std::initializer_list<MetaInfoMap::value_type>{
        {"key1", MetaInfoValue(std::string("field1_key1_str"))},
        {"key2", MetaInfoValue(double(3))}});
    fieldMap->insert("field1", TypeID::Float64, std::vector<int>{20, 15, 20}, metaInfoField1);

    MetaInfoMap metaInfoField2(std::initializer_list<MetaInfoMap::value_type>{
        {"key1", MetaInfoValue(std::string("field2_key1_str"))},
        {"key2", MetaInfoValue(double(4))}});
    fieldMap->insert("field2", TypeID::Float32, std::vector<int>{20, 15}, metaInfoField2);
  }

  virtual void TearDown() override { directory.reset(); }
};

TEST_F(SerializerImplTest, Empty) {
  // -----------------------------------------------------------------------------------------------
  // Writing
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
  // Reading
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
    ASSERT_THROW(SerializerImpl(OpenModeKind::Read, (directory->path() / "not-a-dir").string(),
                                "BinaryArchive"),
                 Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Appending
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

TEST_F(SerializerImplTest, AddMetaInfo) {
  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "BinaryArchive");

  s.addGlobalMetainfo("bool", bool(true));
  s.addGlobalMetainfo("int32", int(32));
  s.addGlobalMetainfo("int64", std::int64_t(64));
  s.addGlobalMetainfo("float32", float(32.0f));
  s.addGlobalMetainfo("float64", double(64.0f));
  s.addGlobalMetainfo("string", std::string("string"));

  EXPECT_EQ(s.getGlobalMetainfoAs<bool>("bool"), bool(true));
  ASSERT_THROW(s.getGlobalMetainfoAs<double>("bool"), Exception);
  ASSERT_THROW(s.getGlobalMetainfoAs<bool>("bool-not-present"), Exception);

  EXPECT_EQ(s.getGlobalMetainfoAs<int>("int32"), int(32));
  EXPECT_EQ(s.getGlobalMetainfoAs<std::int64_t>("int64"), std::int64_t(64));
  EXPECT_EQ(s.getGlobalMetainfoAs<float>("float32"), float(32.0f));
  EXPECT_EQ(s.getGlobalMetainfoAs<double>("float64"), double(64.0f));
  EXPECT_EQ(s.getGlobalMetainfoAs<std::string>("string"), std::string("string"));
}

TEST_F(SerializerImplTest, RegisterSavepoints) {
  SerializerImpl s(OpenModeKind::Write, directory->path().string(), "BinaryArchive");
  
  
//  MetaInfoMap metaInfo1(std::initializer_list<MetaInfoMap::value_type>{
//      {"key1", MetaInfoValue(std::string("Savepoint1_key1_str"))},
//      {"key2", MetaInfoValue(double(1))}});
//  savepoints->emplace_back("Savepoint1", metaInfo1,
//                           std::vector<FieldID>{{"field1", 0}, {"field2", 0}});
}

TEST_F(SerializerImplTest, RegisterFields) {
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
  ASSERT_TRUE(s.fieldMap().hasField("field1"));
  EXPECT_EQ(s.fieldMap().getTypeOf("field1"), TypeID::Float64);
  EXPECT_EQ(s.getFieldMetaInfoOf("field1").metaInfo().at("key1").as<std::string>(),
            "field1_key1_str");
  EXPECT_EQ(s.getFieldMetaInfoOf("field1").metaInfo().at("key2").as<double>(), 3);

  // Check field2
  ASSERT_TRUE(s.fieldMap().hasField("field2"));
  EXPECT_EQ(s.fieldMap().getTypeOf("field2"), TypeID::Float32);
  EXPECT_EQ(s.getFieldMetaInfoOf("field2").metaInfo().at("key1").as<std::string>(),
            "field2_key1_str");
  EXPECT_EQ(s.getFieldMetaInfoOf("field2").metaInfo().at("key2").as<double>(), 4);
}

TEST_F(SerializerImplTest, JSONSerialization) {
  // Serialize meta-data as JSON to disk
  std::unique_ptr<Archive> archive(new BinaryArchive(directory->path(), OpenModeKind::Write));
  SerializerImpl s_wirte(OpenModeKind::Write, directory->path().string(), *savepoints, *fieldMap,
                         *globalMetaInfo, archive);
  ASSERT_NO_THROW(s_wirte.updateMetaData());

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
  EXPECT_EQ(s_read.globalMetaInfo().at("string").as<std::string>(), "string");

  // Savepoints
  ASSERT_EQ(s_read.savepoints().size(), 2);

  const SavepointImpl& sp1 = s_read.savepoints()[0];
  EXPECT_EQ(sp1.name(), "Savepoint1");

  ASSERT_TRUE(sp1.hasField("field1"));
  EXPECT_EQ(sp1.getFieldID("field1").id, 0);

  ASSERT_TRUE(sp1.hasField("field2"));
  EXPECT_EQ(sp1.getFieldID("field2").id, 0);

  EXPECT_EQ(sp1.metaInfo().at("key1").as<std::string>(), "Savepoint1_key1_str");
  EXPECT_EQ(sp1.metaInfo().at("key2").as<double>(), 1);

  const SavepointImpl& sp2 = s_read.savepoints()[1];
  EXPECT_EQ(sp2.name(), "Savepoint2");

  ASSERT_TRUE(sp2.hasField("field1"));
  EXPECT_EQ(sp2.getFieldID("field1").id, 1);

  ASSERT_TRUE(sp2.hasField("field2"));
  EXPECT_EQ(sp2.getFieldID("field2").id, 1);

  EXPECT_EQ(sp2.metaInfo().at("key1").as<std::string>(), "Savepoint2_key1_str");
  EXPECT_EQ(sp2.metaInfo().at("key2").as<double>(), 2);

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

TEST_F(SerializerImplTest, toString) {}
