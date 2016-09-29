//===-- Unittest/Cpp/UnittestBinaryArchive.cpp --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the Binary Archive.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/FileUtility.h"
#include "Utility/Storage.h"
#include "serialbox/Core/Archive/BinaryArchive.h"
#include "serialbox/Core/Version.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

namespace {

template <class T>
class BinaryArchiveTest : public testing::Test {
public:
  using value_type = T;
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

TYPED_TEST_CASE(BinaryArchiveTest, TestTypes);

TYPED_TEST(BinaryArchiveTest, Construction) {

  // -----------------------------------------------------------------------------------------------
  // Writing
  // -----------------------------------------------------------------------------------------------

  // Open fresh archive and write meta data to disk
  {
    BinaryArchive b(OpenModeKind::Write, this->directory->path().string(), "field");
    b.updateMetaData();

    EXPECT_EQ(b.name(), "BinaryArchive");
    EXPECT_EQ(b.mode(), OpenModeKind::Write);
    EXPECT_EQ(b.prefix(), "field");
  }

  // Create directory if not already existent
  {
    BinaryArchive b(OpenModeKind::Write, (this->directory->path() / "this-dir-is-created").string(),
                    "field");
    EXPECT_TRUE(boost::filesystem::exists(this->directory->path() / "this-dir-is-created"));
  }

  // -----------------------------------------------------------------------------------------------
  // Reading
  // -----------------------------------------------------------------------------------------------
  {
    BinaryArchive b(OpenModeKind::Read, this->directory->path().string(), "field");
    b.updateMetaData();
  }

  // Throw Exception: Directory does not exist
  {
    EXPECT_THROW(BinaryArchive(OpenModeKind::Read, (this->directory->path() / "not-a-dir").string(),
                               "field"),
                 Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Appending
  // -----------------------------------------------------------------------------------------------

  {
    EXPECT_NO_THROW(BinaryArchive(OpenModeKind::Append, this->directory->path().string(), "field"));
  }

  // Create directory if not already existent
  {
    BinaryArchive b(OpenModeKind::Append,
                    (this->directory->path() / "this-dir-is-created").string(), "field");
  }

  // Create directories if not already existent
  {
    BinaryArchive b(OpenModeKind::Append, (this->directory->path() / "nest1" / "nest2").string(),
                    "field");
    EXPECT_TRUE(boost::filesystem::exists(this->directory->path() / "nest1" / "nest2"));
  }
}

TYPED_TEST(BinaryArchiveTest, WriteAndRead) {

  // -----------------------------------------------------------------------------------------------
  // Preparation
  // -----------------------------------------------------------------------------------------------
  using Storage = Storage<TypeParam>;

  // Prepare input data
  Storage u_0_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);
  Storage u_1_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);
  Storage u_2_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);

  Storage v_0_input(Storage::ColMajor, {5, 1, 1}, Storage::random);
  Storage v_1_input(Storage::ColMajor, {5, 1, 1}, Storage::random);
  Storage v_2_input(Storage::ColMajor, {5, 1, 1}, Storage::random);

  Storage storage_2d_0_input(Storage::ColMajor, {26, 23}, {{2, 2}, {4, 2}}, Storage::random);
  Storage storage_2d_1_input(Storage::ColMajor, {26, 23}, {{2, 2}, {4, 2}}, Storage::random);

  Storage storage_7d_0_input(Storage::ColMajor, {2, 2, 2, 2, 2, 2, 2}, Storage::random);
  Storage storage_7d_1_input(Storage::ColMajor, {2, 2, 2, 2, 2, 2, 2}, Storage::random);

  // Prepare output
  Storage u_0_output(Storage::RowMajor, {5, 6, 7});
  Storage u_1_output(Storage::RowMajor, {5, 6, 7});
  Storage u_2_output(Storage::RowMajor, {5, 6, 7});

  Storage v_0_output(Storage::RowMajor, {5, 1, 1});
  Storage v_1_output(Storage::RowMajor, {5, 1, 1});
  Storage v_2_output(Storage::RowMajor, {5, 1, 1});

  Storage storage_2d_0_output(Storage::ColMajor, {26, 23}, {{2, 2}, {4, 2}});
  Storage storage_2d_1_output(Storage::ColMajor, {26, 23}, {{2, 2}, {4, 2}});

  Storage storage_7d_0_output(Storage::ColMajor, {2, 2, 2, 2, 2, 2, 2});
  Storage storage_7d_1_output(Storage::ColMajor, {2, 2, 2, 2, 2, 2, 2});

  // -----------------------------------------------------------------------------------------------
  // Writing (data and meta-data)
  // -----------------------------------------------------------------------------------------------
  {
    BinaryArchive archiveWrite(OpenModeKind::Write, this->directory->path().string(), "field");
    EXPECT_STREQ(archiveWrite.directory().c_str(), this->directory->path().string().c_str());

    // u: id = 0
    {
      auto sv = u_0_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "u");
      ASSERT_EQ(fieldID.name, "u");
      ASSERT_EQ(fieldID.id, 0);
    }

    // u:  id = 1
    {
      auto sv = u_1_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "u");
      ASSERT_EQ(fieldID.name, "u");
      ASSERT_EQ(fieldID.id, 1);
    }

    // u:  id = 2
    {
      auto sv = u_2_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "u");
      ASSERT_EQ(fieldID.name, "u");
      ASSERT_EQ(fieldID.id, 2);
    }

    // v:  id = 0
    {
      auto sv = v_0_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "v");
      ASSERT_EQ(fieldID.name, "v");
      ASSERT_EQ(fieldID.id, 0);
    }

    // v:  id = 1
    {
      auto sv = v_1_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "v");
      ASSERT_EQ(fieldID.name, "v");
      ASSERT_EQ(fieldID.id, 1);
    }

    // v:  id = 2
    {
      auto sv = v_2_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "v");
      ASSERT_EQ(fieldID.name, "v");
      ASSERT_EQ(fieldID.id, 2);
    }

    // Try to write already existing data
    auto sv_v_1 = v_1_input.toStorageView();
    ASSERT_EQ(archiveWrite.write(sv_v_1, "v"), (FieldID{"v", 1}));

    // storage 2d
    auto sv_2d_0_input = storage_2d_0_input.toStorageView();
    archiveWrite.write(sv_2d_0_input, "storage_2d");

    auto sv_2d_1_input = storage_2d_1_input.toStorageView();
    archiveWrite.write(sv_2d_1_input, "storage_2d");

    // storage 7d
    auto sv_7d_0_input = storage_7d_0_input.toStorageView();
    archiveWrite.write(sv_7d_0_input, "storage_7d");

    auto sv_7d_1_input = storage_7d_1_input.toStorageView();
    archiveWrite.write(sv_7d_1_input, "storage_7d");

    // Check all exceptional cases
    auto sv_u_2_input = u_2_input.toStorageView();
    ASSERT_THROW(archiveWrite.read(sv_u_2_input, FieldID{"u", 2}), Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Reading
  // -----------------------------------------------------------------------------------------------
  {
    BinaryArchive archiveRead(OpenModeKind::Read, this->directory->path().string(), "field");
    EXPECT_STREQ(archiveRead.directory().c_str(), this->directory->path().string().c_str());
    archiveRead.updateMetaData(); // should do nothing

    // u
    auto sv_u_0_output = u_0_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_u_0_output, FieldID{"u", 0}));

    auto sv_u_1_output = u_1_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_u_1_output, FieldID{"u", 1}));

    auto sv_u_2_output = u_2_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_u_2_output, FieldID{"u", 2}));

    // v
    auto sv_v_0_output = v_0_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_v_0_output, FieldID{"v", 0}));

    auto sv_v_1_output = v_1_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_v_1_output, FieldID{"v", 1}));

    auto sv_v_2_output = v_2_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_v_2_output, FieldID{"v", 2}));

    // Check all exceptional cases
    ASSERT_THROW(archiveRead.write(sv_u_2_output, "u"), Exception);
    ASSERT_THROW(archiveRead.read(sv_u_2_output, FieldID{"u", 1024}), Exception);
    ASSERT_THROW(archiveRead.read(sv_u_2_output, FieldID{"not-a-field", 0}), Exception);

    // storage 2d
    auto sv_2d_0_output = storage_2d_0_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_2d_0_output, FieldID{"storage_2d", 0}));

    auto sv_2d_1_output = storage_2d_1_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_2d_1_output, FieldID{"storage_2d", 1}));

    // storage 7d
    auto sv_7d_0_output = storage_7d_0_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_7d_0_output, FieldID{"storage_7d", 0}));

    auto sv_7d_1_output = storage_7d_1_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_7d_1_output, FieldID{"storage_7d", 1}));
  }

  // -----------------------------------------------------------------------------------------------
  // Validation
  // -----------------------------------------------------------------------------------------------
  ASSERT_TRUE(Storage::verify(u_0_output, u_0_input));
  ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));
  ASSERT_TRUE(Storage::verify(u_2_output, u_2_input));

  ASSERT_TRUE(Storage::verify(v_0_output, v_0_input));
  ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));
  ASSERT_TRUE(Storage::verify(v_2_output, v_2_input));

  ASSERT_TRUE(Storage::verify(storage_2d_0_output, storage_2d_0_input));
  ASSERT_TRUE(Storage::verify(storage_2d_1_output, storage_2d_1_input));

  ASSERT_TRUE(Storage::verify(storage_7d_0_output, storage_7d_0_input));
  ASSERT_TRUE(Storage::verify(storage_7d_1_output, storage_7d_1_input));

  // -----------------------------------------------------------------------------------------------
  // Cleanup
  // -----------------------------------------------------------------------------------------------
  {
    BinaryArchive archiveWrite(OpenModeKind::Write, this->directory->path().string(), "field");
    EXPECT_FALSE(boost::filesystem::exists(this->directory->path() / ("field_u.dat")));
    EXPECT_FALSE(boost::filesystem::exists(this->directory->path() / ("field_v.dat")));
    EXPECT_FALSE(boost::filesystem::exists(this->directory->path() / ("field_storage_2d.dat")));
    EXPECT_FALSE(boost::filesystem::exists(this->directory->path() / ("field_storage_7d.dat")));
  }
}

TYPED_TEST(BinaryArchiveTest, MetaData) {
  using Storage = Storage<TypeParam>;

  Storage u_0_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);
  Storage u_0_output(Storage::RowMajor, {5, 6, 7});

  BinaryArchive archiveWrite(OpenModeKind::Write, this->directory->path().string(), "field");

  auto sv_u_0_input = u_0_input.toStorageView();
  archiveWrite.write(sv_u_0_input, "u");
  archiveWrite.updateMetaData();

  // Read meta data file to get in memory copy
  std::ifstream ifs(archiveWrite.metaDataFile());
  json::json j;
  ifs >> j;
  ifs.close();

  // Write meta file to disk (to corrupt it)
  std::string filename = archiveWrite.metaDataFile();
  auto toFile = [this, &filename](const json::json& jsonNode) -> void {
    std::ofstream ofs(filename, std::ios::out | std::ios::trunc);
    ofs << jsonNode.dump(4);
  };

  // -----------------------------------------------------------------------------------------------
  // Invalid hash
  // -----------------------------------------------------------------------------------------------
  {
    json::json corrupted = j;
    corrupted["fields_table"]["u"][0][1] = "LOOKS_LIKE_THIS_HASH_IS_CORRUPTED";
    toFile(corrupted);

    BinaryArchive archiveRead(OpenModeKind::Read, this->directory->path().string(), "field");

    // The data of u_0_output should NOT be modified
    auto sv = u_0_output.toStorageView();
    ASSERT_THROW(archiveRead.read(sv, FieldID{"u", 0}), Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Invlaid serialbox version
  // -----------------------------------------------------------------------------------------------
  {
    json::json corrupted = j;
    corrupted["serialbox_version"] = 100 * (SERIALBOX_VERSION_MAJOR - 1) +
                                     10 * SERIALBOX_VERSION_MINOR + SERIALBOX_VERSION_PATCH;
    toFile(corrupted);

    ASSERT_THROW(BinaryArchive(OpenModeKind::Read, this->directory->path().string(), "field"),
                 Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Not a binary archive
  // -----------------------------------------------------------------------------------------------
  {
    json::json corrupted = j;
    corrupted["archive_name"] = "not-BinaryArchive";
    toFile(corrupted);

    ASSERT_THROW(BinaryArchive(OpenModeKind::Read, this->directory->path().string(), "field"),
                 Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Invalid binary archive version
  // -----------------------------------------------------------------------------------------------
  {
    json::json corrupted = j;
    corrupted["archive_version"] = -1;
    toFile(corrupted);

    ASSERT_THROW(BinaryArchive(OpenModeKind::Read, this->directory->path().string(), "field"),
                 Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // MetaData not found
  // -----------------------------------------------------------------------------------------------
  {
    boost::filesystem::remove(filename);
    ASSERT_THROW(BinaryArchive(OpenModeKind::Read, this->directory->path().string(), "field"),
                 Exception);
  }
}

TYPED_TEST(BinaryArchiveTest, toString) {
  std::stringstream ss;
  BinaryArchive archive(OpenModeKind::Write, this->directory->path().string(), "field");

  ss << archive;
  EXPECT_TRUE(boost::algorithm::starts_with(ss.str(), "BinaryArchive"));
  EXPECT_NE(ss.str().find("directory"), std::string::npos);
  EXPECT_NE(ss.str().find("mode"), std::string::npos);
  EXPECT_NE(ss.str().find("prefix"), std::string::npos);
  EXPECT_NE(ss.str().find("fieldsTable"), std::string::npos);
}
