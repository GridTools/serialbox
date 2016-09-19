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
  {
    // Open fresh archive and write meta data to disk
    {
      EXPECT_NO_THROW(
          BinaryArchive(this->directory->path(), OpenModeKind::Write).forceUpdateMetaData(););
    }

    // Throw Exception: Directory is not empty
    EXPECT_THROW(BinaryArchive(this->directory->path(), OpenModeKind::Write), Exception);

    // Create directory if not already existent
    EXPECT_NO_THROW(
        BinaryArchive(this->directory->path() / "this-dir-is-created", OpenModeKind::Write));
    EXPECT_TRUE(boost::filesystem::exists(this->directory->path() / "this-dir-is-created"));
  }

  // -----------------------------------------------------------------------------------------------
  // Reading
  // -----------------------------------------------------------------------------------------------
  {
    EXPECT_NO_THROW(BinaryArchive(this->directory->path(), OpenModeKind::Read));

    // Throw Exception: Directory does not exist
    EXPECT_THROW(BinaryArchive(this->directory->path() / "not-a-dir", OpenModeKind::Read),
                 Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Appending
  // -----------------------------------------------------------------------------------------------
  EXPECT_NO_THROW(BinaryArchive(this->directory->path(), OpenModeKind::Append));

  // Create directory if not already existent
  EXPECT_NO_THROW(
      BinaryArchive(this->directory->path() / "this-dir-is-created", OpenModeKind::Append));

  // Create directories if not already existent
  EXPECT_NO_THROW(BinaryArchive(this->directory->path() / "nest1" / "nest2", OpenModeKind::Append));
  EXPECT_TRUE(boost::filesystem::exists(this->directory->path() / "nest1" / "nest2"));

  // Name
  EXPECT_EQ(BinaryArchive(this->directory->path(), OpenModeKind::Append).name(), "BinaryArchive");
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
    BinaryArchive archiveWrite(this->directory->path().string(), OpenModeKind::Write);
    EXPECT_STREQ(archiveWrite.directory().c_str(), this->directory->path().string().c_str());

    // u
    auto sv_u_0_input = u_0_input.toStorageView();
    archiveWrite.write(sv_u_0_input, FieldID{"u", 0});

    auto sv_u_1_input = u_1_input.toStorageView();
    archiveWrite.write(sv_u_1_input, FieldID{"u", 1});

    auto sv_u_2_input = u_2_input.toStorageView();
    archiveWrite.write(sv_u_2_input, FieldID{"u", 2});

    // v
    auto sv_v_0_input = v_0_input.toStorageView();
    archiveWrite.write(sv_v_0_input, FieldID{"v", 0});
    archiveWrite.write(sv_v_0_input, FieldID{"v", 0});

    auto sv_v_1_input = v_1_input.toStorageView();
    archiveWrite.write(sv_v_1_input, FieldID{"v", 1});

    auto sv_v_2_input = v_2_input.toStorageView();
    archiveWrite.write(sv_v_2_input, FieldID{"v", 2});

    // Replace data at v_1 with v_0
    archiveWrite.write(sv_v_0_input, FieldID{"v", 1});

    // Replace data at v_0 with v_1
    archiveWrite.write(sv_v_1_input, FieldID{"v", 0});

    // storage 2d
    auto sv_2d_0_input = storage_2d_0_input.toStorageView();
    archiveWrite.write(sv_2d_0_input, FieldID{"storage_2d", 0});

    auto sv_2d_1_input = storage_2d_1_input.toStorageView();
    archiveWrite.write(sv_2d_1_input, FieldID{"storage_2d", 1});

    // storage 7d
    auto sv_7d_0_input = storage_7d_0_input.toStorageView();
    archiveWrite.write(sv_7d_0_input, FieldID{"storage_7d", 0});

    auto sv_7d_1_input = storage_7d_1_input.toStorageView();
    archiveWrite.write(sv_7d_1_input, FieldID{"storage_7d", 1});

    // Check all exceptional cases
    ASSERT_THROW(archiveWrite.read(sv_u_2_input, FieldID{"u", 2}), Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Reading
  // -----------------------------------------------------------------------------------------------
  {
    BinaryArchive archiveRead(this->directory->path().string(), OpenModeKind::Read);
    EXPECT_STREQ(archiveRead.directory().c_str(), this->directory->path().string().c_str());

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
    ASSERT_THROW(archiveRead.write(sv_u_2_output, FieldID{"u", 2}), Exception);
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
  Storage::verify(u_0_output, u_0_input);
  Storage::verify(u_1_output, u_1_input);
  Storage::verify(u_2_output, u_2_input);

  Storage::verify(v_0_output, v_1_input); // Data was replaced
  Storage::verify(v_1_output, v_0_input); // Data was replaced
  Storage::verify(v_2_output, v_2_input);

  Storage::verify(storage_2d_0_output, storage_2d_0_input);
  Storage::verify(storage_2d_1_output, storage_2d_1_input);

  Storage::verify(storage_7d_0_output, storage_7d_0_input);
  Storage::verify(storage_7d_1_output, storage_7d_1_input);
}

TYPED_TEST(BinaryArchiveTest, MetaData)
{
  using Storage = Storage<TypeParam>;
  
  Storage u_0_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);
  Storage u_0_output(Storage::RowMajor, {5, 6, 7});

  BinaryArchive archiveWrite(this->directory->path().string(), OpenModeKind::Write);
  
  auto sv_u_0_input = u_0_input.toStorageView();
  archiveWrite.write(sv_u_0_input, FieldID{"u", 0});
  archiveWrite.updateMetaData();
  
  // Read meta data file to get in memory copy
  std::ifstream ifs((this->directory->path() / Archive::ArchiveMetaDataFile).string());
  json::json j;
  ifs >> j;
  ifs.close();
  
  // Write meta file to disk (to corrupt it)
  auto toFile = [this](const json::json& jsonNode) -> void {
    std::ofstream ofs((this->directory->path() / Archive::ArchiveMetaDataFile).string(),
                      std::ios::out | std::ios::trunc);
    ofs << jsonNode.dump(4);
  };

  // -----------------------------------------------------------------------------------------------
  // Invalid hash
  // -----------------------------------------------------------------------------------------------
  {
    json::json corrupted = j;
    corrupted["fields_table"]["u"][0][1] = "LOOKS_LIKE_THIS_HASH_IS_CORRUPTED";
    toFile(corrupted);
    
    BinaryArchive archiveRead(this->directory->path().string(), OpenModeKind::Read);

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

    ASSERT_THROW(BinaryArchive(this->directory->path().string(), OpenModeKind::Read), Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Not a binary archive
  // -----------------------------------------------------------------------------------------------
  {
    json::json corrupted = j;
    corrupted["archive_name"] = "not-BinaryArchive";
    toFile(corrupted);

    ASSERT_THROW(BinaryArchive(this->directory->path().string(), OpenModeKind::Read), Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // Invalid binary archive version
  // -----------------------------------------------------------------------------------------------
  {
    json::json corrupted = j;
    corrupted["archive_version"] = -1;
    toFile(corrupted);

    ASSERT_THROW(BinaryArchive(this->directory->path().string(), OpenModeKind::Read), Exception);
  }

  // -----------------------------------------------------------------------------------------------
  // MetaData not found
  // -----------------------------------------------------------------------------------------------
  {
    boost::filesystem::remove(this->directory->path() / Archive::ArchiveMetaDataFile);
    ASSERT_THROW(BinaryArchive(this->directory->path().string(), OpenModeKind::Read), Exception);
  }
}

TYPED_TEST(BinaryArchiveTest, toString) {
  std::stringstream ss;
  BinaryArchive archive(this->directory->path(), OpenModeKind::Write);

  ss << archive;
  EXPECT_TRUE(boost::algorithm::starts_with(ss.str(), "BinaryArchive"));
  EXPECT_NE(ss.str().find("directory"), std::string::npos);
  EXPECT_NE(ss.str().find("mode"), std::string::npos);
  EXPECT_NE(ss.str().find("fieldsTable"), std::string::npos);
}
