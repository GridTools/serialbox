//===-- serialbox/Core/Archive/UnittestBinaryArchive.cpp ----------------------------*- C++ -*-===//
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

#include "Utility/SerializerTestBase.h"
#include "Utility/Storage.h"
#include "serialbox/Core/Archive/BinaryArchive.h"
#include "serialbox/Core/Version.h"
#include <boost/algorithm/string.hpp>
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

//===------------------------------------------------------------------------------------------===//
//     Utility tests
//===------------------------------------------------------------------------------------------===//

namespace {

class BinaryArchiveUtilityTest : public SerializerUnittestBase {};

} // anonymous namespace

TEST_F(BinaryArchiveUtilityTest, Construction) {

  // -----------------------------------------------------------------------------------------------
  // Writing
  // -----------------------------------------------------------------------------------------------

  // Open fresh archive and write meta data to disk
  {
    BinaryArchive b(OpenModeKind::Write, this->directory->path().string(), "field");
    b.updateMetaData();

    EXPECT_EQ(b.name(), "Binary");
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

TEST_F(BinaryArchiveUtilityTest, MetaData) {
  using Storage = Storage<double>;

  Storage u_0_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);

  BinaryArchive archiveWrite(OpenModeKind::Write, this->directory->path().string(), "field");

  auto sv_u_0_input = u_0_input.toStorageView();
  archiveWrite.write(sv_u_0_input, "u", nullptr);
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

TEST_F(BinaryArchiveUtilityTest, toString) {
  using Storage = Storage<double>;
  std::stringstream ss;

  Storage storage(Storage::ColMajor, {5, 1, 1});

  BinaryArchive archive(OpenModeKind::Write, this->directory->path().string(), "field");
  StorageView sv = storage.toStorageView();
  archive.write(sv, "storage", nullptr);

  ss << archive;
  EXPECT_TRUE(boost::algorithm::starts_with(ss.str(), "BinaryArchive"));
  EXPECT_NE(ss.str().find("directory"), std::string::npos);
  EXPECT_NE(ss.str().find("mode"), std::string::npos);
  EXPECT_NE(ss.str().find("prefix"), std::string::npos);
  EXPECT_NE(ss.str().find("fieldsTable"), std::string::npos);
}

TEST_F(BinaryArchiveUtilityTest, writeAndRead) {
  using Storage = Storage<double>;
  Storage storage_input(Storage::ColMajor, {5, 2, 5}, Storage::random);
  Storage storage_output(Storage::ColMajor, {5, 2, 5});

  auto sv_input = storage_input.toStorageView();
  auto sv_output = storage_output.toStorageView();
  
  // Write and read from file
  BinaryArchive::writeToFile((this->directory->path() / "test.dat").string(), sv_input);

  BinaryArchive::readFromFile((this->directory->path() / "test.dat").string(), sv_output);

  ASSERT_TRUE(Storage::verify(storage_input, storage_output));

  // Read from non-existing file -> Exception
  ASSERT_THROW(BinaryArchive::readFromFile((this->directory->path() / "X.dat").string(), sv_output),
               Exception);
}


//===------------------------------------------------------------------------------------------===//
//     Read/Write tests
//===------------------------------------------------------------------------------------------===//

namespace {

template <class T>
class BinaryArchiveReadWriteTest : public SerializerUnittestBase {};

using TestTypes = testing::Types<double, float, int, std::int64_t>;

} // anonymous namespace

TYPED_TEST_CASE(BinaryArchiveReadWriteTest, TestTypes);

TYPED_TEST(BinaryArchiveReadWriteTest, WriteAndRead) {

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
      FieldID fieldID = archiveWrite.write(sv, "u", nullptr);
      ASSERT_EQ(fieldID.name, "u");
      ASSERT_EQ(fieldID.id, 0);
    }

    // u:  id = 1
    {
      auto sv = u_1_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "u", nullptr);
      ASSERT_EQ(fieldID.name, "u");
      ASSERT_EQ(fieldID.id, 1);
    }

    // u:  id = 2
    {
      auto sv = u_2_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "u", nullptr);
      ASSERT_EQ(fieldID.name, "u");
      ASSERT_EQ(fieldID.id, 2);
    }

    // v:  id = 0
    {
      auto sv = v_0_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "v", nullptr);
      ASSERT_EQ(fieldID.name, "v");
      ASSERT_EQ(fieldID.id, 0);
    }

    // v:  id = 1
    {
      auto sv = v_1_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "v", nullptr);
      ASSERT_EQ(fieldID.name, "v");
      ASSERT_EQ(fieldID.id, 1);
    }

    // v:  id = 2
    {
      auto sv = v_2_input.toStorageView();
      FieldID fieldID = archiveWrite.write(sv, "v", nullptr);
      ASSERT_EQ(fieldID.name, "v");
      ASSERT_EQ(fieldID.id, 2);
    }

    // Try to write already existing data
    auto sv_v_1 = v_1_input.toStorageView();
    ASSERT_EQ(archiveWrite.write(sv_v_1, "v", nullptr), (FieldID{"v", 1}));

    // storage 2d
    auto sv_2d_0_input = storage_2d_0_input.toStorageView();
    archiveWrite.write(sv_2d_0_input, "storage_2d", nullptr);

    auto sv_2d_1_input = storage_2d_1_input.toStorageView();
    archiveWrite.write(sv_2d_1_input, "storage_2d", nullptr);

    // storage 7d
    auto sv_7d_0_input = storage_7d_0_input.toStorageView();
    archiveWrite.write(sv_7d_0_input, "storage_7d", nullptr);

    auto sv_7d_1_input = storage_7d_1_input.toStorageView();
    archiveWrite.write(sv_7d_1_input, "storage_7d", nullptr);

    // Check all exceptional cases
    auto sv_u_2_input = u_2_input.toStorageView();
    ASSERT_THROW(archiveWrite.read(sv_u_2_input, FieldID{"u", 2}, nullptr), Exception);
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
    ASSERT_NO_THROW(archiveRead.read(sv_u_0_output, FieldID{"u", 0}, nullptr));

    auto sv_u_1_output = u_1_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_u_1_output, FieldID{"u", 1}, nullptr));

    auto sv_u_2_output = u_2_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_u_2_output, FieldID{"u", 2}, nullptr));

    // v
    auto sv_v_0_output = v_0_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_v_0_output, FieldID{"v", 0}, nullptr));

    auto sv_v_1_output = v_1_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_v_1_output, FieldID{"v", 1}, nullptr));

    auto sv_v_2_output = v_2_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_v_2_output, FieldID{"v", 2}, nullptr));

    // Check all exceptional cases
    ASSERT_THROW(archiveRead.write(sv_u_2_output, "u", nullptr), Exception);
    ASSERT_THROW(archiveRead.read(sv_u_2_output, FieldID{"u", 1024}, nullptr), Exception);
    ASSERT_THROW(archiveRead.read(sv_u_2_output, FieldID{"not-a-field", 0}, nullptr), Exception);

    // storage 2d
    auto sv_2d_0_output = storage_2d_0_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_2d_0_output, FieldID{"storage_2d", 0}, nullptr));

    auto sv_2d_1_output = storage_2d_1_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_2d_1_output, FieldID{"storage_2d", 1}, nullptr));

    // storage 7d
    auto sv_7d_0_output = storage_7d_0_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_7d_0_output, FieldID{"storage_7d", 0}, nullptr));

    auto sv_7d_1_output = storage_7d_1_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_7d_1_output, FieldID{"storage_7d", 1}, nullptr));
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
