//===-- Unittest/UnittestBinaryArchive.cpp ------------------------------------------*- C++ -*-===//
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

#include "FileUtility.h"
#include "Storage.h"
#include "serialbox/Core/Archive/BinaryArchive.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

namespace {

template<class T>
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
  // Try to open archives for writing. This will also create the ArchiveMetaData.json when invoking
  // the destrutor as we set the meta-data to be dirty.
  {
    EXPECT_NO_THROW(BinaryArchive(this->directory->path(), OpenModeKind::Write).setMetaDataDirty());
  }
  EXPECT_NO_THROW(
      BinaryArchive(this->directory->path() / "this-dir-is-created", OpenModeKind::Write));
  EXPECT_TRUE(boost::filesystem::exists(this->directory->path() / "this-dir-is-created"));

  // Reading
  EXPECT_NO_THROW(BinaryArchive(this->directory->path(), OpenModeKind::Read));
  EXPECT_THROW(BinaryArchive(this->directory->path() / "not-a-dir", OpenModeKind::Read), Exception);

  // Appending
  EXPECT_NO_THROW(BinaryArchive(this->directory->path(), OpenModeKind::Append));
  EXPECT_NO_THROW(
      BinaryArchive(this->directory->path() / "this-dir-is-created", OpenModeKind::Append));
  EXPECT_NO_THROW(
      BinaryArchive(this->directory->path() / "this-dir-is-created-2", OpenModeKind::Append));
  EXPECT_TRUE(boost::filesystem::exists(this->directory->path() / "this-dir-is-created-2"));
}

TYPED_TEST(BinaryArchiveTest, WriteAndRead) {

  // -------------------------------------------------------------------------------------------------
  // Preparation
  // -------------------------------------------------------------------------------------------------
  using Storage = Storage<TypeParam>;
  
  // Prepare input data
  Storage u_0_input(Storage::RowMajor, {5, 6, 7}, Storage::random);
  Storage u_1_input(Storage::RowMajor, {5, 6, 7}, Storage::random);
  Storage u_2_input(Storage::RowMajor, {5, 6, 7}, Storage::random);

  Storage v_0_input(Storage::ColMajor, {5, 1, 1}, Storage::random);
  Storage v_1_input(Storage::ColMajor, {5, 1, 1}, Storage::random);
  Storage v_2_input(Storage::ColMajor, {5, 1, 1}, Storage::random);
  
  // Prepare output
  Storage u_0_output(Storage::RowMajor, {5, 6, 7});
  Storage u_1_output(Storage::RowMajor, {5, 6, 7});
  Storage u_2_output(Storage::RowMajor, {5, 6, 7});

  Storage v_0_output(Storage::RowMajor, {5, 1, 1});
  Storage v_1_output(Storage::RowMajor, {5, 1, 1});
  Storage v_2_output(Storage::RowMajor, {5, 1, 1});

  // -----------------------------------------------------------------------------------------------
  // Writing
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
    archiveRead.read(sv_v_0_output, FieldID{"v", 0});

    auto sv_v_1_output = v_1_output.toStorageView();
    archiveRead.read(sv_v_1_output, FieldID{"v", 1});

    auto sv_v_2_output = v_2_output.toStorageView();
    ASSERT_NO_THROW(archiveRead.read(sv_v_2_output, FieldID{"v", 2}));

    // Check all exceptional cases
    ASSERT_THROW(archiveRead.write(sv_u_2_output, FieldID{"u", 2}), Exception);
  }

  {
    // Corrupt the JSON file
    std::ifstream ifs((this->directory->path() / Archive::ArchiveName).string());
    json::json j;
    ifs >> j;
    j["fields_table"]["u"][0][1] = "LOOKS_LIKE_THIS_HASH_IS_CORRUPTED";
    ifs.close();
    std::ofstream ofs((this->directory->path() / Archive::ArchiveName).string(), std::ios::trunc);
    ofs << j.dump(4) << std::endl;

    BinaryArchive archiveRead(this->directory->path().string(), OpenModeKind::Read);

    // The data of u_0_output should NOT be modified
    auto sv = u_0_output.toStorageView();
    ASSERT_THROW(archiveRead.read(sv, FieldID{"u", 0}), Exception);
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
}
