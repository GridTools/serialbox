//===-- Unittest/Cpp/UpgradeArchive.cpp ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Unittest of the upgrade archive functionaility.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/FileUtility.h"
#include "Utility/Serialbox.h"
#include "Utility/Storage.h"
#include "serialbox/Core/SerializerImpl.h"
#include <gtest/gtest.h>

#if SERIALBOX_HAS_SERIALBOX_OLD

using namespace serialbox;
using namespace unittest;

namespace {

class UpgradeArchiveTest : public testing::Test {
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

} // anonymous namespace

TEST_F(UpgradeArchiveTest, upgrade) {

  // -----------------------------------------------------------------------------------------------
  // Preparation
  // -----------------------------------------------------------------------------------------------
  using Storage = Storage<double>;

  // Prepare input data
  Storage u_0_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);
  Storage u_1_input(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::random);

  Storage v_0_input(Storage::ColMajor, {5, 1, 1}, Storage::random);
  Storage v_1_input(Storage::ColMajor, {5, 1, 1}, Storage::random);

  // Prepare output
  Storage u_0_output(Storage::RowMajor, {5, 6, 7});
  Storage u_1_output(Storage::RowMajor, {5, 6, 7});

  Storage v_0_output(Storage::ColMajor, {5, 1, 1});
  Storage v_1_output(Storage::ColMajor, {5, 1, 1});

  // -----------------------------------------------------------------------------------------------
  // Write (with old serialbox)
  // -----------------------------------------------------------------------------------------------
  //
  //  Savepoint     | MetaData            | Fields
  //  ----------------------------------------------
  //  savepoint1    | time: 1, dt: 5.1    | u_0, v_0
  //  savepoint1    | time: 2, dt: 9.1    | u_1, v_1
  //  savepoint_u_1 | -                   | u_1
  //  savepoint_v_1 | -                   | v_1
  //
  {
    ser::Serializer ser_write;
    ser_write.Init(directory->path().string(), "UpgradeArchiveTest", ser::SerializerOpenModeWrite);

    // Add some global metainfo
    ser_write.AddMetainfo("Day", int(29));
    ser_write.AddMetainfo("Month", std::string("March"));
    ser_write.AddMetainfo("Year", double(2016.10));
    ser_write.AddMetainfo("boolean", true);

    // Add savepoints
    ser::Savepoint savepoint1_t_1;
    ser::Savepoint savepoint1_t_2;
    ser::Savepoint savepoint_u_1;
    ser::Savepoint savepoint_v_1;

    savepoint1_t_1.Init("savepoint1");
    savepoint1_t_1.AddMetainfo("time", int(1));
    savepoint1_t_1.AddMetainfo("dt", double(5.1));

    savepoint1_t_2.Init("savepoint1");
    savepoint1_t_2.AddMetainfo("time", int(2));
    savepoint1_t_2.AddMetainfo("dt", double(9.1));

    savepoint_u_1.Init("savepoint_u_1");
    savepoint_v_1.Init("savepoint_v_1");

    // Register fields
    ser_write.RegisterField("u", "double", sizeof(double), 5, 6, 7, 1, 0, 0, 0, 0, 0, 0, 0, 0);
    ser_write.RegisterField("v", "double", sizeof(double), 5, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0);

    // Add some field meta-info
    ser_write.AddFieldMetainfo("u", "Day", int(29));
    ser_write.AddFieldMetainfo("u", "Month", "March");
    ser_write.AddFieldMetainfo("u", "Year", 2016.10);
    ser_write.AddFieldMetainfo("v", "boolean", true);

    // Writing (implicitly register the savepoints)
    int sizeDouble = sizeof(double);

    ser_write.WriteField("u", savepoint1_t_1, (void*)u_0_input.originPtr(),
                         sizeDouble * u_0_input.strides()[0], sizeDouble * u_0_input.strides()[1],
                         sizeDouble * u_0_input.strides()[2], 0);
    ser_write.WriteField("v", savepoint1_t_1, (void*)v_0_input.originPtr(),
                         sizeDouble * v_0_input.strides()[0], sizeDouble * v_0_input.strides()[1],
                         sizeDouble * v_0_input.strides()[2], 0);
    ser_write.WriteField("u", savepoint1_t_2, (void*)u_1_input.originPtr(),
                         sizeDouble * u_1_input.strides()[0], sizeDouble * u_1_input.strides()[1],
                         sizeDouble * u_1_input.strides()[2], 0);
    ser_write.WriteField("v", savepoint1_t_2, (void*)v_1_input.originPtr(),
                         sizeDouble * v_1_input.strides()[0], sizeDouble * v_1_input.strides()[1],
                         sizeDouble * v_1_input.strides()[2], 0);
    ser_write.WriteField("u", savepoint_u_1, (void*)u_1_input.originPtr(),
                         sizeDouble * u_1_input.strides()[0], sizeDouble * u_1_input.strides()[1],
                         sizeDouble * u_1_input.strides()[2], 0);
    ser_write.WriteField("v", savepoint_v_1, (void*)v_1_input.originPtr(),
                         sizeDouble * v_1_input.strides()[0], sizeDouble * v_1_input.strides()[1],
                         sizeDouble * v_1_input.strides()[2], 0);
  }

  // -----------------------------------------------------------------------------------------------
  // Reading (with new serialbox)
  // -----------------------------------------------------------------------------------------------
  {
    // Implicitly upgrade the archive
    SerializerImpl ser_read(OpenModeKind::Read, directory->path().string(), "BinaryArchive",
                            "UpgradeArchiveTest");

    // Check metaInfo
    EXPECT_EQ(ser_read.getGlobalMetainfoAs<int>("Day"), 29);
    EXPECT_EQ(ser_read.getGlobalMetainfoAs<std::string>("Month"), "March");
    EXPECT_EQ(ser_read.getGlobalMetainfoAs<double>("Year"), 2016.10);
    EXPECT_EQ(ser_read.getGlobalMetainfoAs<bool>("boolean"), true);

    // Check FieldMap
    ASSERT_TRUE(ser_read.hasField("u"));
    ASSERT_TRUE(ser_read.hasField("v"));

    EXPECT_EQ(ser_read.getFieldMetaInfoOf("u").type(), TypeID::Float64);
    EXPECT_EQ(ser_read.getFieldMetaInfoOf("v").type(), TypeID::Float64);

    EXPECT_EQ(ser_read.getFieldMetaInfoOf("u").dims(), (std::vector<int>{5, 6, 7}));
    EXPECT_EQ(ser_read.getFieldMetaInfoOf("v").dims(), (std::vector<int>{5, 1, 1}));

    EXPECT_EQ(ser_read.getFieldMetaInfoOf("u").metaInfo().at("Day").as<int>(), 29);
    EXPECT_EQ(ser_read.getFieldMetaInfoOf("u").metaInfo().at("Month").as<std::string>(), "March");
    EXPECT_EQ(ser_read.getFieldMetaInfoOf("u").metaInfo().at("Year").as<double>(), 2016.10);
    EXPECT_EQ(ser_read.getFieldMetaInfoOf("v").metaInfo().at("boolean").as<bool>(), true);

    // Check order of savepoints is correct
    ASSERT_EQ(ser_read.savepoints().size(), 4);

    const Savepoint& savepoint1_t_1 = ser_read.savepoints()[0];
    const Savepoint& savepoint1_t_2 = ser_read.savepoints()[1];
    const Savepoint& savepoint_u_1 = ser_read.savepoints()[2];
    const Savepoint& savepoint_v_1 = ser_read.savepoints()[3];

    EXPECT_EQ(savepoint1_t_1.name(), "savepoint1");
    EXPECT_EQ(savepoint1_t_1.metaInfo().at("time").as<int>(), 1);
    EXPECT_EQ(savepoint1_t_1.metaInfo().at("dt").as<double>(), 5.1);

    EXPECT_EQ(savepoint1_t_2.name(), "savepoint1");
    EXPECT_EQ(savepoint1_t_2.metaInfo().at("time").as<int>(), 2);
    EXPECT_EQ(savepoint1_t_2.metaInfo().at("dt").as<double>(), 9.1);

    EXPECT_EQ(savepoint_u_1.name(), "savepoint_u_1");
    EXPECT_EQ(savepoint_v_1.name(), "savepoint_v_1");

    // StorageViews
    auto sv_u_0 = u_0_output.toStorageView();
    auto sv_u_1 = u_1_output.toStorageView();
    auto sv_v_0 = v_0_output.toStorageView();
    auto sv_v_1 = v_1_output.toStorageView();

    ser_read.read("u", savepoint1_t_1, sv_u_0);
    ASSERT_TRUE(Storage::verify(u_0_output, u_0_input));

    ser_read.read("v", savepoint1_t_1, sv_v_0);
    ASSERT_TRUE(Storage::verify(v_0_output, v_0_input));

    ser_read.read("u", savepoint1_t_2, sv_u_1);
    ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));

    ser_read.read("v", savepoint1_t_2, sv_v_1);
    ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));

    ser_read.read("u", savepoint_u_1, sv_u_1);
    ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));

    ser_read.read("v", savepoint_v_1, sv_v_1);
    ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));
  }

  // Old meta data is possibly not out-dated -> upgrade again
  {
    ASSERT_NO_THROW(SerializerImpl(OpenModeKind::Read, directory->path().string(), "BinaryArchive",
                                   "UpgradeArchiveTest"));
  }

  // Old meta data is outdated -> no upgrade
  {
    auto timeStampBeforeConstruction = boost::filesystem::last_write_time(
        directory->path() / SerializerImpl::SerializerMetaDataFile);

    // Set old-meta data to be out-dated
    boost::filesystem::last_write_time(directory->path() / "UpgradeArchiveTest.json",
                                       timeStampBeforeConstruction - 1);

    // Should perform no upgrade
    SerializerImpl ser_read(OpenModeKind::Read, directory->path().string(), "BinaryArchive",
                            "UpgradeArchiveTest");

    auto timeStampAfterConstruction = boost::filesystem::last_write_time(
        directory->path() / SerializerImpl::SerializerMetaDataFile);

    ASSERT_EQ(timeStampBeforeConstruction, timeStampAfterConstruction);
  }
}

#endif
