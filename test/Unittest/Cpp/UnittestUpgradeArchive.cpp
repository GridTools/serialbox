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

#include "Utility/Storage.h"
#include "Utility/FileUtility.h"
#include "Utility/Serialbox.h"
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

  Storage v_0_output(Storage::RowMajor, {5, 1, 1});
  Storage v_1_output(Storage::RowMajor, {5, 1, 1});
  
  // -----------------------------------------------------------------------------------------------
  // Write (with old serialbox)
  // -----------------------------------------------------------------------------------------------
  //
  //  Savepoint     | MetaData   | Fields
  //  -------------------------------------
  //  savepoint1    | time: 1    | u_0, v_0
  //  savepoint1    | time: 2    | u_1, v_1
  //  savepoint_u_1 | -          | u_1
  //  savepoint_v_1 | -          | v_1
  //
  ser::Serializer ser;
  ser.Init(directory->path().string(), "UpgradeArchiveTest", ser::SerializerOpenModeWrite);

  // Add some metainfo
  ser.AddMetainfo("Day", int(31));
  ser.AddMetainfo("Month", std::string("March"));
  ser.AddMetainfo("Year", double(2015.25));
  
  // Add savepoints  
  ser::Savepoint savepoint1_t_1;
  ser::Savepoint savepoint1_t_2;
  ser::Savepoint savepoint_u_1;
  ser::Savepoint savepoint_v_1;
  
  savepoint1_t_1.Init("savepoint1");
  savepoint1_t_1.AddMetainfo("time", int(1));
  savepoint1_t_2.Init("savepoint1");
  savepoint1_t_2.AddMetainfo("time", int(2));
  savepoint_u_1.Init("savepoint_u_1");
  savepoint_v_1.Init("savepoint_v_1");
  
  // Register fields  
  ser.RegisterField("u", "double", sizeof(double), 5, 6, 7, 1, 0, 0, 0, 0, 0, 0, 0, 0);
  ser.RegisterField("v", "double", sizeof(double), 5, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0);

  // Writing (implicitly register the savepoints)  
  ser.WriteField("u", savepoint1_t_1, (void*) u_0_input.originPtr(), u_0_input.strides()[0], u_0_input.strides()[1], u_0_input.strides()[2], 0);
  ser.WriteField("v", savepoint1_t_1, (void*) v_0_input.originPtr(), v_0_input.strides()[0], v_0_input.strides()[1], v_0_input.strides()[2], 0);
  ser.WriteField("u", savepoint1_t_2, (void*) u_1_input.originPtr(), u_1_input.strides()[0], u_1_input.strides()[1], u_1_input.strides()[2], 0);
  ser.WriteField("v", savepoint1_t_2, (void*) u_1_input.originPtr(), v_1_input.strides()[0], v_1_input.strides()[1], v_1_input.strides()[2], 0);
  ser.WriteField("u", savepoint_u_1, (void*) u_1_input.originPtr(), u_1_input.strides()[0], u_1_input.strides()[1], u_1_input.strides()[2], 0);
  ser.WriteField("v", savepoint_v_1, (void*) v_1_input.originPtr(), v_1_input.strides()[0], v_1_input.strides()[1], v_1_input.strides()[2], 0);
  
  // -----------------------------------------------------------------------------------------------
  // Reading (with new serialbox)
  // -----------------------------------------------------------------------------------------------
  
  // Implicitly upgrade the archive
  SerializerImpl s_read(OpenModeKind::Read, directory->path().string(), "BinaryArchive", "UpgradeArchiveTest");

//    // StorageViews
//    auto sv_u_0 = u_0_output.toStorageView();
//    auto sv_u_1 = u_1_output.toStorageView();
//    auto sv_v_0 = v_0_output.toStorageView();
//    auto sv_v_1 = v_1_output.toStorageView();
//    auto sv_field_6d = field_6d_output.toStorageView();
    
//    // Check fields exists
//    ASSERT_TRUE(s_read.hasField("u"));
//    ASSERT_EQ(s_read.getFieldMetaInfoOf("u").dims(), (std::vector<int>{5, 6, 7}));

//    ASSERT_TRUE(s_read.hasField("v"));
//    ASSERT_EQ(s_read.getFieldMetaInfoOf("v").dims(), (std::vector<int>{5, 1, 1}));

//    ASSERT_TRUE(s_read.hasField("field_6d"));
//    ASSERT_EQ(s_read.getFieldMetaInfoOf("field_6d").dims(), (std::vector<int>{2, 2, 1, 2, 1, 2}));

//    // Check order of savepoints is correct
//    ASSERT_EQ(s_read.savepoints().size(), 5);
//    EXPECT_EQ(s_read.savepoints(),
//              (std::vector<Savepoint>{savepoint1_t_1, savepoint1_t_2, savepoint_u_1, savepoint_v_1,
//                                      savepoint_6d}));
//    // Read
//    s_read.read("u", savepoint1_t_1, sv_u_0);
//    ASSERT_TRUE(Storage::verify(u_0_output, u_0_input));

//    s_read.read("v", savepoint1_t_1, sv_v_0);
//    ASSERT_TRUE(Storage::verify(v_0_output, v_0_input));

//    s_read.read("u", savepoint1_t_2, sv_u_1);
//    ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));

//    s_read.read("v", savepoint1_t_2, sv_v_1);
//    ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));

//    s_read.read("u", savepoint_u_1, sv_u_1);
//    ASSERT_TRUE(Storage::verify(u_1_output, u_1_input));

//    s_read.read("v", savepoint_v_1, sv_v_1);
//    ASSERT_TRUE(Storage::verify(v_1_output, v_1_input));
    
//    s_read.read("field_6d", savepoint_6d, sv_field_6d);
//    ASSERT_TRUE(Storage::verify(field_6d_output, field_6d_input));
//  }
}

#endif
