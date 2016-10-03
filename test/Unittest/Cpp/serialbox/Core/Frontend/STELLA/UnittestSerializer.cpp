//===-- serialbox/Core/Frontend/STELLA/UnittestSerializer.cpp -----------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the Serializer implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/Cpp/FileUtility.h"
#include "serialbox/Core/Frontend/STELLA/SerializationException.h"
#include "serialbox/Core/Frontend/STELLA/Serializer.h"
#include "serialbox/Core/SerializerImpl.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

//===------------------------------------------------------------------------------------------===//
//     Utility tests
//===------------------------------------------------------------------------------------------===//

namespace {

class STELLASerializerUtilityTest : public testing::Test {
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

TEST_F(STELLASerializerUtilityTest, ConstructionOfEmptySerializer) {
  // SerializerOpenModeWrite
  {
    ser::Serializer s;
    ASSERT_NO_THROW(s.Init(directory->path().string(), "Field", ser::SerializerOpenModeWrite));
    EXPECT_EQ(s.directory(), directory->path().string());
    EXPECT_EQ(s.prefix(), "Field");
    EXPECT_EQ(s.mode(), ser::SerializerOpenModeWrite);
  }

  // SerializerOpenModeAppend
  {
    ser::Serializer s;
    ASSERT_NO_THROW(s.Init(directory->path().string(), "Field", ser::SerializerOpenModeAppend));
  }

  // SerializerOpenModeRead

  // No meta-data found -> Exception
  {
    ser::Serializer s;
    ASSERT_THROW(s.Init(directory->path().string(), "Field", ser::SerializerOpenModeRead),
                 ser::SerializationException);
  }
}

TEST_F(STELLASerializerUtilityTest, AddMetaInfo) {
  ser::Serializer s;
  s.Init(directory->path().string(), "Field", ser::SerializerOpenModeWrite);

  // Add some meta-info
  s.AddMetainfo("bool", bool(true));
  s.AddMetainfo("int32", int(32));
  s.AddMetainfo("float32", float(32.0f));
  s.AddMetainfo("float64", double(64.0f));
  s.AddMetainfo("string", "str"); // This has to go through the const char* specialization

  // Query meta-info
  EXPECT_EQ(s.globalMetainfo().AsBool("bool"), bool(true));
  EXPECT_EQ(s.globalMetainfo().AsInt("int32"), int(32));
  EXPECT_EQ(s.globalMetainfo().AsFloat("float32"), float(32.0f));
  EXPECT_EQ(s.globalMetainfo().AsDouble("float64"), double(64.0f));
  EXPECT_EQ(s.globalMetainfo().AsString("string"), "str");

  // Check if information has been correctly passed to the SerializerImpl
  SerializerImpl& serializer = *s.getImpl();
  EXPECT_EQ(serializer.getGlobalMetainfoAs<int>("int32"), int(32));
  EXPECT_EQ(serializer.getGlobalMetainfoAs<float>("float32"), float(32.0f));
  EXPECT_EQ(serializer.getGlobalMetainfoAs<double>("float64"), double(64.0f));
  EXPECT_EQ(serializer.getGlobalMetainfoAs<std::string>("string"), "str");
}

TEST_F(STELLASerializerUtilityTest, RegisterField) {
  ser::Serializer ser;
  ser.Init(directory->path().string(), "Field", ser::SerializerOpenModeWrite);

  // Register some fields
  ser.RegisterField("field1", "int", 4, 42, 1, 1, 12, 1, 1, 0, 0, 0, 0, 2, 2);
  ser.RegisterField("field2", "double", 8, 42, 28, 80, 1, 3, 3, 3, 3, 0, 1, 0, 0);

  // Register already existing field (same meta-data) -> Returns false
  ASSERT_FALSE(ser.RegisterField("field1", "int", 4, 42, 1, 1, 12, 1, 1, 0, 0, 0, 0, 2, 2));

  // Register already existing field (different meta-data) -> Exception
  ASSERT_THROW(ser.RegisterField("field1", "int", 4, 42, 1, 1, 12, 100, 1, 0, 0, 0, 0, 2, 2),
               ser::SerializationException);

  // Register with inconsistent bytes-per-element -> Exception
  ASSERT_THROW(ser.RegisterField("field1", "int", 8, 42, 1, 1, 12, 1, 1, 0, 0, 0, 0, 2, 2),
               ser::SerializationException);

  // Query fields
  const ser::DataFieldInfo& field1 = ser.FindField("field1");
  EXPECT_EQ(std::string("field1"), field1.name());
  EXPECT_EQ(std::string("int"), field1.type());
  EXPECT_EQ(4, field1.bytesPerElement());
  EXPECT_EQ(2, field1.rank());
  EXPECT_EQ(42, field1.iSize());
  EXPECT_EQ(1, field1.jSize());
  EXPECT_EQ(1, field1.kSize());
  EXPECT_EQ(12, field1.lSize());
  EXPECT_EQ(1, field1.iMinusHaloSize());
  EXPECT_EQ(0, field1.jMinusHaloSize());
  EXPECT_EQ(0, field1.kMinusHaloSize());
  EXPECT_EQ(2, field1.lMinusHaloSize());
  EXPECT_EQ(1, field1.iPlusHaloSize());
  EXPECT_EQ(0, field1.jPlusHaloSize());
  EXPECT_EQ(0, field1.kPlusHaloSize());
  EXPECT_EQ(2, field1.lPlusHaloSize());

  const ser::DataFieldInfo& field2 = ser.FindField("field2");
  EXPECT_EQ(std::string("field2"), field2.name());
  EXPECT_EQ(std::string("double"), field2.type());
  EXPECT_EQ(8, field2.bytesPerElement());
  EXPECT_EQ(3, field2.rank());
  EXPECT_EQ(42, field2.iSize());
  EXPECT_EQ(28, field2.jSize());
  EXPECT_EQ(80, field2.kSize());
  EXPECT_EQ(1, field2.lSize());
  EXPECT_EQ(3, field2.iMinusHaloSize());
  EXPECT_EQ(3, field2.jMinusHaloSize());
  EXPECT_EQ(0, field2.kMinusHaloSize());
  EXPECT_EQ(0, field2.lMinusHaloSize());
  EXPECT_EQ(3, field2.iPlusHaloSize());
  EXPECT_EQ(3, field2.jPlusHaloSize());
  EXPECT_EQ(1, field2.kPlusHaloSize());
  EXPECT_EQ(0, field2.lPlusHaloSize());
}

TEST_F(STELLASerializerUtilityTest, FieldMetaInfo) {
  ser::Serializer ser;
  ser.Init(directory->path().string(), "Field", ser::SerializerOpenModeWrite);

  ser.RegisterField("field1", "int", 4, 42, 1, 1, 12, 1, 1, 0, 0, 0, 0, 2, 2);
  ser.RegisterField("field2", "double", 8, 42, 28, 80, 1, 3, 3, 3, 3, 0, 1, 0, 0);

  // Add some metainfo
  ser.AddFieldMetainfo("field1", "FirstField", true);
  ser.AddFieldMetainfo("field1", "InitValue", 10.75);
  ser.AddFieldMetainfo("field1", "Elements", 42 * 80);
  ser.AddFieldMetainfo("field2", "FirstField", false);
  ser.AddFieldMetainfo("field2", "AlternateName", "density");

  // Read metainfo
  ASSERT_EQ(3, ser.FindField("field1").metainfo().size());
  ASSERT_EQ(2, ser.FindField("field2").metainfo().size());

  ASSERT_EQ(true, ser.FindField("field1").metainfo().AsBool("FirstField"));
  ASSERT_EQ(10.75, ser.FindField("field1").metainfo().AsDouble("InitValue"));
  ASSERT_EQ(42 * 80, ser.FindField("field1").metainfo().AsInt("Elements"));
  ASSERT_EQ(false, ser.FindField("field2").metainfo().AsBool("FirstField"));
  ASSERT_EQ(std::string("density"), ser.FindField("field2").metainfo().AsString("AlternateName"));
}

//===------------------------------------------------------------------------------------------===//
//     Read/Write tests
//===------------------------------------------------------------------------------------------===//

#include "Utility/Cpp/STELLA.h"
#ifdef SERIALBOX_HAS_STELLA

namespace {

template <class T>
class STELLASerializerReadWriteTest : public testing::Test {
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

using TestTypes = testing::Types<double>;

} // anonymous namespace

TYPED_TEST_CASE(STELLASerializerReadWriteTest, TestTypes);

TYPED_TEST(STELLASerializerReadWriteTest, WriteAndRead) {

  // -----------------------------------------------------------------------------------------------
  // Write
  // -----------------------------------------------------------------------------------------------
  //
  //  Savepoint     | MetaData                               | Fields
  //  -----------------------------------------------------------------
  //  savepoint1    | time: 1, dt: 5.1, b: true, s: "str1"   | u_0, v_0
  //  savepoint1    | time: 2, dt: 9.1, b: false, s: "str2"  | u_1, v_1
  //  savepoint_u_1 | -                                      | u_1
  //  savepoint_v_1 | -                                      | v_1
  //
  {
    ser::Serializer ser_write;
    ser_write.Init(this->directory->path().string(), "Field", ser::SerializerOpenModeWrite);

    // Add some global metainfo
    ser_write.AddMetainfo("Day", int(29));
    ser_write.AddMetainfo("Month", std::string("March"));
    ser_write.AddMetainfo("Year", TypeParam(2016.10));
    ser_write.AddMetainfo("boolean", true);

    // Add savepoints
    ser::Savepoint savepoint1_t_1;
    ser::Savepoint savepoint1_t_2;
    ser::Savepoint savepoint_u_1;
    ser::Savepoint savepoint_v_1;

    savepoint1_t_1.Init("savepoint1");
    savepoint1_t_1.AddMetainfo("time", int(1));
    savepoint1_t_1.AddMetainfo("dt", TypeParam(5.1));
    savepoint1_t_1.AddMetainfo("b", bool(true));
    savepoint1_t_1.AddMetainfo("s", std::string("str1"));

    savepoint1_t_2.Init("savepoint1");
    savepoint1_t_2.AddMetainfo("time", int(2));
    savepoint1_t_2.AddMetainfo("dt", TypeParam(9.1));
    savepoint1_t_2.AddMetainfo("b", bool(false));
    savepoint1_t_2.AddMetainfo("s", std::string("str2"));

    savepoint_u_1.Init("savepoint_u_1");
    savepoint_v_1.Init("savepoint_v_1");

    // Register fields
    ser_write.RegisterField("u", ser::type_name<TypeParam>(), sizeof(TypeParam), 5, 6, 7, 1, 0, 0,
                            0, 0, 0, 0, 0, 0);
    ser_write.RegisterField("v", ser::type_name<TypeParam>(), sizeof(TypeParam), 5, 1, 1, 1, 0, 0,
                            0, 0, 0, 0, 0, 0);

    // Add some field meta-info
    ser_write.AddFieldMetainfo("u", "Day", int(29));
    ser_write.AddFieldMetainfo("u", "Month", "March");
    ser_write.AddFieldMetainfo("u", "Year", TypeParam(2016.10));
    ser_write.AddFieldMetainfo("v", "boolean", true);

    // Writing (implicitly register the savepoints)
//    int bytesPerElement = sizeof(TypeParam);

//    ser_write.WriteField(
//        "u", savepoint1_t_1, (void*)u_0_input.originPtr(), bytesPerElement * u_0_input.strides()[0],
//        bytesPerElement * u_0_input.strides()[1], bytesPerElement * u_0_input.strides()[2], 0);
//    ser_write.WriteField(
//        "v", savepoint1_t_1, (void*)v_0_input.originPtr(), bytesPerElement * v_0_input.strides()[0],
//        bytesPerElement * v_0_input.strides()[1], bytesPerElement * v_0_input.strides()[2], 0);
//    ser_write.WriteField(
//        "u", savepoint1_t_2, (void*)u_1_input.originPtr(), bytesPerElement * u_1_input.strides()[0],
//        bytesPerElement * u_1_input.strides()[1], bytesPerElement * u_1_input.strides()[2], 0);
//    ser_write.WriteField(
//        "v", savepoint1_t_2, (void*)v_1_input.originPtr(), bytesPerElement * v_1_input.strides()[0],
//        bytesPerElement * v_1_input.strides()[1], bytesPerElement * v_1_input.strides()[2], 0);
//    ser_write.WriteField(
//        "u", savepoint_u_1, (void*)u_1_input.originPtr(), bytesPerElement * u_1_input.strides()[0],
//        bytesPerElement * u_1_input.strides()[1], bytesPerElement * u_1_input.strides()[2], 0);
//    ser_write.WriteField(
//        "v", savepoint_v_1, (void*)v_1_input.originPtr(), bytesPerElement * v_1_input.strides()[0],
//        bytesPerElement * v_1_input.strides()[1], bytesPerElement * v_1_input.strides()[2], 0);
  }
}

#endif
