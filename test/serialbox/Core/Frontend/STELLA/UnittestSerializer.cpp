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

#include "Utility/STELLA.h"
#include "Utility/SerializerTestBase.h"
#include "Utility/Storage.h"
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

class STELLASerializerUtilityTest : public SerializerUnittestBase {};

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
    EXPECT_EQ(s.mode(), ser::SerializerOpenModeAppend);
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

  // Field does not exists -> Exception
  ASSERT_THROW(ser.FindField("field3"), ser::SerializationException);
}

TEST_F(STELLASerializerUtilityTest, FieldMetaInfo) {
  ser::Serializer ser;
  ser.Init(directory->path().string(), "Field", ser::SerializerOpenModeWrite);

  ser.RegisterField("field1", "int", 4, 42, 1, 1, 12, 1, 1, 0, 0, 0, 0, 2, 2);
  ser.RegisterField("field2", "double", 8, 42, 28, 80, 1, 3, 3, 3, 3, 0, 1, 0, 0);

  // Add some metainfo (second invocation should always throw)
  ser.AddFieldMetainfo("field1", "FirstField", true);
  ASSERT_THROW(ser.AddFieldMetainfo("field1", "FirstField", true), ser::SerializationException);

  ser.AddFieldMetainfo("field1", "InitValue", 10.75);
  ASSERT_THROW(ser.AddFieldMetainfo("field1", "InitValue", 10.75), ser::SerializationException);

  ser.AddFieldMetainfo("field1", "InitValue2", 0.75f);
  ASSERT_THROW(ser.AddFieldMetainfo("field1", "InitValue2", 0.75f), ser::SerializationException);

  ser.AddFieldMetainfo("field1", "Elements", 42 * 80);
  ASSERT_THROW(ser.AddFieldMetainfo("field1", "Elements", 42 * 80), ser::SerializationException);

  ser.AddFieldMetainfo("field2", "FirstField", false);
  ASSERT_THROW(ser.AddFieldMetainfo("field2", "FirstField", false), ser::SerializationException);

  ser.AddFieldMetainfo("field2", "AlternateName", "density");
  ASSERT_THROW(ser.AddFieldMetainfo("field2", "AlternateName", "density"),
               ser::SerializationException);

  // Read metainfo
  ASSERT_EQ(4, ser.FindField("field1").metainfo().size());
  ASSERT_EQ(2, ser.FindField("field2").metainfo().size());

  ASSERT_EQ(true, ser.FindField("field1").metainfo().AsBool("FirstField"));
  ASSERT_EQ(10.75, ser.FindField("field1").metainfo().AsDouble("InitValue"));
  ASSERT_EQ(0.75f, ser.FindField("field1").metainfo().AsFloat("InitValue2"));
  ASSERT_EQ(42 * 80, ser.FindField("field1").metainfo().AsInt("Elements"));
  ASSERT_EQ(false, ser.FindField("field2").metainfo().AsBool("FirstField"));
  ASSERT_EQ(std::string("density"), ser.FindField("field2").metainfo().AsString("AlternateName"));
}

//===------------------------------------------------------------------------------------------===//
//     STELLA Read/Write tests
//===------------------------------------------------------------------------------------------===//

#ifdef SERIALBOX_HAS_STELLA

namespace {

template <class T>
class STELLASerializerReadWriteTest : public SerializerUnittestBase {};

using TestTypes = testing::Types<double, float, int>;

} // anonymous namespace

TYPED_TEST_CASE(STELLASerializerReadWriteTest, TestTypes);

/// Call ser::Serializer::WriteField with dataField
template <class SerializerType, class FieldType, class StringType>
static void writeField(SerializerType& serializer, StringType&& name, FieldType& dataField,
                       const ser::Savepoint& savepoint) {

  DataFieldStorageStrides<typename FieldType::StorageFormat::StorageOrder> storageStrides;
  storageStrides.Init(dataField.storage().paddedSize());

  const int bytesPerElement = sizeof(typename FieldType::ValueType);

  int iStride = storageStrides.ComputeStride(1, 0, 0) * bytesPerElement;
  int jStride = storageStrides.ComputeStride(0, 1, 0) * bytesPerElement;
  int kStride = storageStrides.ComputeStride(0, 0, 1) * bytesPerElement;

  serializer.WriteField(name, dataField, savepoint, iStride, jStride, kStride);
}

/// Call ser::Serializer::ReadField with dataField
template <class SerializerType, class FieldType, class StringType>
static void readField(SerializerType& serializer, StringType&& name, FieldType& dataField,
                      const ser::Savepoint& savepoint) {

  DataFieldStorageStrides<typename FieldType::StorageFormat::StorageOrder> storageStrides;
  storageStrides.Init(dataField.storage().paddedSize());

  const int bytesPerElement = sizeof(typename FieldType::ValueType);

  int iStride = storageStrides.ComputeStride(1, 0, 0) * bytesPerElement;
  int jStride = storageStrides.ComputeStride(0, 1, 0) * bytesPerElement;
  int kStride = storageStrides.ComputeStride(0, 0, 1) * bytesPerElement;

  serializer.ReadField(name, dataField, savepoint, true, true, iStride, jStride, kStride, false);
}

/// Verify data fields are bit-wise identical
template <class FieldType, class RefFieldType>
static testing::AssertionResult verifyFields(FieldType&& field, RefFieldType&& refField) {
  const IJKSize& calculationDomain = field.calculationDomain();
  const IJKBoundary& boundary = field.boundary();

  const int Ni = calculationDomain.iSize();
  const int Nj = calculationDomain.jSize();
  const int Nk = calculationDomain.kSize();

  for(int i = boundary.iMinusOffset(); i < (Ni + boundary.iPlusOffset()); ++i)
    for(int j = boundary.jMinusOffset(); j < (Nj + boundary.jPlusOffset()); ++j)
      for(int k = boundary.kMinusOffset(); k < (Nk + +boundary.kPlusOffset()); ++k)
        if(field(i, j, k) != refField(i, j, k)) {
          std::stringstream ss;
          ss << "\nStorage mismatch:\n";
          ss << " storage  at (" << i << ", " << j << ", " << k << ") = " << field(i, j, k) << "\n";
          ss << " refrence at (" << i << ", " << j << ", " << k << ") = " << refField(i, j, k)
             << "\n";
          return testing::AssertionFailure() << ss.str().c_str();
        }
  return testing::AssertionSuccess();
}

TYPED_TEST(STELLASerializerReadWriteTest, WriteAndRead) {
  // Sizes (including halos)
  int iSize = 12;
  int jSize = 18;
  int kSize = 10;

  // Savepoints
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

  using IJKField =
      DataFieldOpenMP<TypeParam,
                      DataFieldStorageFormat<OpenMPIJBoundary, StorageOrder::JIK, OpenMPAlignment>>;

  IJKField u_0_input, u_1_input, v_0_input, v_1_input;

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

    // Register fields
    ser_write.RegisterField("u", ser::type_name<TypeParam>(), sizeof(TypeParam), iSize, jSize,
                            kSize, 1, 3, 3, 3, 3, 0, 0, 0, 0);
    ser_write.RegisterField("v", ser::type_name<TypeParam>(), sizeof(TypeParam), iSize, jSize, 1, 1,
                            3, 3, 3, 3, 0, 0, 0, 0);

    ser_write.InitializeField("u", u_0_input, true, true);
    ser_write.InitializeField("u", u_1_input, true, true);
    ser_write.InitializeField("v", v_0_input, true, true);
    ser_write.InitializeField("v", v_1_input, true, true);

    // Fill some random values
    bool isFp = std::is_floating_point<TypeParam>::value;
    const IJKBoundary& boundary = u_0_input.boundary();
    for(int i = boundary.iMinusOffset(); i < (iSize + boundary.iMinusOffset()); ++i)
      for(int j = boundary.jMinusOffset(); j < (jSize + boundary.jMinusOffset()); ++j) {
        v_0_input(i, j, 0) = (isFp ? (TypeParam(std::rand()) / RAND_MAX) : std::rand());
        v_1_input(i, j, 0) = (isFp ? (TypeParam(std::rand()) / RAND_MAX) : std::rand());
        for(int k = boundary.kMinusOffset(); k < (kSize + boundary.kMinusOffset()); ++k) {
          u_0_input(i, j, k) = (isFp ? (TypeParam(std::rand()) / RAND_MAX) : std::rand());
          u_1_input(i, j, k) = (isFp ? (TypeParam(std::rand()) / RAND_MAX) : std::rand());
        }
      }

    // Writing (implicitly register the savepoints)
    writeField(ser_write, "u", u_0_input, savepoint1_t_1);
    writeField(ser_write, "v", v_0_input, savepoint1_t_1);
    writeField(ser_write, "u", u_1_input, savepoint1_t_2);
    writeField(ser_write, "v", v_1_input, savepoint1_t_2);
    writeField(ser_write, "u", u_1_input, savepoint_u_1);
    writeField(ser_write, "v", v_1_input, savepoint_v_1);

    // Write field with wrong dimensions -> Exception
    ASSERT_THROW(writeField(ser_write, "v", u_0_input, savepoint1_t_1),
                 ser::SerializationException);

    // Write field with wrong type -> Exception
    if(std::is_same<TypeParam, double>::value) {
      IJKIntField field;
      ser_write.InitializeField("u", field, true, true);
      ASSERT_THROW(writeField(ser_write, "u", field, savepoint1_t_1),
                   ser::SerializationException);
    }
  }

  // -----------------------------------------------------------------------------------------------
  // Read
  // -----------------------------------------------------------------------------------------------
  {
    ser::Serializer ser_read;
    ser_read.Init(this->directory->path().string(), "Field", ser::SerializerOpenModeRead);

    // Verify global metainfo
    EXPECT_EQ(ser_read.globalMetainfo().AsInt("Day"), 29);
    EXPECT_EQ(ser_read.globalMetainfo().AsString("Month"), "March");
    EXPECT_EQ(ser_read.globalMetainfo().AsBool("boolean"), true);

    EXPECT_EQ(boost::any_cast<TypeParam>(ser_read.globalMetainfo().AsAny("Year")),
              TypeParam(2016.10));

    IJKField u_0_output, u_1_output, v_0_output, v_1_output;
    ser_read.InitializeField("u", u_0_output, true, true);
    ser_read.InitializeField("u", u_1_output, true, true);
    ser_read.InitializeField("v", v_0_output, true, true);
    ser_read.InitializeField("v", v_1_output, true, true);

    // Check savepoint ordering
    ASSERT_EQ(ser_read.savepoints().size(), 4);
    ASSERT_EQ(ser_read.savepoints()[0], savepoint1_t_1);
    ASSERT_EQ(ser_read.savepoints()[1], savepoint1_t_2);
    ASSERT_EQ(ser_read.savepoints()[2], savepoint_u_1);
    ASSERT_EQ(ser_read.savepoints()[3], savepoint_v_1);

    // Check fields at savepoint
    auto fields = ser_read.FieldsAtSavepoint(savepoint1_t_1);
    ASSERT_EQ(fields.size(), 2);
    ASSERT_NE(std::find(fields.begin(), fields.end(), "u"), fields.end());
    ASSERT_NE(std::find(fields.begin(), fields.end(), "v"), fields.end());

    // Read
    readField(ser_read, "u", u_0_output, savepoint1_t_1);
    ASSERT_TRUE(verifyFields(u_0_output, u_0_input));

    readField(ser_read, "v", v_0_output, savepoint1_t_1);
    ASSERT_TRUE(verifyFields(v_0_output, v_0_input));

    readField(ser_read, "u", u_1_output, savepoint1_t_2);
    ASSERT_TRUE(verifyFields(u_1_output, u_1_input));

    readField(ser_read, "v", v_1_output, savepoint1_t_2);
    ASSERT_TRUE(verifyFields(v_1_output, v_1_input));

    // Read into u_0_output as u_1_output already has the correct result
    readField(ser_read, "u", u_0_output, savepoint_u_1);
    ASSERT_TRUE(verifyFields(u_0_output, u_1_input));

    // Read into v_0_output as v_1 already_output has the correct result
    readField(ser_read, "v", v_0_output, savepoint_v_1);
    ASSERT_TRUE(verifyFields(v_0_output, v_1_input));

    // Read field with wrong dimensions -> Exception
    ASSERT_THROW(readField(ser_read, "v", u_0_input, savepoint1_t_1), ser::SerializationException);

    // Read field with wrong type -> Exception
    if(std::is_same<TypeParam, double>::value) {
      IJKIntField field;
      ser_read.InitializeField("u", field, true, true);
      ASSERT_THROW(readField(ser_read, "u", field, savepoint1_t_1),
                   ser::SerializationException);
    }
  }
}

#endif // SERIALBOX_HAS_STELLA

//===------------------------------------------------------------------------------------------===//
//     Old serialbox Read/Write tests
//===------------------------------------------------------------------------------------------===//

template <typename T>
bool CheckVector(const std::vector<T>& ref, const std::vector<T>& val) {
  int size = ref.size();
  EXPECT_EQ(size, val.size());

  for(int i = 0; i < size; ++i)
    if(ref[i] != val[i])
      return false;

  return true;
}

TEST(STELLASerializerLagecyTest, WriteAndRead) {
  std::vector<int> fieldInt2, fieldInt3;
  std::vector<double> fieldDouble1, fieldDouble3;
  std::vector<float> fieldFloat1, fieldFloat3;
  std::vector<int> fieldcheckInt2, fieldcheckInt3;
  std::vector<double> fieldcheckDouble1, fieldcheckDouble3;
  std::vector<float> fieldcheckFloat1, fieldcheckFloat3;

  ser::Savepoint sp;
  sp.Init("TestSavepoint");

  Directory directory(UnittestEnvironment::getInstance().directory() /
                      UnittestEnvironment::getInstance().testCaseName() /
                      UnittestEnvironment::getInstance().testName());
  int iSize = 12;
  int jSize = 18;
  int kSize = 10;

  int intSize = sizeof(int);
  int doubleSize = sizeof(double);
  int floatSize = sizeof(float);

  // Allocate fields
  fieldInt2.resize(iSize * kSize);
  fieldInt3.resize(iSize * jSize * kSize);
  fieldDouble1.resize(jSize);
  fieldDouble3.resize(iSize * jSize * kSize);
  fieldFloat1.resize(jSize);
  fieldFloat3.resize(iSize * jSize * kSize);
  fieldcheckInt2.resize(iSize * kSize);
  fieldcheckInt3.resize(iSize * jSize * kSize);
  fieldcheckDouble1.resize(jSize);
  fieldcheckDouble3.resize(iSize * jSize * kSize);
  fieldcheckFloat1.resize(jSize);
  fieldcheckFloat3.resize(iSize * jSize * kSize);

  // -----------------------------------------------------------------------------------------------
  // Write
  // -----------------------------------------------------------------------------------------------
  {
    ser::Serializer ser;
    ser.Init(directory.path().string(), "ReadWriteUnittest", ser::SerializerOpenModeWrite);

    // Register fields
    ser.RegisterField("int3", "int", intSize, iSize, jSize, kSize, 1, 0, 0, 0, 0, 0, 0, 0, 0);
    ser.RegisterField("int2", "int", intSize, iSize, 1, kSize, 1, 0, 0, 0, 0, 0, 0, 0, 0);
    ser.RegisterField("double3", "double", doubleSize, iSize, jSize, kSize, 1, 0, 0, 0, 0, 0, 0, 0,
                      0);
    ser.RegisterField("double1", "double", doubleSize, 1, jSize, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0);
    ser.RegisterField("float3", "float", floatSize, iSize, jSize, kSize, 1, 0, 0, 0, 0, 0, 0, 0, 0);
    ser.RegisterField("float1", "float", floatSize, 1, jSize, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0);

    for(int i = 0; i < iSize; ++i)
      for(int j = 0; j < jSize; ++j)
        for(int k = 0; k < kSize; ++k) {
          fieldInt3[i + j * iSize + k * iSize * jSize] = i + 2 * j - 12 * k * i;
          fieldDouble3[i + j * iSize + k * iSize * jSize] = 1.125 * i + 2.25 * j - 12.5 * k * i;
          fieldFloat3[i + j * iSize + k * iSize * jSize] = 1.5f * i + 4.25f * j - 16.0f * k * i;
        }

    for(int i = 0; i < iSize; ++i)
      for(int k = 0; k < kSize; ++k) {
        fieldInt2[i + k * iSize] = i - 12 * k * i;
      }

    for(int j = 0; j < jSize; ++j) {
      fieldDouble1[j] = j * j + 2.875;
      fieldFloat1[j] = j * j + 3.5f;
    }

    ser.WriteField("int3", sp, fieldInt3.data(), intSize, intSize * iSize, intSize * iSize * jSize,
                   0);
    ser.WriteField("int2", sp, fieldInt2.data(), intSize, 0, intSize * iSize, 0);
    ser.WriteField("double3", sp, fieldDouble3.data(), doubleSize, doubleSize * iSize,
                   doubleSize * iSize * jSize, 0);
    ser.WriteField("double1", sp, fieldDouble1.data(), 0, doubleSize, 0, 0);
    ser.WriteField("float3", sp, fieldFloat3.data(), floatSize, floatSize * iSize,
                   floatSize * iSize * jSize, 0);
    ser.WriteField("float1", sp, fieldFloat1.data(), 0, floatSize, 0, 0);
  }

  // -----------------------------------------------------------------------------------------------
  // Read
  // -----------------------------------------------------------------------------------------------
  {

    ser::Serializer ser;
    ser.Init(directory.path().string(), "ReadWriteUnittest", ser::SerializerOpenModeRead);

    ser.ReadField("int3", sp, fieldcheckInt3.data(), intSize, intSize * iSize,
                  intSize * iSize * jSize, 0);
    ser.ReadField("int2", sp, fieldcheckInt2.data(), intSize, 0, intSize * iSize, 0);
    ser.ReadField("double3", sp, fieldcheckDouble3.data(), doubleSize, doubleSize * iSize,
                  doubleSize * iSize * jSize, 0);
    ser.ReadField("double1", sp, fieldcheckDouble1.data(), 0, doubleSize, 0, 0);
    ser.ReadField("float3", sp, fieldcheckFloat3.data(), floatSize, floatSize * iSize,
                  floatSize * iSize * jSize, 0);
    ser.ReadField("float1", sp, fieldcheckFloat1.data(), 0, floatSize, 0, 0);

    // Check
    ASSERT_TRUE(CheckVector(fieldInt3, fieldcheckInt3));
    ASSERT_TRUE(CheckVector(fieldInt2, fieldcheckInt2));
    ASSERT_TRUE(CheckVector(fieldDouble3, fieldcheckDouble3));
    ASSERT_TRUE(CheckVector(fieldDouble1, fieldcheckDouble1));
    ASSERT_TRUE(CheckVector(fieldFloat3, fieldcheckFloat3));
    ASSERT_TRUE(CheckVector(fieldFloat1, fieldcheckFloat1));
  }
}
