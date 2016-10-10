//===-- serialbox/Core/UnittestVersion.cpp ------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the Version class.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/Serialbox.h"
#include "Utility/SerializerTestBase.h"
#include "Utility/Storage.h"
#include "serialbox/Core/StorageView.h"
#include "serialbox/Core/Timer.h"
#include "serialbox/Core/Unreachable.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

#if defined(SERIALBOX_HAS_SERIALBOX_OLD)

/// Call ser::Serializer::WriteField
template <class Serializer>
static double writeField(Serializer&& serializer, const std::string& name,
                         const StorageView& storageView, const ser::Savepoint& savepoint) {

  const int bytesPerElement = storageView.bytesPerElement();

  const auto& strides = storageView.strides();
  int iStride = (strides.size() < 1 ? 0 : strides[0]) * bytesPerElement;
  int jStride = (strides.size() < 2 ? 0 : strides[1]) * bytesPerElement;
  int kStride = (strides.size() < 3 ? 0 : strides[2]) * bytesPerElement;
  int lStride = (strides.size() < 4 ? 0 : strides[3]) * bytesPerElement;

  const auto& sizes = storageView.dims();
  int iSize = sizes.size() < 1 ? 1 : sizes[0];
  int jSize = sizes.size() < 2 ? 1 : sizes[1];
  int kSize = sizes.size() < 3 ? 1 : sizes[2];
  int lSize = sizes.size() < 4 ? 1 : sizes[3];

  // Register field
  std::string type;
  switch(storageView.type()) {
  case TypeID::Int32:
    type = "int";
    break;
  case TypeID::Float32:
    type = "float";
    break;
  case TypeID::Float64:
    type = "double";
    break;
  default:
    serialbox_unreachable("invalid type-id");
  }

  serializer.RegisterField(name, type, bytesPerElement, iSize, jSize, kSize, lSize, 0, 0, 0, 0, 0,
                           0, 0, 0);

  // Write field
  Timer t;
  serializer.WriteField(name, savepoint, storageView.originPtr(), iStride, jStride, kStride,
                        lStride);
  return t.stop();
}

/// Call ser::Serializer::ReadField
template <class Serializer>
static double readField(Serializer&& serializer, const std::string& name,
                        const StorageView& storageView, const ser::Savepoint& savepoint) {

  const int bytesPerElement = storageView.bytesPerElement();

  const auto& strides = storageView.strides();
  int iStride = (strides.size() < 1 ? 0 : strides[0]) * bytesPerElement;
  int jStride = (strides.size() < 2 ? 0 : strides[1]) * bytesPerElement;
  int kStride = (strides.size() < 3 ? 0 : strides[2]) * bytesPerElement;
  int lStride = (strides.size() < 4 ? 0 : strides[3]) * bytesPerElement;

  // Read field
  Timer t;
  serializer.ReadField(name, savepoint, (void*)storageView.originPtr(), iStride, jStride, kStride,
                       lStride);
  return t.stop();
}

class OldSerialboxBenchmark : public SerializerBenchmarkBase {};

TEST_F(OldSerialboxBenchmark, Benchmark) {
  std::string name = "Serialbox (0.1.1)";
  BenchmarkResult result;
  result.name = name;

  const auto& sizes = BenchmarkEnvironment::getInstance().sizes();

  using Storage = Storage<double>;

  ser::Savepoint savepoint;
  savepoint.Init("savepoint");

  for(std::size_t i = 0; i < sizes.size(); ++i) {
    const Size& size = sizes[i];

    //
    // Allocate data
    //
    Storage data1(Storage::RowMajor, size.dimensions, Storage::random);
    Storage data2(Storage::ColMajor, size.dimensions, Storage::random);
    Storage data3(Storage::RowMajor, size.dimensions, Storage::random);
    Storage data4(Storage::ColMajor, size.dimensions, Storage::random);

    //
    // Write data
    //
    double timingWrite = 0.0;
    {
      for(int n = 0; n < BenchmarkEnvironment::NumRepetitions; ++n) {
        ser::Serializer ser_write;
        ser_write.Init(this->directory->path().string(),
                       BenchmarkEnvironment::getInstance().testName(),
                       ser::SerializerOpenModeWrite);

        timingWrite += writeField(ser_write, "data1", data1.toStorageView(), savepoint);
        timingWrite += writeField(ser_write, "data2", data2.toStorageView(), savepoint);
        timingWrite += writeField(ser_write, "data3", data3.toStorageView(), savepoint);
        timingWrite += writeField(ser_write, "data4", data4.toStorageView(), savepoint);
      }
    }
    timingWrite /= BenchmarkEnvironment::NumRepetitions;

    result.timingsWrite.push_back(std::make_pair(size, timingWrite));

    //
    // Read data
    //
    double timingRead = 0.0;
    {
      for(int n = 0; n < BenchmarkEnvironment::NumRepetitions; ++n) {
        ser::Serializer ser_read;
        ser_read.Init(this->directory->path().string(),
                      BenchmarkEnvironment::getInstance().testName(), ser::SerializerOpenModeRead);

        timingRead += readField(ser_read, "data1", data1.toStorageView(), savepoint);
        timingRead += readField(ser_read, "data2", data2.toStorageView(), savepoint);
        timingRead += readField(ser_read, "data3", data3.toStorageView(), savepoint);
        timingRead += readField(ser_read, "data4", data4.toStorageView(), savepoint);
      }
    }
    timingRead /= BenchmarkEnvironment::NumRepetitions;

    result.timingsRead.push_back(std::make_pair(size, timingRead));
  }

  BenchmarkEnvironment::getInstance().appendResult(result);
}

#endif
