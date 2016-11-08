//===-- serialbox/core/BenchmarkSerialbox.cpp ---------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the benchmarks for all registered archives.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/SerializerTestBase.h"
#include "Utility/Storage.h"
#include "serialbox/core/archive/ArchiveFactory.h"
#include "serialbox/core/SerializerImpl.h"
#include "serialbox/core/Timer.h"
#include "serialbox/core/Type.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

/// Call write
template <class Serializer, class Storage>
static double writeField(Serializer&& serializer, const std::string& name, Storage&& storage,
                         const SavepointImpl& savepoint) {
  TypeID type = ToTypeID<typename std::decay<Storage>::type::value_type>::value;
  serializer.registerField(name, type, storage.dims());

  StorageView storageView(storage.toStorageView());

  Timer t;
  serializer.write(name, savepoint, storageView);
  return t.stop();
}

/// Call read
template <class Serializer, class Storage>
static double readField(Serializer&& serializer, const std::string& name, Storage&& storage,
                        const SavepointImpl& savepoint) {

  StorageView storageView(storage.toStorageView());
  Timer t;
  serializer.read(name, savepoint, storageView);
  return t.stop();
}

class SerialboxBenchmark : public SerializerBenchmarkBase,
                           public ::testing::WithParamInterface<std::string> {};

TEST_P(SerialboxBenchmark, Benchmark) {
  if(GetParam() == "Mock")
    return;

  std::string name = GetParam();
  BenchmarkResult result;
  result.name = name;

  const auto& sizes = BenchmarkEnvironment::getInstance().sizes();

  using Storage = Storage<double>;

  SavepointImpl savepoint("savepoint");

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
        SerializerImpl ser_write(OpenModeKind::Write, this->directory->path().string(), "field",
                                 GetParam());

        timingWrite += writeField(ser_write, "data1", data1, savepoint);
        timingWrite += writeField(ser_write, "data2", data2, savepoint);
        timingWrite += writeField(ser_write, "data3", data3, savepoint);
        timingWrite += writeField(ser_write, "data4", data4, savepoint);
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
        SerializerImpl ser_read(OpenModeKind::Read, this->directory->path().string(), "field",
                                GetParam());

        timingRead += readField(ser_read, "data1", data1, savepoint);
        timingRead += readField(ser_read, "data2", data2, savepoint);
        timingRead += readField(ser_read, "data3", data3, savepoint);
        timingRead += readField(ser_read, "data4", data4, savepoint);
      }
    }
    timingRead /= BenchmarkEnvironment::NumRepetitions;

    result.timingsRead.push_back(std::make_pair(size, timingRead));
  }

  BenchmarkEnvironment::getInstance().appendResult(result);
}

INSTANTIATE_TEST_CASE_P(BenchmarkTest, SerialboxBenchmark,
                        ::testing::ValuesIn(ArchiveFactory::registeredArchives()));
