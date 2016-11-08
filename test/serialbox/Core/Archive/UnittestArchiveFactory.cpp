//===-- serialbox/Core/Archive/UnittestArchiveFactory.cpp ---------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the ArchiveFactory.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/SerializerTestBase.h"
#include "Utility/Storage.h"
#include "serialbox/Core/Archive/ArchiveFactory.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

namespace {

class ArchiveFactoryTest : public SerializerUnittestBase,
                           public ::testing::WithParamInterface<std::string> {};

} // anonymous namespace

TEST_P(ArchiveFactoryTest, registerdArchives) {
  std::vector<std::string> archives(ArchiveFactory::registeredArchives());

  // BinaryArchive and Mock are always present
  EXPECT_GE(archives.size(), 2);
  EXPECT_TRUE(std::find(archives.begin(), archives.end(), "Binary") != archives.end());
  EXPECT_TRUE(std::find(archives.begin(), archives.end(), "Mock") != archives.end());
}

TEST_P(ArchiveFactoryTest, Extension) {
  ASSERT_EQ(ArchiveFactory::archiveFromExtension("test.dat"), "Binary");

  if(GetParam() == "NetCDF")
    ASSERT_EQ(ArchiveFactory::archiveFromExtension("test.nc"), "NetCDF");
  
  ASSERT_THROW(ArchiveFactory::archiveFromExtension("test.X").c_str(), Exception);
}

TEST_P(ArchiveFactoryTest, writeAndRead) {
  if(GetParam() == "Mock")
    return;

  using Storage = Storage<double>;
  Storage storage_input(Storage::ColMajor, {5, 2, 5}, Storage::random);
  Storage storage_output(Storage::ColMajor, {5, 2, 5});

  auto sv_input = storage_input.toStorageView();
  auto sv_output = storage_output.toStorageView();

  // Write and read from file
  ArchiveFactory::writeToFile((directory->path() / "test.dat").string(), sv_input, GetParam(),
                              "field");
  ArchiveFactory::readFromFile((directory->path() / "test.dat").string(), sv_output, GetParam(),
                               "field");
  ASSERT_TRUE(Storage::verify(storage_input, storage_output));

  // Invalid Archive -> Exception
  ASSERT_THROW(ArchiveFactory::writeToFile((directory->path() / "test.dat").string(), sv_input,
                                           "Not-a-archive", "field"),
               Exception);
  ASSERT_THROW(ArchiveFactory::readFromFile((directory->path() / "test.dat").string(), sv_output,
                                            "Not-a-archive", "field"),
               Exception);
}

INSTANTIATE_TEST_CASE_P(ArchiveTest, ArchiveFactoryTest,
                        ::testing::ValuesIn(ArchiveFactory::registeredArchives()));
