//===-- serialbox/core/archive/UnittestBinaryArchive.cpp ----------------------------*- C++ -*-===//
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

#include "utility/SerializerTestBase.h"
#include "utility/Storage.h"
#include "serialbox/core/archive/MockArchive.h"
#include <gtest/gtest.h>

using namespace serialbox;
using namespace unittest;

namespace {

template <class T>
class MockArchiveReadWriteTest : public SerializerUnittestBase {};

using TestTypes = testing::Types<double, float, int, std::int64_t>;

} // anonymous namespace

TYPED_TEST_CASE(MockArchiveReadWriteTest, TestTypes);

TYPED_TEST(MockArchiveReadWriteTest, WriteAndRead) {
  using Storage = Storage<TypeParam>;

  Storage field(Storage::RowMajor, {5, 6, 7}, {{2, 2}, {4, 2}, {4, 5}}, Storage::sequential);

  // Fill with random data
  MockArchive archive(OpenModeKind::Read);
  auto sv = field.toStorageView();

  auto before = field(0, 0, 0);
  archive.read(sv, FieldID{"field", 0}, nullptr);
  ASSERT_NE(field(0, 0, 0), before);

  // Writing is not supported -> Exception
  ASSERT_THROW(archive.write(sv, "field", nullptr), Exception);
}
