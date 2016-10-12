//===-- serialbox-c/UnittestArchive.cpp ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface Archive.
///
//===------------------------------------------------------------------------------------------===//

#include "Utility/CInterfaceTestBase.h"
#include "serialbox-c/Archive.h"
#include <algorithm>
#include <gtest/gtest.h>

namespace {

class CArchiveTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

TEST_F(CArchiveTest, Test) {
  int len;
  char** archives;
  serialboxArchiveGetRegisteredArchives(&archives, &len);

  ASSERT_GE(len, 1);
  ASSERT_TRUE(std::find_if(archives, archives + len, [](const char* s) {
                return (std::memcmp(s, "Binary", sizeof("Binary")) == 0);
              }) != (archives + len));

  for(int i = 0; i < len; ++i)
    std::free(archives[i]);
  std::free(archives);
}
