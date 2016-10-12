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
/// This file contains the unittests for the Exception class.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Archive/ArchiveFactory.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(ArchiveFactory, registerdArchives) {
  std::vector<std::string> archives(ArchiveFactory::getInstance().registeredArchives());

  // BinaryArchive is always present
  EXPECT_GE(archives.size(), 1);
  EXPECT_TRUE(std::find(archives.begin(), archives.end(), "Binary") != archives.end());
}
