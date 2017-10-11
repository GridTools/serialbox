//===-- serialbox/core/UnittestVersion.cpp ------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/Version.h"
#include <gtest/gtest.h>

using namespace serialbox;

TEST(VersionTest, Comparison) {
  int major = SERIALBOX_VERSION_MAJOR;
  int minor = SERIALBOX_VERSION_MINOR;
  int patch = SERIALBOX_VERSION_PATCH;

  EXPECT_TRUE(Version::match(major * 100 + minor * 10 + patch));
  EXPECT_FALSE(Version::match(0));

  EXPECT_TRUE(Version::isCompatible(major * 100 + minor * 10 + (patch - 1)));
  EXPECT_FALSE(Version::isCompatible(major * 100 + minor * 10 + (patch + 1)));
}

TEST(VersionTest, ToString) {
  int major = SERIALBOX_VERSION_MAJOR;
  int minor = SERIALBOX_VERSION_MINOR;
  int patch = SERIALBOX_VERSION_PATCH;

  EXPECT_STREQ(Version::toString(major, minor, patch).c_str(), SERIALBOX_VERSION_STRING);
  EXPECT_STREQ(Version::toString(100 * major + 10 * minor + patch).c_str(),
               SERIALBOX_VERSION_STRING);
}
