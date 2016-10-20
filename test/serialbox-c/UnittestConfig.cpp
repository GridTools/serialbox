//===-- serialbox-c/UnittestConfig.cpp ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the unittests of the C Interface Configuration.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox-c/Config.h"
#include "Utility/CInterfaceTestBase.h"

namespace {

class CConfigTest : public serialbox::unittest::CInterfaceTestBase {};

} // anonymous namespace

TEST_F(CConfigTest, Test) {
  char* config = serialboxConfigOptions();
  ASSERT_TRUE(NULL != config);
  std::free(config);
}
