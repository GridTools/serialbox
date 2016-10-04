//===-- serialbox/Core/UnittestStringSwitch.cpp -------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the StringSwitch class.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/StringSwitch.h"
#include <gtest/gtest.h>

using namespace serialbox;

namespace {

enum Color { UnknownColor, Red, Orange, Yellow, Green, Blue, Indigo, Violet };

static Color switchColor(std::string colorStr) {
  Color color = StringSwitch<Color>(colorStr)
                    .Case("red", Red)
                    .Case("orange", Orange)
                    .Case("yellow", Yellow)
                    .Case("green", Green)
                    .Case("blue", Blue)
                    .Case("indigo", Indigo)
                    .Cases("violet", "purple", Violet)
                    .Default(UnknownColor);
  return color;
}

} // anonymous namespace

TEST(StringSwitchTest, Swtich) {
  EXPECT_EQ(switchColor("red"), Red);
  EXPECT_EQ(switchColor("purple"), Violet);
  EXPECT_EQ(switchColor("RED"),UnknownColor);
}
