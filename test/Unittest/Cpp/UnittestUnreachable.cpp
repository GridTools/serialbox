//===-- Unittest/Cpp/UnittestUnreachable.cpp ----------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the Unreachable utility.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Unreachable.h"
#include <gtest/gtest.h>

namespace {

enum AnimalKind {
  Dog,
  Cat,
  Lion,
};

} // anonymous namespace

static const char* petName(AnimalKind animal) {
  switch(animal) {
  case AnimalKind::Dog:
    return "Dog";
  case AnimalKind::Cat:
    return "Cat";
  default:
    serialbox_unreachable("Not a pet!");
  }
}

TEST(UnreachableTest, Unreachable) {
#ifndef NDEBUG
  ASSERT_DEATH_IF_SUPPORTED(petName(AnimalKind::Lion), "");
#endif
}
