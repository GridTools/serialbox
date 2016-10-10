//===-- Utility/SerializerTestBase.h ------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file provides a template to setup Serializer tests.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UTILITY_SERIALIZERTESTBASE_H
#define SERIALBOX_UTILITY_SERIALIZERTESTBASE_H

#include "Utility/BenchmarkEnvironment.h"
#include "Utility/FileUtility.h"
#include "Utility/UnittestEnvironment.h"
#include "serialbox/Core/STLExtras.h"
#include <gtest/gtest.h>
#include <memory>

namespace serialbox {

namespace unittest {

/// \brief Simplify construction of serializer tests by automatically creating the necessary (empty)
/// directories
template <class Environment>
class SerializerTestBase : public testing::Test {
public:
  std::unique_ptr<Directory> directory;

protected:
  /// \brief Create an empty directory ready to use
  virtual void SetUp() override {
    directory = std::make_unique<unittest::Directory>(Environment::getInstance().directory() /
                                                      Environment::getInstance().testCaseName() /
                                                      Environment::getInstance().testName());
  }

  /// \brief Remove directory
  virtual void TearDown() override { directory.reset(); }
};

using SerializerUnittestBase = SerializerTestBase<UnittestEnvironment>;
using SerializerBenchmarkBase = SerializerTestBase<BenchmarkEnvironment>;

} // namespace unittest

} // namespace serialbox

#endif
