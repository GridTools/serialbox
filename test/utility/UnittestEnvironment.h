//===-- utility/UnittestEnvironment.h -----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Setup the global test environment.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UTILITY_UNITTESTENVIRONMENT_H
#define SERIALBOX_UTILITY_UNITTESTENVIRONMENT_H

#include <filesystem>
#include <gtest/gtest.h>

namespace serialbox {

namespace unittest {

/// \brief Global access to the testing infrastructure
class UnittestEnvironment : public ::testing::Environment /* singleton */ {
public:
  UnittestEnvironment(bool cleanup) : cleanup_(cleanup) {}

  /// \brief Return the instance of this singleton class
  static UnittestEnvironment& getInstance() noexcept;

  virtual void SetUp() override;
  virtual void TearDown() override;

  const std::filesystem::path& directory() const noexcept { return (*directory_); }
  std::filesystem::path& directory() noexcept { return (*directory_); }

  bool cleanup() const noexcept { return cleanup_; }

  std::string testCaseName() const;
  std::string testName() const;

private:
  bool cleanup_;
  std::unique_ptr<std::filesystem::path> directory_;
  static UnittestEnvironment* instance_;
};

} // namespace unittest

} // namespace serialbox

#endif
