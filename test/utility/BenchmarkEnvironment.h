//===-- utility/BenchmarkEnvironment.h ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file provides the interface to define and register benchmarks.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UTILITY_BENCHMARKENVIRONMENT_H
#define SERIALBOX_UTILITY_BENCHMARKENVIRONMENT_H

#include "utility/UnittestEnvironment.h"
#include <gtest/gtest.h>

namespace serialbox {

namespace unittest {

/// \brief Represent the dimensions used to construct the fields for the benchmark
/// (e.g {1024, 1025, 80})
struct Size {
  std::vector<int> dimensions;
  std::string toString() const;
};

/// \brief Represent a result of a benchmark run
struct BenchmarkResult {
  std::string name;
  std::vector<std::pair<Size, double>> timingsWrite;
  std::vector<std::pair<Size, double>> timingsRead;
};

/// \brief Global access to the benchmarking infrastructure
class BenchmarkEnvironment : public ::testing::Environment /* singleton */ {
public:
  static constexpr int NumRepetitions = 5;

  /// \brief Return the instance of this singleton class
  static BenchmarkEnvironment& getInstance() noexcept;

  virtual void SetUp() override;
  virtual void TearDown() override;

  const std::filesystem::path& directory() const noexcept { return (*directory_); }
  std::filesystem::path& directory() noexcept { return (*directory_); }

  /// \brief Add a benchmark results
  void appendResult(const BenchmarkResult& result) { results_.push_back(result); }

  /// \brief Report results
  void reportResults() const;

  /// \brief Get the sizes of the benchmark
  const std::vector<Size>& sizes() const noexcept { return sizes_; }

  std::string testCaseName() const;
  std::string testName() const;

private:
  std::unique_ptr<std::filesystem::path> directory_;
  std::vector<BenchmarkResult> results_;

  std::vector<Size> sizes_;

  static BenchmarkEnvironment* instance_;
};

} // namespace unittest

} // namespace serialbox

#endif
