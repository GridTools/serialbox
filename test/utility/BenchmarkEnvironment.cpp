//===-- utility/BenchmarkEnvironment.cpp --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file provides the interface to define and register performance Unittests
///
//===------------------------------------------------------------------------------------------===//

#include "utility/BenchmarkEnvironment.h"
#include "serialbox/core/Logging.h"
#include "serialbox/core/STLExtras.h"
#include <boost/format.hpp>
#include <iostream>

namespace serialbox {

namespace unittest {

std::string Size::toString() const {
  std::stringstream ss;
  if(!dimensions.empty()) {
    ss << "[ ";
    for(std::size_t i = 0; i < dimensions.size() - 1; ++i)
      ss << dimensions[i] << ", ";
    ss << dimensions.back() << " ]";
  }
  return ss.str();
}

BenchmarkEnvironment* BenchmarkEnvironment::instance_ = nullptr;

BenchmarkEnvironment& BenchmarkEnvironment::getInstance() noexcept {
  if(!instance_)
    instance_ = new BenchmarkEnvironment;
  return (*instance_);
}

void BenchmarkEnvironment::SetUp() {
  bool hasError = false;
  std::string errStr;

  // Try to create a path to run our unittests in the form "$(pwd)/benchmark-tmp-dir/"
  try {
    directory_ = std::make_unique<filesystem::path>(filesystem::current_path() /
                                                    filesystem::path("benchmark-tmp-dir"));

    if(filesystem::exists(*directory_))
      serialbox::remove_all(*directory_);

    hasError = !filesystem::create_directories(*directory_);

    LOG(info) << "Creating unittest directory: " << directory_->string();
  } catch(filesystem::filesystem_error& e) {
    LOG(warning) << "unresolved filesystem::filesystem_error: " << e.what();
    hasError = true;
    errStr += e.what();
  }

  // If we encounterd an error we give up
  if(hasError) {
    std::cerr << "serialbox::BenchmarkEnvironment: failed to create benchmark directory: "
              << errStr;
    std::abort();
  }

  // Setup sizes
  sizes_ = {{{1024}}, {{512, 2}}, {{128, 128, 4}}};
}

void BenchmarkEnvironment::TearDown() {
  reportResults();
  try {
    auto numFiles = serialbox::remove_all(*directory_);
    LOG(info) << "Removed " << numFiles << " files";
  } catch(filesystem::filesystem_error& e) {
    LOG(warning) << e.what();
  }
}

void BenchmarkEnvironment::reportResults() const {
  const int TerminalLength = 78;

  std::string banner(TerminalLength, '-');
  std::cout << banner << "\n";

  if(results_.empty())
    std::cout << "    NO BENCHMARKS REPORTED\n";
  else
    for(const auto& result : results_) {
      std::cout << (boost::format("  %s\n") % result.name);
      std::cout << "  " << std::string(result.name.size(), '=') << "\n";
      std::string header =
          (boost::format("%-15s %-20s %15s") % "Mode" % "Size" % "Time (ms)").str();
      std::cout << "  " << header << "\n";
      std::cout << "  " << std::string(header.size(), '-') << "\n";

      for(const auto& timingPair : result.timingsWrite)
        std::cout << (boost::format("  %-15s %-20s %15.5f\n") % "Writing" %
                      timingPair.first.toString() % timingPair.second);

      for(const auto& timingPair : result.timingsRead)
        std::cout << (boost::format("  %-15s %-20s %15.5f\n") % "Reading" %
                      timingPair.first.toString() % timingPair.second);
      std::cout << "\n";
    }

  std::cout << banner << std::endl;
}

std::string BenchmarkEnvironment::testCaseName() const {
  const ::testing::TestInfo* testInfo = ::testing::UnitTest::GetInstance()->current_test_info();
  if(testInfo)
    return testInfo->test_case_name();
  return "UnknownTestCase";
}

std::string BenchmarkEnvironment::testName() const {
  const ::testing::TestInfo* testInfo = ::testing::UnitTest::GetInstance()->current_test_info();
  if(testInfo)
    return testInfo->name();
  return "UnknownTest";
}

} // namespace unittest

} // namespace serialbox
