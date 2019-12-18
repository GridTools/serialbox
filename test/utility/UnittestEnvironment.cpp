//===-- utility/UnittestEnvironment.cpp ---------------------------------------------*- C++ -*-===//
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

#include "utility/UnittestEnvironment.h"
#include "serialbox/core/Exception.h"
#include "serialbox/core/Logging.h"
#include "serialbox/core/STLExtras.h"

namespace serialbox {

namespace unittest {

//#define SERIALBOX_UNITTEST_NO_CLEANUP 1

UnittestEnvironment* UnittestEnvironment::instance_ = nullptr;

UnittestEnvironment& UnittestEnvironment::getInstance() noexcept {
  if(!instance_)
#ifndef SERIALBOX_UNITTEST_NO_CLEANUP
    instance_ = new UnittestEnvironment(true);
#else
    instance_ = new UnittestEnvironment(false);
#endif

  return (*instance_);
}

void UnittestEnvironment::SetUp() {
  bool hasError = false;
  std::string errStr;

  // Try to create a path to run our unittests in the form "$(pwd)/unittest-tmp-dir/"
  try {
    directory_ = std::make_unique<filesystem::path>(filesystem::current_path() /
                                                    filesystem::path("unittest-tmp-dir"));

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
    std::cerr << "serialbox::UnittestEnvironment: failed to create unittest directory: " << errStr;
    std::abort();
  }
}

void UnittestEnvironment::TearDown() {
  // Try to cleanup
  try {
    if(cleanup_) {
      auto numFiles = serialbox::remove_all(*directory_);
      LOG(info) << "Removed " << numFiles << " files";
    }
  } catch(filesystem::filesystem_error& e) {
    LOG(warning) << e.what();
  }
}

std::string UnittestEnvironment::testCaseName() const {
  const ::testing::TestInfo* testInfo = ::testing::UnitTest::GetInstance()->current_test_info();
  if(testInfo)
    return testInfo->test_case_name();
  return "UnknownTestCase";
}

std::string UnittestEnvironment::testName() const {
  const ::testing::TestInfo* testInfo = ::testing::UnitTest::GetInstance()->current_test_info();
  if(testInfo)
    return testInfo->name();
  return "UnknownTest";
}

} // namespace unittest

} // namespace serialbox
