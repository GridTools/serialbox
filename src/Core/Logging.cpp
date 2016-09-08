//===-- Core/Logging.cpp ------------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the logging infrastructure.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Logging.h"

namespace serialbox {

#ifndef SERIALBOX_DISABLE_LOGGING

void Logging::init(const char* programName, bool useSignalHandler) noexcept {
  if(useSignalHandler)
    google::InstallFailureSignalHandler();

  google::InitGoogleLogging(programName);
}

void Logging::setLogToStderr(bool value) noexcept { FLAGS_logtostderr = value; }

void Logging::setAlsoLogToStderr(bool value) noexcept { FLAGS_alsologtostderr = value; }

void Logging::setStdErrThreshold(SeverityKind value) noexcept {
  FLAGS_stderrthreshold = static_cast<int>(value);
}

void Logging::setMinLogLevel(SeverityKind value) noexcept {
  FLAGS_minloglevel = static_cast<int>(value);
}

void Logging::setColorLogToStdErr(bool value) noexcept { FLAGS_colorlogtostderr = value; }

void Logging::setLogDir(const char* value) noexcept { FLAGS_log_dir = value; }

#else // SERIALBOX_DISABLE_LOGGING

void Logging::init(const char* programName, bool useSignalHandler) noexcept {}

void Logging::setLogToStderr(bool value) noexcept {}

void Logging::setAlsoLogToStderr(bool value) noexcept {}

void Logging::setStdErrThreshold(SeverityKind value) noexcept {}

void Logging::setMinLogLevel(SeverityKind value) noexcept {}

void Logging::setColorLogToStdErr(bool value) noexcept {}

void Logging::setLogDir(const char* value) noexcept {}

namespace internal {

NullLogger* NullLogger::instance_ = nullptr;

NullLogger& NullLogger::getInstance() noexcept {
  if(instance_)
    instance_ = new NullLogger();
  return (*instance_);
}

} // namespace internal

#endif

} // namespace serialbox
