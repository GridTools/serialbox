//===-- serialbox/Core/Logging.h ----------------------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_LOGGING_H
#define SERIALBOX_CORE_LOGGING_H

#ifndef SERIALBOX_DISABLE_LOGGING
#include <glog/logging.h>
#endif

namespace serialbox {

/// \brief Logging infrastructure based on glog
///
/// The logging can be completely turned off by defining SERIALBOX_DISABLE_LOGGING.
///
/// \see http://rpg.ifi.uzh.ch/docs/glog.html
class Logging {
public:
  /// \brief Severity levels in increasing order of severity
  enum SeverityKind { Info = 0, Warning, Error, Fatal };

  /// \brief Initialize glog
  ///
  /// \brief programName        Name of the program used to prefix the logs
  /// \brief useSignalHandler   Install a signal handler that will dump useful information when the
  ///                           program crashes on certain signals such as SIGSEGV
  static void init(const char* programName = "serialbox", bool useSignalHandler = true) noexcept;

  /// @{ Logging Options

  /// \brief Log messages to stderr instead of logfiles [default: false]
  static void setLogToStderr(bool value) noexcept;

  /// \brief Additionally log messages to stderr [default: false]
  static void setAlsoLogToStderr(bool value) noexcept;

  /// \brief Copy log messages at or above this level to stderr in addition to ogfiles [default:
  /// SeverityKind::Error]
  static void setStdErrThreshold(SeverityKind value) noexcept;

  /// \brief Log messages at or above this level [default: SeverityKind::Info]
  static void setMinLogLevel(SeverityKind value) noexcept;

  /// \brief Use colored output when logging messages to stderr [default: false]
  static void setColorLogToStdErr(bool value) noexcept;

  /// \brief Logfiles are written into this directory instead of the default logging directory
  /// [default: /tmp/]
  static void setLogDir(const char* value) noexcept;

  /// @}
};

#ifdef SERIALBOX_DISABLE_LOGGING

namespace internal {

/// Mock glog logger
class NullLogger {
public:
  NullLogger() = default;

  static NullLogger& getInstance() noexcept;

  template <class T>
  NullLogger& operator<<(T&& t) noexcept {
    return (*this);
  }

private:
  static NullLogger* instance_;
};

} // namespace internal

//===------------------------------------------------------------------------------------------===//
//     Mock GLOG macros
//===------------------------------------------------------------------------------------------===//

#define LOG(SEVERITY) serialbox::internal::NullLogger::getInstance()

// Conditional / Occasional Logging
#define LOG_IF(SEVERITY, CONDITION) serialbox::internal::NullLogger::getInstance()
#define LOG_EVERY_N(SEVERITY, CONDITION) serialbox::internal::NullLogger::getInstance()
#define LOG_IF_EVERY_N(SEVERITY, CONDITION, N) serialbox::internal::NullLogger::getInstance()
#define LOG_FIRST_N(SEVERITY, N) serialbox::internal::NullLogger::getInstance()

// Debug Mode Support
#define DLOG(SEVERITY) serialbox::internal::NullLogger::getInstance()
#define DLOG_IF(SEVERITY, CONDITION) serialbox::internal::NullLogger::getInstance()
#define DLOG_EVERY_N(SEVERITY, N) serialbox::internal::NullLogger::getInstance()

// CHECK Macros
#define CHECK(CONDITION) serialbox::internal::NullLogger::getInstance()
#define CHECK_NE(A, B) serialbox::internal::NullLogger::getInstance()
#define CHECK_EQ(A, B) serialbox::internal::NullLogger::getInstance()
#define CHECK_NOTNULL(PTR) PTR

//  Verbose Logging
#define VLOG(N) serialbox::internal::NullLogger::getInstance()
#define VLOG_IS_ON(ARG) false

#endif // SERIALBOX_DISABLE_LOGGING

} // namespace serialbox

#endif
