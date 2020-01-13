//===-- serialbox/core/Logging.h ----------------------------------------------------*- C++ -*-===//
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

#include <iostream>

namespace serialbox {

/// \addtogroup core
/// @{

namespace internal {

class NullLogger {
public:
  static NullLogger& getInstance() noexcept;

  template <class T>
  NullLogger& operator<<(T&& t) noexcept {
    return (*this);
  }

private:
  static NullLogger* instance_;
};

class LoggerProxy {
public:
  /// @brief Unlock the mutex and flush stdout
  ~LoggerProxy();

  template <class StreamableValueType>
  LoggerProxy& operator<<(StreamableValueType&& value) {
    std::cout << value;
    return *this;
  }
};

class Logger {
public:
  Logger() = default;

  static Logger& getInstance() noexcept;

  LoggerProxy trace() noexcept;
  LoggerProxy debug() noexcept;
  LoggerProxy info() noexcept;
  LoggerProxy warning() noexcept;
  LoggerProxy error() noexcept;
  LoggerProxy fatal() noexcept;

private:
  static Logger* instance_;
};

extern bool LoggingIsEnabled;

} // namespace internal

/// \brief Control the logging behaviour
///
/// For logging use the macro LOG(severity)
class Logging {
public:
  Logging() = delete;

  /// \brief Disable logging
  static void enable() noexcept { internal::LoggingIsEnabled = true; }

  /// \brief Enable logging
  static void disable() noexcept { internal::LoggingIsEnabled = false; }

  /// \brief Return true if logging is eneabled
  static bool isEnabled() noexcept { return internal::LoggingIsEnabled; }
};

/// \macro LOG
/// \brief Logging infrastructure
///
/// The macro is used to initiate logging. The `lvl` argument of the macro specifies one of the
/// following severity levels: `trace`, `debug`, `info`, `warning`, `error` or `fatal`.
///
/// The logging can be completely turned off by defining `SERIALBOX_DISABLE_LOGGING`.
///
/// \code
///   LOG(info) << "Hello, world!";
/// \endcode
///
#define LOG(severity) SERIALBOX_INTERNAL_LOG(severity)

#ifdef SERIALBOX_DISABLE_LOGGING

#define SERIALBOX_INTERNAL_LOG(severity)                                                           \
  while(0)                                                                                         \
  serialbox::internal::NullLogger::getInstance()

#else

#define SERIALBOX_INTERNAL_LOG(severity)                                                           \
  if(serialbox::Logging::isEnabled())                                                              \
  serialbox::internal::Logger::getInstance().severity()

#endif

/// @}

} // namespace serialbox

#endif
