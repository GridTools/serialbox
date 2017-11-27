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
/// This file contains the logging infrastructure (based on Boost.Log).
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_LOGGING_H
#define SERIALBOX_CORE_LOGGING_H

#ifndef SERIALBOX_DISABLE_LOGGING
// TODO reintroduce a proper logger
//#include <boost/log/trivial.hpp>
#endif

namespace serialbox {

/// \addtogroup core
/// @{

namespace internal {

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

extern bool LoggingIsEnabled;

} // namespace internal

/// \brief Control the logging behaviour
///
/// For logging use the macro LOG(severity)
class Logging {
  Logging();

public:
  /// \brief Disable logging
  static void enable() noexcept { internal::LoggingIsEnabled = true; }

  /// \brief Enable logging
  static void disable() noexcept { internal::LoggingIsEnabled = false; }

  /// \brief Return true if logging is eneabled
  static bool isEnabled() noexcept { return internal::LoggingIsEnabled; }
};

/// \macro LOG
/// \brief Logging infrastructure based on trivial logger of Boost.Log
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
/// \see
///   http://www.boost.org/doc/libs/1_62_0/libs/log/doc/html/index.html
///
#define LOG(severity) SERIALBOX_INTERNAL_LOG(severity)

// TODO reintroduce a logger
//#ifdef SERIALBOX_DISABLE_LOGGING

#define SERIALBOX_INTERNAL_LOG(severity)                                                           \
  while(0)                                                                                         \
  serialbox::internal::NullLogger::getInstance()

//#else
//#define SERIALBOX_INTERNAL_LOG(severity)
//  if(serialbox::Logging::isEnabled())
//  BOOST_LOG_TRIVIAL(severity)
//#endif

/// @}

} // namespace serialbox

#endif
