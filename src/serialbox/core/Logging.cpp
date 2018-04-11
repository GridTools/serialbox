//===-- serialbox/core/Logging.cpp --------------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/Logging.h"
#include <boost/format.hpp>
#include <chrono>

namespace serialbox {

namespace internal {

/// Get current date-time (up to ms accuracy)
static std::string getCurrentTime() {
  std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
  auto now_ms = now.time_since_epoch();
  auto now_sec = std::chrono::duration_cast<std::chrono::seconds>(now_ms);
  auto tm_ms = std::chrono::duration_cast<std::chrono::milliseconds>(now_ms - now_sec);

  std::time_t currentTime = std::chrono::system_clock::to_time_t(now);
  struct tm* localTime = std::localtime(&currentTime);

  return (boost::format("%02i:%02i:%02i.%03i") % localTime->tm_hour % localTime->tm_min %
          localTime->tm_sec % tm_ms.count())
      .str();
}

NullLogger* NullLogger::instance_ = nullptr;

NullLogger& NullLogger::getInstance() noexcept {
  if(!instance_)
    instance_ = new NullLogger();
  return (*instance_);
}

Logger* Logger::instance_ = nullptr;

Logger& Logger::getInstance() noexcept {
  if(!instance_)
    instance_ = new Logger();
  Logging::enable();
  return (*instance_);
}

LoggerProxy::~LoggerProxy() { std::cout << std::endl; }

LoggerProxy Logger::trace() noexcept {
  std::cout << "[" << getCurrentTime() << "] [trace] ";
  return LoggerProxy();
}

LoggerProxy Logger::debug() noexcept {
  std::cout << "[" << getCurrentTime() << "] [debug] ";
  return LoggerProxy();
}

LoggerProxy Logger::info() noexcept {
  std::cout << "[" << getCurrentTime() << "] [info] ";
  return LoggerProxy();
}

LoggerProxy Logger::warning() noexcept {
  std::cout << "[" << getCurrentTime() << "] [warning] ";
  return LoggerProxy();
}

LoggerProxy Logger::error() noexcept {
  std::cout << "[" << getCurrentTime() << "] [error] ";
  return LoggerProxy();
}

LoggerProxy Logger::fatal() noexcept {
  std::cout << "[" << getCurrentTime() << "] [fatal] ";
  return LoggerProxy();
}

bool LoggingIsEnabled = false;

} // namespace internal

} // namespace serialbox
