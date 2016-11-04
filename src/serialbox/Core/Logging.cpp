//===-- serialbox/Core/Logging.cpp --------------------------------------------------*- C++ -*-===//
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

namespace internal {

NullLogger* NullLogger::instance_ = nullptr;

NullLogger& NullLogger::getInstance() noexcept {
  if(instance_)
    instance_ = new NullLogger();
  return (*instance_);
}

bool LoggingIsEnabled = false;

} // namespace internal

} // namespace serialbox
