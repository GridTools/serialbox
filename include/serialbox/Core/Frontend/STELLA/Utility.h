//===-- serialbox/Core/Frontend/STELLA/Utility.h ------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains some utilities internally used by the STELLA frontend.
///
/// This file contains C++11 and should therefore not be included in exposed headers.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_UTILITY_H
#define SERIALBOX_CORE_FRONTEND_STELLA_UTILITY_H

#include "serialbox/Core/Frontend/STELLA/SerializationException.h"
#include <boost/format.hpp>

namespace serialbox {

namespace stella {

namespace internal {

/// \brief Throw SerializationException constructed with printf-style arguments
template <typename... Args>
void throwSerializationException(const char* fmt, Args&&... args) throw(SerializationException) {
  boost::format f(fmt);

  int unroll[]{0, (f % std::forward<Args>(args), 0)...};
  static_cast<void>(unroll);

  SerializationException exception;
  exception.Init(boost::str(f));
  throw exception;
}

} // namespace internal

} // namespace stella

} // namespace serialbox

#endif
