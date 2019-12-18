//===-- serialbox/core/Exception.h --------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the Exception class used in serialbox.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_EXCEPTION_H
#define SERIALBOX_CORE_EXCEPTION_H

#include <boost/format.hpp>
#include <stdexcept>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Exception class which stores a human-readable error description
///
/// Creates a `std::runtime_error` with `std::runtime_error::what()` set to the formatted
/// string.
class Exception : public std::runtime_error {
public:
  /// \brief Variadic constructor to support printf-style arguments
  ///
  /// \param fmt   Printf like format string
  /// \param args  Arguments specifying data to print
  template <typename... Args>
  Exception(const char* fmt, Args&&... args)
      : std::runtime_error(formatVargs(fmt, std::forward<Args>(args)...)) {}

private:
  template <typename... Args>
  std::string formatVargs(const std::string& fmt, Args&&... args) {
    boost::format f(fmt);
    int unroll[]{0, (f % std::forward<Args>(args), 0)...};
    static_cast<void>(unroll);
    return boost::str(f);
  }
};

/// @}

} // namespace serialbox

#endif
