//===-- serialbox/core/frontend/stella/SerializationException.h ---------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains helper functions to create StorageViews of STELLA fields.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_SERIALIZATIONEXCEPTION_H
#define SERIALBOX_CORE_FRONTEND_STELLA_SERIALIZATIONEXCEPTION_H

#include "serialbox/core/frontend/stella/ForwardDecl.h"
#include <exception>
#include <string>

#ifndef SERIALBOX_NOEXCEPT
#if __cplusplus >= 201103L
#define SERIALBOX_NOEXCEPT noexcept
#else
#define SERIALBOX_NOEXCEPT throw()
#endif
#endif

namespace serialbox {

namespace stella {

/// \brief Exception of the STELLA frontend
///
/// \ingroup STELLA
class SerializationException : public std::exception {
public:
  /// \brief Default constructor
  SerializationException() SERIALBOX_NOEXCEPT {}

  /// \brief Virtual destructor
  virtual ~SerializationException() SERIALBOX_NOEXCEPT {}

  /// \brief Initialize the exception with an explanatory string `errormsg`
  void Init(const std::string& errormsg) { message_ = errormsg; }

  /// \brief Returns an explanatory string
  const char* what() const SERIALBOX_NOEXCEPT { return message_.c_str(); }

  /// \brief Returns an explanatory string
  const std::string& message() const { return message_; }

private:
  std::string message_;
};

} // namespace stella

} // namespace serialbox

#endif
