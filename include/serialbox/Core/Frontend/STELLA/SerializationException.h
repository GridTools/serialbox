//===-- serialbox/Core/Frontend/STELLA/SerializationException.h ---------------------*- C++ -*-===//
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

#include "serialbox/Core/Frontend/STELLA/ForwardDecl.h"
#include <exception>
#include <string>

namespace serialbox {

namespace stella {

/// \brief Exception of the STELLA frontend
class SerializationException : public std::exception {
public:
  /// \brief Default constructor
  SerializationException() throw() {}

  /// \brief Virtual destructor
  virtual ~SerializationException() throw() {}

  /// \brief Initialize the exception with an explanatory string ´errormsg´
  void Init(const std::string& errormsg) { message_ = errormsg; }

  /// \brief Returns an explanatory string
  const char* what() const throw() { return message_.c_str(); }

  /// \brief Returns an explanatory string
  const std::string& message() const { return message_; }

private:
  std::string message_;
};

} // namespace stella

} // namespace serialbox

#endif
