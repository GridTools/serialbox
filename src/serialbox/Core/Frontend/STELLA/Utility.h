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

#include "serialbox/Core/Compiler.h"
#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Frontend/STELLA/SerializationException.h"
#include "serialbox/Core/Type.h"
#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <string>
#include <memory>

namespace serialbox {

namespace stella {

namespace internal {

/// \brief Throw SerializationException constructed with printf-style arguments
template <typename... Args>
SERIALBOX_ATTRIBUTE_NORETURN void throwSerializationException(const char* fmt, Args&&... args) {
  boost::format f(fmt);

  int unroll[]{0, (f % std::forward<Args>(args), 0)...};
  static_cast<void>(unroll);

  SerializationException exception;
  exception.Init(boost::str(f));
  throw exception;
}

template <class StringType>
inline TypeID TypeNameToTypeID(StringType&& name) {
  if(name == "bool")
    return TypeID::Boolean;
  else if(name == "int")
    return TypeID::Int32;
  else if(name == "float")
    return TypeID::Float32;
  else if(name == "double")
    return TypeID::Float64;
  throw Exception("unsupported type: %s", name);
}

inline std::string TypeIDToTypeName(TypeID typeID) {
  switch(typeID) {
  case TypeID::Boolean:
    return "bool";
  case TypeID::Int32:
    return "int";
  case TypeID::Float32:
    return "float";
  case TypeID::Float64:
    return "double";
  default:
    throw Exception("unsupported type: %s", TypeUtil::toString(typeID));
  }
}

template <typename T>
boost::shared_ptr<T> make_shared_ptr(std::shared_ptr<T>& ptr) {
  return boost::shared_ptr<T>(ptr.get(), [ptr](T*) mutable { ptr.reset(); });
}

template <typename T>
std::shared_ptr<T> make_shared_ptr(boost::shared_ptr<T>& ptr) {
  return std::shared_ptr<T>(ptr.get(), [ptr](T*) mutable { ptr.reset(); });
}

} // namespace internal

} // namespace stella

} // namespace serialbox

#endif
