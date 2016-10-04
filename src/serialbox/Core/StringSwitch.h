//===-- serialbox/Core/StringSwitch.h -----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the StringSwitch template, which mimics a switch() statement whose cases
/// are string literals.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_STRINGSWITCH_H
#define SERIALBOX_CORE_STRINGSWITCH_H

#include "serialbox/Core/Compiler.h"
#include <boost/utility/string_ref.hpp>
#include <cassert>
#include <cstring>

namespace serialbox {

using StringRef = boost::string_ref;

/// \brief A switch()-like statement whose cases are string literals.
///
/// The StringSwitch class is a simple form of a switch() statement that determines whether the
/// given string matches one of the given string literals. The template type parameter \p T is the
/// type of the value that will be returned from the string-switch expression.
///
/// For example, the following code switches on the name of a color in \c argv[i]:
///
/// \code
/// Color color = StringSwitch<Color>(argv[i])
///   .Case("red", Red)
///   .Case("orange", Orange)
///   .Case("yellow", Yellow)
///   .Case("green", Green)
///   .Case("blue", Blue)
///   .Case("indigo", Indigo)
///   .Cases("violet", "purple", Violet)
///   .Default(UnknownColor);
/// \endcode
template <typename T, typename R = T>
class StringSwitch {
public:
  /// \brief Initialize with string ´str´ to switch
  SERIALBOX_ATTRIBUTE_ALWAYS_INLINE
  explicit StringSwitch(StringRef str) : str_(str), result_(nullptr) {}

  /// \brief Move constructor
  StringSwitch(StringSwitch&& other) { *this = std::move(other); }

  /// \brief Move assignment
  StringSwitch& operator=(StringSwitch&& other) {
    str_ = other.str_;
    result_ = other.result_;
    return *this;
  }

  StringSwitch(const StringSwitch&) = delete;
  void operator=(const StringSwitch&) = delete;
  ~StringSwitch() = default;

  template <unsigned N>
  SERIALBOX_ATTRIBUTE_ALWAYS_INLINE StringSwitch& Case(const char (&str)[N],
                                                       const T& value) noexcept {
    if(!result_ && N - 1 == str_.size() && (std::memcmp(str, str_.data(), N - 1) == 0)) {
      result_ = &value;
    }
    return *this;
  }

  template <unsigned N0, unsigned N1>
  SERIALBOX_ATTRIBUTE_ALWAYS_INLINE StringSwitch& Cases(const char (&str0)[N0],
                                                        const char (&str1)[N1], const T& value) {
    return Case(str0, value).Case(str1, value);
  }

  SERIALBOX_ATTRIBUTE_ALWAYS_INLINE
  R Default(const T& value) const noexcept {
    if(result_)
      return *result_;
    return value;
  }

  SERIALBOX_ATTRIBUTE_ALWAYS_INLINE
  operator R() const {
    assert(result_ && "Fell off the end of a string-switch");
    return *result_;
  }

private:
  StringRef str_;
  const T* result_;
};

} // namespace serialbox

#endif
