//===-- serialbox/Support/STLExtras.h -----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Extensions to the STL.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_SUPPORT_STLEXTRAS_H
#define SERIALBOX_SUPPORT_STLEXTRAS_H

#include <cstddef>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

namespace serialbox {

//===------------------------------------------------------------------------------------------===//
//     Extra additions to <memory>
//===------------------------------------------------------------------------------------------===//

// Implement make_unique according to N3656.

/// \brief Constructs a `new T()` with the given args and returns a `unique_ptr<T>` which owns the
/// object.
///
/// \b Example:
/// \code
///   auto p = make_unique<int>();
///   auto p = make_unique<std::tuple<int, int>>(0, 1);
/// \endcode
template <class T, class... Args>
typename std::enable_if<!std::is_array<T>::value, std::unique_ptr<T>>::type
make_unique(Args&&... args) {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

/// \brief Constructs a `new T[n]` with the given args and returns a `unique_ptr<T[]>` which owns
/// the object.
///
/// \param n size of the new array.
///
/// \b Example:
/// \code
///   auto p = make_unique<int[]>(2); // value-initializes the array with 0's.
/// \endcode
template <class T>
typename std::enable_if<std::is_array<T>::value && std::extent<T>::value == 0,
                        std::unique_ptr<T>>::type
make_unique(std::size_t n) {
  return std::unique_ptr<T>(new typename std::remove_extent<T>::type[n]());
}

/// \brief This function isn't used and is only here to provide better compile errors.
template <class T, class... Args>
typename std::enable_if<std::extent<T>::value != 0>::type make_unique(Args&&...) = delete;

//===------------------------------------------------------------------------------------------===//
//     Extra additions to <string>
//===------------------------------------------------------------------------------------------===//

namespace internal {

// ADL for user defined to_string
using std::to_string;

std::string concat() { return ""; }

template <typename Head, typename... Tail>
decltype(to_string(std::declval<Head>())) concat(Head&& h, Tail&&... t);

template <typename Head, typename... Tail>
decltype(std::string() + std::declval<Head>()) concat(Head&& h, Tail&&... t) {
  return std::forward<Head>(h) + concat(std::forward<Tail>(t)...);
}

template <typename Head, typename... Tail>
decltype(to_string(std::declval<Head>())) concat(Head&& h, Tail&&... t) {
  return to_string(std::forward<Head>(h)) + concat(std::forward<Tail>(t)...);
}

} // namespace internal

/// \fn concat
/// \brief Concatenate variadic number of strings
///
/// For a given type, either concatenation (with \c +) \b or \c to_string must be defined, but not
/// both. This means, \c std::string, \c const \c char* will go through the \c + version, while
/// other types will be converted to \c std::string (via \c to_string).
///
/// \b Example:
/// \code
///   std::string str = concat("the", std::string(" answer"), " is ", 42);
/// \endcode
template <typename... Args>
std::string concat(Args&&... args) {
  return internal::concat(std::forward<Args>(args)...);
}

} // namespace serialbox

#endif
