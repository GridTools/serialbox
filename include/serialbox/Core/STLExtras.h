//===-- serialbox/Core/STLExtras.h --------------------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_STLEXTRAS_H
#define SERIALBOX_CORE_STLEXTRAS_H

#include "serialbox/Core/Compiler.h"
#include <cstddef>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

namespace serialbox {

//===------------------------------------------------------------------------------------------===//
//     Extra additions to <memory>
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_COMPILER_MSVC

// Implement make_unique according to N3656.

/// \brief Constructs a `new T()` with the given args and returns a `unique_ptr<T>` which owns the
/// object
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
/// the object
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

/// \brief This function isn't used and is only here to provide better compile errors
template <class T, class... Args>
typename std::enable_if<std::extent<T>::value != 0>::type make_unique(Args&&...) = delete;

#else
using std::make_unique;
#endif

} // namespace serialbox

#endif
