//===-- serialbox/core/Array.h ------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file defines the Array class of serialbox.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_ARRAY_H
#define SERIALBOX_CORE_ARRAY_H

#include <sstream>
#include <string>
#include <type_traits>
#include <vector>

namespace serialbox {

/// \addtogroup core
/// @{

/// \typedef Array
/// \brief Array class used by serialbox to store meta-information
template <class T>
using Array = std::vector<T>;

/// \brief Utilites for Array
struct ArrayUtil {

  /// \brief Convert to string
  template <class T>
  static std::string toString(const Array<T>& array) {
    std::stringstream ss;
    if(!array.empty()) {
      for(std::size_t i = 0; i < array.size() - 1; ++i)
        ss << array[i] << ", ";
      ss << array.back();
    }
    return ss.str();
  }
};

/// \brief Check if type `T` is an Array
/// @{
template <typename T>
struct IsArray : public std::false_type {};

template <typename T>
struct IsArray<Array<T>> : public std::true_type {};
/// @}

namespace internal {

template <class T, bool IsArray>
struct MakePrimitiveImpl {
  using type = typename T::value_type;
};

template <class T>
struct MakePrimitiveImpl<T, false> {
  using type = T;
};

} // namespace internal

/// \brief Return the primtive type (`T::value_type`) if `T` is an Array or `T` otherwise
template <class T>
struct MakePrimitive {
  using type = typename internal::MakePrimitiveImpl<T, IsArray<T>::value>::type;
};

/// @}

} // namespace serialbox

#endif
