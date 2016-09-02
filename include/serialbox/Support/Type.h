//===-- serialbox/Support/Type.h ----------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains definitions for types recognized by Serialbox.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_SUPPORT_TYPE_H
#define SERIALBOX_SUPPORT_TYPE_H

#include <cstdint>
#include <string>

namespace serialbox {

/// \enum TypeID
/// \brief Type id of types recognized by serialbox
enum class TypeID : std::uint8_t { Invalid = 0, Boolean, Int32, Int64, Float32, Float64 };

/// \brief Utilites for TypeID
struct TypeUtil {

  /// \brief Convert to string
  static std::string toString(TypeID id);

  /// \brief Get size of the type
  static int sizeOf(TypeID id) noexcept;
};

//===------------------------------------------------------------------------------------------===//
//     Compile time conversion
//===------------------------------------------------------------------------------------------===//

/// \brief Convert C++ type \c T to \ref serialbox::TypeID "TypeID"
template <class T>
struct toTypeID {};

template <>
struct toTypeID<bool> {
  static constexpr TypeID value = TypeID::Boolean;
};

template <>
struct toTypeID<std::int32_t> {
  static constexpr TypeID value = TypeID::Int32;
};

template <>
struct toTypeID<std::int64_t> {
  static constexpr TypeID value = TypeID::Int64;
};

template <>
struct toTypeID<float> {
  static constexpr TypeID value = TypeID::Float32;
};

template <>
struct toTypeID<double> {
  static constexpr TypeID value = TypeID::Float64;
};

/// \brief Convert \ref serialbox::Type "TypeID" to C++ type
template <TypeID ID>
struct toType {};

template <>
struct toType<TypeID::Boolean> {
  using type = bool;
};

template <>
struct toType<TypeID::Int32> {
  using type = std::int32_t;
};

template <>
struct toType<TypeID::Int64> {
  using type = std::int64_t;
};

template <>
struct toType<TypeID::Float32> {
  using type = float;
};

template <>
struct toType<TypeID::Float64> {
  using type = double;
};

} // namespace serialbox

#endif
