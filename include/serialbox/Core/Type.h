//===-- serialbox/Core/Type.h -------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains definitions for types recognized and used by Serialbox.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_TYPE_H
#define SERIALBOX_CORE_TYPE_H

#include <cstdint>
#include <string>
#include <iosfwd>
#include <boost/mpl/set.hpp>
#include <boost/mpl/assert.hpp>

namespace serialbox {

/// \typedef Byte
/// \brief Represent a byte i.e sizeof(Byte) == 1
using Byte = char;

/// \typedef OpenMode
/// \brief Policy of opening files for Serializer and Archive
enum OpenModeKind : std::uint8_t {
  Read = 0,
  Write,
  Append
};

/// \enum TypeID
/// \brief Type id of types recognized by serialbox
enum class TypeID : std::uint8_t {
  Invalid = 0,
  Boolean,
  Int32,
  Int64,
  Float32,
  Float64,
  String
};

/// \typedef SupportedTypes
/// \brief Types recognized by serialbox
using SupportedTypes = boost::mpl::set<bool, int, std::int64_t, float, double, std::string>;

/// \brief Convert to stream
std::ostream& operator<<(std::ostream& stream, const TypeID& t);

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
struct ToTypeID {
  BOOST_MPL_ASSERT((boost::mpl::has_key<SupportedTypes, T>)); // Unsupported type
};

template <>
struct ToTypeID<bool> {
  static constexpr TypeID value = TypeID::Boolean;
};

template <>
struct ToTypeID<std::int32_t> {
  static constexpr TypeID value = TypeID::Int32;
};

template <>
struct ToTypeID<std::int64_t> {
  static constexpr TypeID value = TypeID::Int64;
};

template <>
struct ToTypeID<float> {
  static constexpr TypeID value = TypeID::Float32;
};

template <>
struct ToTypeID<double> {
  static constexpr TypeID value = TypeID::Float64;
};

template <>
struct ToTypeID<std::string> {
  static constexpr TypeID value = TypeID::String;
};

/// \brief Convert \ref serialbox::Type "TypeID" to C++ type
template <TypeID ID>
struct ToType {};

template <>
struct ToType<TypeID::Boolean> {
  using type = bool;
};

template <>
struct ToType<TypeID::Int32> {
  using type = std::int32_t;
};

template <>
struct ToType<TypeID::Int64> {
  using type = std::int64_t;
};

template <>
struct ToType<TypeID::Float32> {
  using type = float;
};

template <>
struct ToType<TypeID::Float64> {
  using type = double;
};

template <>
struct ToType<TypeID::String> {
  using type = std::string;
};

} // namespace serialbox

#endif
