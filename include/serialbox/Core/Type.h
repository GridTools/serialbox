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

#include <boost/mpl/set.hpp>
#include <cstdint>
#include <iosfwd>
#include <string>

namespace serialbox {

/// \typedef Byte
/// \brief Represent a byte i.e sizeof(Byte) == 1
using Byte = char;
static_assert(sizeof(Byte) == 1, "invalid size of Byte");

/// \typedef OpenMode
/// \brief Policy for opening files in the Serializer and Archive
enum OpenModeKind : std::uint8_t { Read = 0, Write, Append };

//===------------------------------------------------------------------------------------------===//
//     Type-id
//===------------------------------------------------------------------------------------------===//

/// \brief Convert TypeID to stream
std::ostream& operator<<(std::ostream& stream, const OpenModeKind& mode);

/// \enum TypeID
/// \brief Type-id of types recognized by serialbox
///
/// \see isSupported
enum class TypeID : std::uint8_t { Invalid = 0, Boolean, Int32, Int64, Float32, Float64, String };

/// \brief Check if the Type is recognized by serialbox i.e maps to a type-id in \ref TypeID
///
/// \b Supported Types:
///  - bool
///  - int (32-bit)
///  - int (64-bit)
///  - float
///  - double
///  - string (std::string)
///
/// \see TypeID
template <class T>
struct isSupported {

  /// \brief Set of supported types
  using SupportedTypesSet = boost::mpl::set<bool, int, std::int64_t, float, double, std::string>;

  /// \brief True iff the type is supported
  constexpr static bool value = boost::mpl::has_key<SupportedTypesSet, T>::type::value;
};

/// \brief Convert TypeID to stream
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
  static_assert(isSupported<T>::value, "type is not supported (cannot be mapped to TypeID)");
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

//===------------------------------------------------------------------------------------------===//
//     Meta-functions
//===------------------------------------------------------------------------------------------===//

namespace internal {

template <typename UnqualifiedType, bool IsConst, bool IsVolatile>
struct cv_selector;

template <typename UnqualifiedType>
struct cv_selector<UnqualifiedType, false, false> {
  using type = UnqualifiedType;
};

template <typename UnqualifiedType>
struct cv_selector<UnqualifiedType, false, true> {
  using type = volatile UnqualifiedType;
};

template <typename UnqualifiedType>
struct cv_selector<UnqualifiedType, true, false> {
  using type = const UnqualifiedType;
};

template <typename UnqualifiedType>
struct cv_selector<UnqualifiedType, true, true> {
  using type = const volatile UnqualifiedType;
};
}

/// \brief Utility for constructing identically cv-qualified types
///
/// \b Example
/// \code
///   static_assert(std::is_same<match_cv_qualifier<const int, float>::type, const float>::value);
/// \endcode
template <typename QualifiedType, typename UnqualifiedType,
          bool IsConst = std::is_const<QualifiedType>::value,
          bool IsVolatile = std::is_volatile<QualifiedType>::value>
class match_cv_qualifier {
  using match = internal::cv_selector<UnqualifiedType, IsConst, IsVolatile>;

public:
  using type = typename match::type;
};

} // namespace serialbox

#endif
