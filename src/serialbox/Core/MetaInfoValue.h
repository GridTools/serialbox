//===-- serialbox/Core/MetaInfoValue.h ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the meta-info value.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_METAINFOVALUE_H
#define SERIALBOX_CORE_METAINFOVALUE_H

#include "serialbox/Core/Array.h"
#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Type.h"
#include <boost/any.hpp>
#include <boost/mpl/if.hpp>
#include <type_traits>

namespace serialbox {

/// \brief Represent an immutable meta information value as a type-id and type-erased data
class MetaInfoValue {
public:
  /// \brief Default constructor
  MetaInfoValue() : type_(TypeID::Invalid), any_(){};

  /// \brief Copy constructor
  MetaInfoValue(const MetaInfoValue&) = default;

  /// \brief Move constructor
  MetaInfoValue(MetaInfoValue&&) = default;

  /// \brief Copy assignment
  MetaInfoValue& operator=(const MetaInfoValue&) = default;

  /// \brief Move assignment
  MetaInfoValue& operator=(MetaInfoValue&&) = default;

  /// \brief Construct with given value
  ///
  /// \tparam ValueType  Type of the captured value (needs to be supported)
  /// \param  value      Value to capture
  template <
      class ValueType, class DecayedValueType = typename std::decay<ValueType>::type,
      bool IsArray = isArray<ValueType>::value,
      class PrimitiveType = typename MakePrimitive<DecayedValueType>::type,
      class = typename std::enable_if<!std::is_same<DecayedValueType, MetaInfoValue>::value>::type>
  explicit MetaInfoValue(ValueType&& value) {
    static_assert(isSupported<PrimitiveType>::value, "ValueType is not supported");

    type_ = IsArray ? TypeID((int)ToTypeID<PrimitiveType>::value | (int)TypeID::Array)
                    : ToTypeID<PrimitiveType>::value;
    any_ = boost::any(DecayedValueType(value));
  }
  explicit MetaInfoValue(const char* value) : MetaInfoValue(std::string(value)) {}

  /// \brief Convert the value to type ´T´
  ///
  /// If the type ´T´ is different than the internally stored type, the function does its best to
  /// convert the value to ´T´ in a meaningful way.
  ///
  /// \return Copy of the value of the element as type ´T´
  ///
  /// \throw Exception  Conversion error: Conversion would result in truncation of the value or
  ///                   conversions from primitive to array type
  template <class T>
  T as() const {
    static_assert(isSupported<typename MakePrimitive<T>::type>::value,
                  "ValueType is not supported");
    return T(); // Unreachable
  }

  /// \brief Implicitly convert value to type ´T´
  ///
  /// \see MetaInfoValue::as
  template <class T>
  operator T() const {
    return as<T>();
  }

  /// \brief Swap with other
  void swap(MetaInfoValue& other) noexcept {
    any_.swap(other.any_);
    std::swap(type_, other.type_);
  }

  /// \brief Test for equality
  bool operator==(const MetaInfoValue& right) const noexcept;

  /// \brief Test for inequality
  bool operator!=(const MetaInfoValue& right) const noexcept { return (!(*this == right)); }

  /// \brief Get TypeID
  TypeID type() const noexcept { return type_; }

  /// \brief Get boost::any
  boost::any& any() noexcept { return any_; }
  const boost::any& any() const noexcept { return any_; }

  /// \brief Convert to string
  std::string toString() const;

private:
  template <class T>
  const T& convert() const noexcept {
    return *boost::any_cast<T>(&any_);
  }

private:
  TypeID type_;    ///< Type of the data
  boost::any any_; ///< Type-erased value of the data
};

template <>
bool MetaInfoValue::as() const;

template <>
int MetaInfoValue::as() const;

template <>
std::int64_t MetaInfoValue::as() const;

template <>
float MetaInfoValue::as() const;

template <>
double MetaInfoValue::as() const;

template <>
std::string MetaInfoValue::as() const;

template <>
Array<bool> MetaInfoValue::as() const;

template <>
Array<int> MetaInfoValue::as() const;

template <>
Array<std::int64_t> MetaInfoValue::as() const;

template <>
Array<float> MetaInfoValue::as() const;

template <>
Array<double> MetaInfoValue::as() const;

template <>
Array<std::string> MetaInfoValue::as() const;

} // namespace serialbox

#endif
