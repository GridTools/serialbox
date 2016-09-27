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

#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Type.h"
#include <boost/any.hpp>
#include <type_traits>

namespace serialbox {

/// \brief Represent a meta information value as a type-id and type-erased data
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
  /// \param  value      Value to caputre
  template <
      class ValueType, class DecayedValueType = typename std::decay<ValueType>::type,
      class = typename std::enable_if<!std::is_same<DecayedValueType, MetaInfoValue>::value>::type>
  explicit MetaInfoValue(ValueType&& value) {
    static_assert(isSupported<DecayedValueType>::value, "ValueType is not supported");

    type_ = ToTypeID<DecayedValueType>::value;
    any_ = boost::any(DecayedValueType(value));
  }
  explicit MetaInfoValue(const char* value) : MetaInfoValue(std::string(value)) {}

  /// \brief Convert value to type T
  ///
  /// \throws Exception  TypeID of type T does not match TypeID of the captured value
  template <class T>
  T& as() {
    if(ToTypeID<T>::value != type_)
      throw Exception("cannot convert [type = %s] to [T = %s]", TypeUtil::toString(type_),
                      TypeUtil::toString(ToTypeID<T>::value));
    return (*boost::any_cast<T>(&any_));
  }

  template <class T>
  const T& as() const {
    if(ToTypeID<T>::value != type_)
      throw Exception("cannot convert [type = %s] to [T = %s]", TypeUtil::toString(type_),
                      TypeUtil::toString(ToTypeID<T>::value));
    return (*boost::any_cast<T>(&any_));
  }
  
  /// \brief Implicitly convert value to type T
  ///
  /// \throws Exception  TypeID of type T does not match TypeID of the captured value
  template <class T>
  operator T() const {
    return as<T>();
  }

  template <class T>
  operator T() {
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
  std::string toString() const noexcept;

private:
  TypeID type_;    ///< Type of the data
  boost::any any_; ///< Type-erased value of the data
};

} // namespace serialbox

#endif
