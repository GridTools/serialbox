//===-- serialbox/core/MetainfoValueImpl.h ------------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_METAINFOVALUEIMPL_H
#define SERIALBOX_CORE_METAINFOVALUEIMPL_H

#include "serialbox/core/Array.h"
#include "serialbox/core/Exception.h"
#include "serialbox/core/Type.h"
#include <boost/any.hpp>
#include <boost/mpl/if.hpp>
#include <type_traits>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Represent an immutable meta information value as a type-id and type-erased data
///
/// The MetainfoValueImpl can be implicitly casted to the supported types.
class MetainfoValueImpl {
public:
  /// \brief Default constructor
  MetainfoValueImpl() : type_(TypeID::Invalid), any_(){};

  /// \brief Copy constructor
  MetainfoValueImpl(const MetainfoValueImpl&) = default;

  /// \brief Move constructor
  MetainfoValueImpl(MetainfoValueImpl&&) = default;

  /// \brief Copy assignment
  MetainfoValueImpl& operator=(const MetainfoValueImpl&) = default;

  /// \brief Move assignment
  MetainfoValueImpl& operator=(MetainfoValueImpl&&) = default;

  /// \brief Construct with given value
  ///
  /// \tparam ValueType  Type of the captured value (needs to be supported)
  /// \param  value      Value to capture
  template <class ValueType, class DecayedValueType = typename std::decay<ValueType>::type,
            class PrimitiveType = typename MakePrimitive<DecayedValueType>::type,
            class = typename std::enable_if<
                !std::is_same<DecayedValueType, MetainfoValueImpl>::value>::type>
  explicit MetainfoValueImpl(ValueType&& value) {
    static_assert(IsSupported<PrimitiveType>::value, "ValueType is not supported");

    type_ = ToTypeID<DecayedValueType>::value;
    any_ = boost::any(DecayedValueType(value));
  }
  explicit MetainfoValueImpl(const char* value) : MetainfoValueImpl(std::string(value)) {}

  /// \brief Convert the value to type `T`
  ///
  /// If the type `T` is different than the internally stored type, the function does its best to
  /// convert the value to `T` in a meaningful way.
  ///
  /// \return Copy of the value of the element as type `T`
  ///
  /// \throw Exception  Conversion error: Conversion would result in truncation of the value or
  ///                   conversions from primitive to array type
  template <class T>
  T as() const {
    static_assert(IsSupported<typename MakePrimitive<T>::type>::value,
                  "ValueType is not supported");
    return T(); // Unreachable
  }

  /// \brief Implicitly convert value to type `T`
  ///
  /// \see MetainfoValueImpl::as
  template <class T>
  operator T() const {
    return as<T>();
  }

  /// \brief Swap with other
  void swap(MetainfoValueImpl& other) noexcept {
    any_.swap(other.any_);
    std::swap(type_, other.type_);
  }

  /// \brief Test for equality
  bool operator==(const MetainfoValueImpl& right) const noexcept;

  /// \brief Test for inequality
  bool operator!=(const MetainfoValueImpl& right) const noexcept { return (!(*this == right)); }

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
bool MetainfoValueImpl::as() const;

template <>
int MetainfoValueImpl::as() const;

template <>
std::int64_t MetainfoValueImpl::as() const;

template <>
float MetainfoValueImpl::as() const;

template <>
double MetainfoValueImpl::as() const;

template <>
std::string MetainfoValueImpl::as() const;

template <>
Array<bool> MetainfoValueImpl::as() const;

template <>
Array<int> MetainfoValueImpl::as() const;

template <>
Array<std::int64_t> MetainfoValueImpl::as() const;

template <>
Array<float> MetainfoValueImpl::as() const;

template <>
Array<double> MetainfoValueImpl::as() const;

template <>
Array<std::string> MetainfoValueImpl::as() const;

/// @}

} // namespace serialbox

#endif
