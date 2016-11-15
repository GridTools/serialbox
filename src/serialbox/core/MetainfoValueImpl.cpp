//===-- serialbox/core/MetainfoValueImpl.cpp ----------------------------------------*- C++ -*-===//
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

#include "serialbox/core/MetainfoValueImpl.h"
#include "serialbox/core/Unreachable.h"
#include <sstream>

#include <iostream>

namespace serialbox {

namespace internal {

/// Convert any to type T
template <class T>
const T& convert(const boost::any& any) noexcept {
  return *boost::any_cast<T>(&any);
}

/// Construct T from string
template <class T, class StringType>
T fromString(StringType&& valueStr) {
  std::stringstream ss;
  ss << valueStr;
  T value;
  ss >> value;
  return value;
}

/// Convert any to primtive T
template <class T>
T makePrimitiveOf(const boost::any& any, TypeID type);

template <>
bool makePrimitiveOf<bool>(const boost::any& any, TypeID type) {
  switch(type) {
  case TypeID::Boolean:
    return convert<bool>(any);
  case TypeID::Int32:
    return (bool)convert<int>(any);
  case TypeID::Int64:
    return (bool)convert<std::int64_t>(any);
  case TypeID::Float32:
    return (bool)convert<float>(any);
  case TypeID::Float64:
    return (bool)convert<double>(any);
  case TypeID::String:
    return internal::fromString<bool>(convert<std::string>(any));
  default:
    throw Exception("cannot convert [type = %s] to [T = bool]", TypeUtil::toString(type));
  }
  serialbox_unreachable("Invalid TypeID");
}

template <>
int makePrimitiveOf<int>(const boost::any& any, TypeID type) {
  switch(type) {
  case TypeID::Boolean:
    return (int)convert<bool>(any);
  case TypeID::Int32:
    return convert<int>(any);
  case TypeID::Int64:
    return (int)convert<std::int64_t>(any);
  case TypeID::Float32: {
    if((float)static_cast<int>(convert<float>(any)) != convert<float>(any))
      throw Exception("conversion of [type = %s] to [T = %s] results in truncation of the value",
                      TypeUtil::toString(type), TypeUtil::toString(ToTypeID<int>::value));
    return (int)convert<float>(any);
  }
  case TypeID::Float64: {
    if((double)static_cast<int>(convert<double>(any)) != convert<double>(any))
      throw Exception("conversion of [type = %s] to [T = %s] results in truncation of the value",
                      TypeUtil::toString(type), TypeUtil::toString(ToTypeID<int>::value));
    return (int)convert<double>(any);
  }
  case TypeID::String:
    return internal::fromString<int>(convert<std::string>(any));
  default:
    throw Exception("cannot convert [type = %s] to [T = int]", TypeUtil::toString(type));
  }
  serialbox_unreachable("Invalid TypeID");
}

template <>
std::int64_t makePrimitiveOf<std::int64_t>(const boost::any& any, TypeID type) {
  switch(type) {
  case TypeID::Boolean:
    return (std::int64_t)convert<bool>(any);
  case TypeID::Int32:
    return (std::int64_t)convert<int>(any);
  case TypeID::Int64:
    return convert<std::int64_t>(any);
  case TypeID::Float32: {
    if((float)static_cast<std::int64_t>(convert<float>(any)) != convert<float>(any))
      throw Exception("conversion of [type = %s] to [T = %s] results in truncation of the value",
                      TypeUtil::toString(type), TypeUtil::toString(ToTypeID<std::int64_t>::value));
    return (std::int64_t)convert<float>(any);
  }
  case TypeID::Float64: {
    if((double)static_cast<std::int64_t>(convert<double>(any)) != convert<double>(any))
      throw Exception("conversion of [type = %s] to [T = %s] results in truncation of the value",
                      TypeUtil::toString(type), TypeUtil::toString(ToTypeID<std::int64_t>::value));
    return (std::int64_t)convert<double>(any);
  }
  case TypeID::String:
    return internal::fromString<std::int64_t>(convert<std::string>(any));
  default:
    throw Exception("cannot convert [type = %s] to [T = std::int64_t]", TypeUtil::toString(type));
  }
  serialbox_unreachable("Invalid TypeID");
}

template <>
float makePrimitiveOf<float>(const boost::any& any, TypeID type) {
  switch(type) {
  case TypeID::Boolean:
    return (float)convert<bool>(any);
  case TypeID::Int32:
    return (float)convert<int>(any);
  case TypeID::Int64:
    return (float)convert<std::int64_t>(any);
  case TypeID::Float32:
    return convert<float>(any);
  case TypeID::Float64:
    return (float)convert<double>(any);
  case TypeID::String:
    return internal::fromString<float>(convert<std::string>(any));
  default:
    throw Exception("cannot convert [type = %s] to [T = float]", TypeUtil::toString(type));
  }
  serialbox_unreachable("Invalid TypeID");
}

template <>
double makePrimitiveOf<double>(const boost::any& any, TypeID type) {
  switch(type) {
  case TypeID::Boolean:
    return (double)convert<bool>(any);
  case TypeID::Int32:
    return (double)convert<int>(any);
  case TypeID::Int64:
    return (double)convert<std::int64_t>(any);
  case TypeID::Float32:
    return (double)convert<float>(any);
  case TypeID::Float64:
    return convert<double>(any);
  case TypeID::String:
    return internal::fromString<double>(convert<std::string>(any));
  default:
    throw Exception("cannot convert [type = %s] to [T = double]", TypeUtil::toString(type));
  }
  serialbox_unreachable("Invalid TypeID");
}

template <>
std::string makePrimitiveOf<std::string>(const boost::any& any, TypeID type) {
  switch(type) {
  case TypeID::Boolean:
    return (convert<bool>(any) ? "true" : "false");
  case TypeID::Int32:
    return std::to_string(convert<int>(any));
  case TypeID::Int64:
    return std::to_string(convert<std::int64_t>(any));
  case TypeID::Float32:
    return std::to_string(convert<float>(any));
  case TypeID::Float64:
    return std::to_string(convert<double>(any));
  case TypeID::String:
    return convert<std::string>(any);
  default:
    throw Exception("cannot convert [type = %s] to [T = std::string]", TypeUtil::toString(type));
  }
  serialbox_unreachable("Invalid TypeID");
}

/// Convert any to array of T
template <class T, class ArrayType = Array<T>>
ArrayType makeArrayOf(const boost::any& any, TypeID type) {
  if(!TypeUtil::isArray(type))
    throw Exception("cannot convert non-array [type = %s] to array [T = %s]",
                    TypeUtil::toString(type), TypeUtil::toString(ToTypeID<ArrayType>::value));

  ArrayType arrayT;

  switch(TypeUtil::getPrimitive(type)) {
  case TypeID::Boolean: {
    const auto& array = convert<Array<bool>>(any);
    for(const auto& a : array)
      arrayT.push_back(makePrimitiveOf<T>(boost::any(bool(a)), TypeID::Boolean));
    break;
  }
  case TypeID::Int32: {
    const auto& array = convert<Array<int>>(any);
    for(const auto& a : array)
      arrayT.push_back(makePrimitiveOf<T>(boost::any(int(a)), TypeID::Int32));
    break;
  }
  case TypeID::Int64: {
    const auto& array = convert<Array<std::int64_t>>(any);
    for(const auto& a : array)
      arrayT.push_back(makePrimitiveOf<T>(boost::any(std::int64_t(a)), TypeID::Int64));
    break;
  }
  case TypeID::Float32: {
    const auto& array = convert<Array<float>>(any);
    for(const auto& a : array)
      arrayT.push_back(makePrimitiveOf<T>(boost::any(float(a)), TypeID::Float32));
    break;
  }
  case TypeID::Float64: {
    const auto& array = convert<Array<double>>(any);
    for(const auto& a : array)
      arrayT.push_back(makePrimitiveOf<T>(boost::any(double(a)), TypeID::Float64));
    break;
  }
  case TypeID::String: {
    const auto& array = convert<Array<std::string>>(any);
    for(const auto& a : array)
      arrayT.push_back(makePrimitiveOf<T>(boost::any(std::string(a)), TypeID::String));
    break;
  }
  default:
    serialbox_unreachable("Invalid TypeID");
  }

  return arrayT;
}

} // namespace internal

bool MetainfoValueImpl::operator==(const MetainfoValueImpl& right) const noexcept {
  if(type_ != right.type_)
    return false;

  switch(type_) {

  // Primitive
  case TypeID::Boolean:
    return (convert<bool>() == right.convert<bool>());
  case TypeID::Int32:
    return (convert<int>() == right.convert<int>());
  case TypeID::Int64:
    return (convert<std::int64_t>() == right.convert<std::int64_t>());
  case TypeID::Float32:
    return (convert<float>() == right.convert<float>());
  case TypeID::Float64:
    return (convert<double>() == right.convert<double>());
  case TypeID::String:
    return (convert<std::string>() == right.convert<std::string>());

  // Array
  case TypeID::ArrayOfBoolean:
    return (convert<Array<bool>>() == right.convert<Array<bool>>());
  case TypeID::ArrayOfInt32:
    return (convert<Array<int>>() == right.convert<Array<int>>());
  case TypeID::ArrayOfInt64:
    return (convert<Array<std::int64_t>>() == right.convert<Array<std::int64_t>>());
  case TypeID::ArrayOfFloat32:
    return (convert<Array<float>>() == right.convert<Array<float>>());
  case TypeID::ArrayOfFloat64:
    return (convert<Array<double>>() == right.convert<Array<double>>());
  case TypeID::ArrayOfString:
    return (convert<Array<std::string>>() == right.convert<Array<std::string>>());

  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

std::string MetainfoValueImpl::toString() const { return as<std::string>(); }

template <>
bool MetainfoValueImpl::as() const {
  return internal::makePrimitiveOf<bool>(any_, type_);
}

template <>
int MetainfoValueImpl::as() const {
  return internal::makePrimitiveOf<int>(any_, type_);
}

template <>
std::int64_t MetainfoValueImpl::as() const {
  return internal::makePrimitiveOf<std::int64_t>(any_, type_);
}

template <>
float MetainfoValueImpl::as() const {
  return internal::makePrimitiveOf<float>(any_, type_);
}

template <>
double MetainfoValueImpl::as() const {
  return internal::makePrimitiveOf<double>(any_, type_);
}

template <>
std::string MetainfoValueImpl::as() const {
  return internal::makePrimitiveOf<std::string>(any_, type_);
}

template <>
Array<bool> MetainfoValueImpl::as() const {
  return internal::makeArrayOf<bool>(any_, type_);
}

template <>
Array<int> MetainfoValueImpl::as() const {
  return internal::makeArrayOf<int>(any_, type_);
}

template <>
Array<std::int64_t> MetainfoValueImpl::as() const {
  return internal::makeArrayOf<std::int64_t>(any_, type_);
}

template <>
Array<float> MetainfoValueImpl::as() const {
  return internal::makeArrayOf<float>(any_, type_);
}

template <>
Array<double> MetainfoValueImpl::as() const {
  return internal::makeArrayOf<double>(any_, type_);
}

template <>
Array<std::string> MetainfoValueImpl::as() const {
  return internal::makeArrayOf<std::string>(any_, type_);
}

} // namespace serialbox
