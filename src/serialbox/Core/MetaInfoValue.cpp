//===-- serialbox/Core/MetaInfoValue.cpp --------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/MetaInfoValue.h"
#include "serialbox/Core/Unreachable.h"
#include <sstream>

namespace serialbox {

namespace internal {

template <class ValueType, class StringType>
static ValueType fromString(StringType&& valueStr) {
  std::stringstream ss;
  ss << valueStr;
  ValueType value;
  ss >> value;
  return value;
}

} // namespace internal

bool MetaInfoValue::operator==(const MetaInfoValue& right) const noexcept {
  if(type_ != right.type_)
    return false;

  switch(type_) {
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
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

std::string MetaInfoValue::toString() const { return as<std::string>(); }

template <>
bool MetaInfoValue::as() const {
  switch(type_) {
  case TypeID::Boolean:
    return convert<bool>();
  case TypeID::Int32:
    return (bool)convert<int>();
  case TypeID::Int64:
    return (bool)convert<std::int64_t>();
  case TypeID::Float32:
    return (bool)convert<float>();
  case TypeID::Float64:
    return (bool)convert<double>();
  case TypeID::String:
    return internal::fromString<bool>(convert<std::string>());
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

template <>
int MetaInfoValue::as() const {
  switch(type_) {
  case TypeID::Boolean:
    return (int)convert<bool>();
  case TypeID::Int32:
    return convert<int>();
  case TypeID::Int64:
    return (int)convert<std::int64_t>();
  case TypeID::Float32: {
    if((float)static_cast<int>(convert<float>()) != convert<float>())
      throw Exception("conversion of [type = %s] to [T = %s] results in truncation of the value",
                      TypeUtil::toString(type_), TypeUtil::toString(ToTypeID<int>::value));
    return (int)convert<float>();
  }
  case TypeID::Float64: {
    if((double)static_cast<int>(convert<double>()) != convert<double>())
      throw Exception("conversion of [type = %s] to [T = %s] results in truncation of the value",
                      TypeUtil::toString(type_), TypeUtil::toString(ToTypeID<int>::value));
    return (int)convert<double>();
  }
  case TypeID::String:
    return internal::fromString<int>(convert<std::string>());
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

template <>
std::int64_t MetaInfoValue::as() const {
  switch(type_) {
  case TypeID::Boolean:
    return (std::int64_t)convert<bool>();
  case TypeID::Int32:
    return (std::int64_t)convert<int>();
  case TypeID::Int64:
    return convert<std::int64_t>();
  case TypeID::Float32: {
    if((float)static_cast<std::int64_t>(convert<float>()) != convert<float>())
      throw Exception("conversion of [type = %s] to [T = %s] results in truncation of the value",
                      TypeUtil::toString(type_), TypeUtil::toString(ToTypeID<std::int64_t>::value));
    return (std::int64_t)convert<float>();
  }
  case TypeID::Float64: {
    if((double)static_cast<std::int64_t>(convert<double>()) != convert<double>())
      throw Exception("conversion of [type = %s] to [T = %s] results in truncation of the value",
                      TypeUtil::toString(type_), TypeUtil::toString(ToTypeID<std::int64_t>::value));
    return (std::int64_t)convert<double>();
  }
  case TypeID::String:
    return internal::fromString<std::int64_t>(convert<std::string>());
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

template <>
float MetaInfoValue::as() const {
  switch(type_) {
  case TypeID::Boolean:
    return (float)convert<bool>();
  case TypeID::Int32:
    return (float)convert<int>();
  case TypeID::Int64:
    return (float)convert<std::int64_t>();
  case TypeID::Float32:
    return convert<float>();
  case TypeID::Float64:
    return (float)convert<double>();
  case TypeID::String:
    return internal::fromString<float>(convert<std::string>());
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

template <>
double MetaInfoValue::as() const {
  switch(type_) {
  case TypeID::Boolean:
    return (double)convert<bool>();
  case TypeID::Int32:
    return (double)convert<int>();
  case TypeID::Int64:
    return (double)convert<std::int64_t>();
  case TypeID::Float32:
    return (double)convert<float>();
  case TypeID::Float64:
    return convert<double>();
  case TypeID::String:
    return internal::fromString<double>(convert<std::string>());
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

template <>
std::string MetaInfoValue::as() const {
  switch(type_) {
  case TypeID::Boolean:
    return (convert<bool>() ? "true" : "false");
  case TypeID::Int32:
    return std::to_string(convert<int>());
  case TypeID::Int64:
    return std::to_string(convert<std::int64_t>());
  case TypeID::Float32:
    return std::to_string(convert<float>());
  case TypeID::Float64:
    return std::to_string(convert<double>());
  case TypeID::String:
    return convert<std::string>();
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

} // namespace serialbox
