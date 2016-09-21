//===-- Core/MetaInfoValue.cpp ------------------------------------------------------*- C++ -*-===//
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
#include <iostream>

namespace serialbox {

bool MetaInfoValue::operator==(const MetaInfoValue& right) const noexcept {
  if(type_ != right.type_)
    return false;

  switch(type_) {
  case TypeID::Boolean:
    return (*boost::any_cast<bool>(&any_) == *boost::any_cast<bool>(&right.any_));
  case TypeID::Int32:
    return (*boost::any_cast<int>(&any_) == *boost::any_cast<int>(&right.any_));
  case TypeID::Int64:
    return (*boost::any_cast<std::int64_t>(&any_) == *boost::any_cast<std::int64_t>(&right.any_));
  case TypeID::Float32:
    return (*boost::any_cast<float>(&any_) == *boost::any_cast<float>(&right.any_));
  case TypeID::Float64:
    return (*boost::any_cast<double>(&any_) == *boost::any_cast<double>(&right.any_));
  case TypeID::String:
    return (*boost::any_cast<std::string>(&any_) == *boost::any_cast<std::string>(&right.any_));
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

/// \brief Convert to string
std::string MetaInfoValue::toString() const noexcept {
  switch(type_) {
  case TypeID::Boolean:
    return std::to_string(*boost::any_cast<bool>(&any_));
  case TypeID::Int32:
    return std::to_string(*boost::any_cast<int>(&any_));
  case TypeID::Int64:
    return std::to_string(*boost::any_cast<std::int64_t>(&any_));
  case TypeID::Float32:
    return std::to_string(*boost::any_cast<float>(&any_));
  case TypeID::Float64:
    return std::to_string(*boost::any_cast<double>(&any_));
  case TypeID::String:
    return (*boost::any_cast<std::string>(&any_));
  default:
    serialbox_unreachable("Invalid TypeID");
  }
}

} // namespace serialbox
