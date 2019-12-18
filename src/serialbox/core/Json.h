//===-- serialbox/core/Json.h -------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file includes the headers of the json library.
/// See: https://github.com/nlohmann/json/tree/master
///
/// IMPORTANT: Conversion between our types and json::json is enabled via assignment by ADL.
/// However the ADL-enablers (to_json, from_json) are not included in the types declaration header
/// to not pollute the public interface with the json library. Therefore we need to include the
/// <MyType>Serializer.hpp in files where we want to do <MyType> <-> json conversions. Do this only
/// in cpp-files or private headers.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_JSON_H
#define SERIALBOX_CORE_JSON_H

// On purpose including relatively as we are going out of the serialbox include directory. We don't
// want to install json.hpp to prevent accidently including it in the public interface.
// TODO split public and private headers properly in separate directories.
#include "../../external/json/json.hpp"

/// \namespace json
/// \brief Namespace of the JSON library
///
/// \see https://github.com/nlohmann/json/tree/master
namespace json = nlohmann;

#endif
