//===-- serialbox/Core/MD5.h --------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// OpenSSL implementation of the MD5 cryptographic hash algorithm.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_MD5_H
#define SERIALBOX_CORE_MD5_H

#include <string>

namespace serialbox {

/// \brief Implementation of MD5 cryptographic hash algorithm
///
/// \see https://en.wikipedia.org/wiki/MD5
struct MD5 {
  MD5() = delete;
  
  /// \brief Identifier of the hash
  static const char* Name;
  
  /// \brief Compute 128 bit hash using MD5
  /// 
  /// This function is only available if Serialbox has OpenSSL support.
  ///
  /// \param data     Binary data
  /// \param length   Lenght of the binary data
  ///
  /// \return MD5 hash hex representation as string
  static std::string hash(const void* data, int length);
};

} // namespace serialbox

#endif
