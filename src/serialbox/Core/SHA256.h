//===-- serialbox/Core/SHA256.h -----------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Implementation of the SHA-1 cryptographic hash algorithm.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SHA256_H
#define SERIALBOX_CORE_SHA256_H

#include <string>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Implementation of the SHA-1 (Secure Hash Algorithm 1)
///
/// \see 
///   https://en.wikipedia.org/wiki/SHA-1
struct SHA256 {
  SHA256() = delete;

  /// \brief Identifier of the hash
  static const char* Name;

  /// \brief Compute 256 bit hash using SHA-1
  ///
  /// \param data     Binary data
  /// \param length   Lenght of the binary data
  ///
  /// \return SHA-1 hash hex representation as string
  static std::string hash(const void* data, int length) noexcept;
};

/// @}

} // namespace serialbox

#endif
