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
/// Implementation of the SHA-1 cryptographic hash function.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SHA256_H
#define SERIALBOX_CORE_SHA256_H

#include <string>

namespace serialbox {

/// \brief Implementation of the SHA-1 (Secure Hash Algorithm 1) cryptographic hash function
/// \see https://en.wikipedia.org/wiki/SHA-1
struct SHA256 {
  SHA256() = delete;

  /// \brief Compute 256 bit hash using SHA-1
  /// 
  /// \param data     Binary data
  /// \param length   Lenght of the binary data
  /// \return SHA-1 hash as string of length 32
  static std::string hash(const void* data, int length) noexcept;
};

} // namespace serialbox

#endif
