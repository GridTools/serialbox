//===-- serialbox/core/hash/SHA256.h ------------------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_HASH_SHA256_H
#define SERIALBOX_CORE_HASH_SHA256_H

#include "serialbox/core/hash/Hash.h"

namespace serialbox {

/// \brief Implementation of the SHA-1 (Secure Hash Algorithm 1)
///
/// \see
///   https://en.wikipedia.org/wiki/SHA-1
/// 
/// \ingroup core
class SHA256 : public Hash {
public:
  /// \brief Identifier of the hash
  static const char* Name;

  /// \brief Get identifier of the hash as used in the HashFactory
  ///
  /// \return Name of the Hash
  virtual const char* name() const noexcept override { return Name; }
  
  /// \brief Compute 256 bit hash using SHA-1
  ///
  /// \param data     Binary data
  /// \param length   Lenght of the binary data
  ///
  /// \return SHA-1 hash hex representation as string
  virtual std::string hash(const void* data, int length) override;
};

} // namespace serialbox

#endif
