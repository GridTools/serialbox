//===-- serialbox/Core/Hash/MD5.h ---------------------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_HASH_MD5_H
#define SERIALBOX_CORE_HASH_MD5_H

#include "serialbox/Core/Hash/Hash.h"

namespace serialbox {

/// \brief Implementation of MD5 cryptographic hash algorithm
///
/// \see
///   https://www.openssl.org/docs/manmaster/crypto/MD2.html
///
/// \ingroup core
class MD5 : public Hash {
public:
  /// \brief Identifier of the hash
  static const char* Name;

  /// \brief Get identifier of the hash as used in the HashFactory
  ///
  /// \return Name of the Hash
  virtual const char* name() const noexcept override { return Name; }

  /// \brief Compute 128 bit hash using MD5
  ///
  /// This function is only available if Serialbox has OpenSSL support.
  ///
  /// \param data     Binary data
  /// \param length   Lenght of the binary data
  ///
  /// \return MD5 hash hex representation as string
  virtual std::string hash(const void* data, int length) override;
};

} // namespace serialbox

#endif
