//===-- serialbox/core/hash/Hash.h --------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the abstract interface of the Hash algorithms.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_HASH_HASH_H
#define SERIALBOX_CORE_HASH_HASH_H

#include <string>

namespace serialbox {

/// \brief Hash algorithm interface
///
/// \ingroup core
class Hash {
public:
  /// \brief Get identifier of the hash as used in the HashFactory
  ///
  /// \return Name of the Hash
  virtual const char* name() const noexcept = 0;

  /// \brief Compute hash
  ///
  /// \param data     Binary data
  /// \param length   Lenght of the binary data
  ///
  /// \return Hex representation as string of the computed hash
  virtual std::string hash(const void* data, int length) = 0;

  virtual ~Hash(){};
};

} // namespace serialbox

#endif
