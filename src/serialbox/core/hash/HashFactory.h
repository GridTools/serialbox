//===-- serialbox/core/hash/HashFactory.h -------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the hash factory which constructs the different hash algorithms.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_HASH_HASHFACTORY_H
#define SERIALBOX_CORE_HASH_HASHFACTORY_H

#include "serialbox/core/hash/Hash.h"
#include <memory>

namespace serialbox {

/// \brief Factory to create Hash algorithms
///
/// \ingroup core
class HashFactory {
  HashFactory() = delete;

public:
  /// \brief Construct an instance of the Hash `name`
  ///
  /// \param name        Name of the Hash (as given by Hash::name())
  /// \return Pointer to the newly constructed Hash with the requested dynamic-type
  ///
  /// \throw Exception   No Hash with given `name` exists or is registered
  static std::unique_ptr<Hash> create(const std::string& name);

  /// \brief Get a vector of strings of the registered hashes
  static std::vector<std::string> registeredHashes();

  /// \brief Get the default hash algorithm (currently MD5 if avialable, SHA256 otherwise)
  static std::string defaultHash();
};

} // namespace serialbox

#endif
