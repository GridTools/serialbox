//===-- serialbox/core/hash/HashFactory.cpp -----------------------------------------*- C++ -*-===//
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

#include "serialbox/core/hash/HashFactory.h"
#include "serialbox/core/Exception.h"
#include "serialbox/core/STLExtras.h"
#include "serialbox/core/hash/MD5.h"
#include "serialbox/core/hash/SHA256.h"
#include <sstream>

#include <iostream>

namespace serialbox {

std::unique_ptr<Hash> HashFactory::create(const std::string& name) {
  if(name == MD5::Name) {
    return std::make_unique<MD5>();
  } else if(name == SHA256::Name) {
    return std::make_unique<SHA256>();
  } else {
    std::stringstream ss;
    ss << "cannot create Hash '" << name << "': hash does not exist or is not registred.\n";
    ss << "Registered hashes:\n";
    for(const auto& hash : HashFactory::registeredHashes())
      ss << " " << hash << "\n";
    throw Exception(ss.str().c_str());
  }
}

std::vector<std::string> HashFactory::registeredHashes() {
  std::vector<std::string> hashes{MD5::Name, SHA256::Name};
  return hashes;
}

std::string HashFactory::defaultHash() {
#ifdef SERIALBOX_HAS_OPENSSL
  return MD5::Name;
#else
  return SHA256::Name;
#endif
}

} // namespace serialbox
