//===-- serialbox/Core/SHA256.cpp ---------------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/SHA256.h"
#include <sha256/sha256.h>
#include <sstream>

namespace serialbox {

std::string SHA256::hash(const void* data, int length) noexcept {
  BYTE hash[32];
  sha256(data, length, hash);

  std::ostringstream ss;
  for(int i = 0; i < 32; ++i)
    ss << std::hex << std::uppercase << static_cast<int>(hash[i]);

  return ss.str();
}

} // namespace serialbox
