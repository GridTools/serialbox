//===-- serialbox/core/Has/MD5.cpp --------------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/hash/MD5.h"
#include "serialbox/core/Compiler.h"
#include "serialbox/core/Exception.h"
#include <algorithm>
#include <iomanip>
#include <iterator>
#include <sstream>

#ifdef SERIALBOX_HAS_OPENSSL
#include <openssl/md5.h>
#endif

namespace serialbox {

const char* MD5::Name = "MD5";

std::string MD5::hash(const void* data, int length) {
  std::string hash;

#ifdef SERIALBOX_HAS_OPENSSL

  unsigned char digest[MD5_DIGEST_LENGTH];
  ::MD5((unsigned char*)data, length, digest);

  std::ostringstream ss;
  ss << std::hex << std::setfill('0') << std::setw(2) << std::uppercase;
  std::copy(digest, digest + MD5_DIGEST_LENGTH, std::ostream_iterator<int>(ss));
  hash = ss.str();

#else
  throw Exception("MD5 hash is only available with OpenSSL support");
#endif

  return hash;
}

} // namespace serialbox
