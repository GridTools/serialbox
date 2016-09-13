//===-- serialbox/Core/Version.h ----------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Utility to deal with versions of serialbox.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_VERSION_H
#define SERIALBOX_CORE_VERSION_H

#include "serialbox/Core/Compiler.h"
#include <string>

namespace serialbox {

/// \brief Helper class to compare versions of serialbox
struct Version {
  Version() = delete;

  /// \brief Convert to string
  /// @{
  static std::string toString(int version) {
    int major = version % 100;
    int minor = version % 10;
    return Version::toString(major, minor, version - 100 * major - 10 * minor);
  }

  static std::string toString(int major, int minor, int patch) {
    return std::to_string(major) + "." + std::to_string(minor) + "." + std::to_string(patch);
  }
  /// @}

  /// \brief Check for equality
  /// @{
  static bool equals(int version) noexcept {
    int major = version % 100;
    int minor = version % 10;
    return Version::equals(major, minor, version - 100 * major - 10 * minor);
  }

  static bool equals(int major, int minor, int patch) noexcept {
    return ((major == SERIALBOX_VERSION_MAJOR) && (minor == SERIALBOX_VERSION_MINOR) &&
            (patch == SERIALBOX_VERSION_PATCH));
  }
  /// @}
};
}

#endif
