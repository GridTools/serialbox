//===-- serialbox/core/Version.h ----------------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/Compiler.h"
#include <string>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Utility to deal with Serialbox versions
struct Version {
private:
  static int getVersion() {
    return SERIALBOX_VERSION_MAJOR * 100 + SERIALBOX_VERSION_MINOR * 10 + SERIALBOX_VERSION_PATCH;
  }

public:
  Version() = delete;

  /// \brief Convert to string
  /// @{
  static std::string toString(int version) {
    int major = version / 100;
    int minor = (version - major * 100) / 10;
    return Version::toString(major, minor, version - 100 * major - 10 * minor);
  }

  static std::string toString(int major, int minor, int patch) {
    return std::to_string(major) + "." + std::to_string(minor) + "." + std::to_string(patch);
  }
  /// @}

  /// \brief Check if the given version matches the current library version
  ///
  /// \return Return true if the versions match
  /// @{
  static bool match(int version) noexcept { return version == getVersion(); }
  /// @}

  /// \brief Check if the given version is compatible with the current library version (i.e. is
  /// older)
  ///
  /// \return Return true if the versions is compatible
  /// @{
  static bool isCompatible(int version) noexcept { return version <= getVersion(); }
  /// @}
};

/// @}

} // namespace serialbox

#endif
