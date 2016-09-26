//===-- serialbox/Core/UpgradArchive.h ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Upgrade an archive from an older version of serialbox to the current version.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_UPGRADEARCHIVE_H
#define SERIALBOX_CORE_UPGRADEARCHIVE_H

#include <boost/filesystem.hpp>

namespace serialbox {

/// \brief Upgrade an archive from an older version of serialbox to the current version
class UpgradeArchive {
public:
  /// \brief Check if ´directory´ contains meta-information of older version of serialbox and
  /// upgrade it if necessary
  ///
  /// The function will check if there is a ´Fields.json´ file which is newer than ´MetaData.json´
  /// and, if ture, convert ´Fields.json´ to ´MetaData.json´ and ´ArchiveMetaData.json´.
  ///
  /// \param directory    Directory of the Archive and Serializer meta-data
  ///
  /// \throw Exception
  static void upgrade(const boost::filesystem::path& directory);
};

} // namespace serialbox

#endif
