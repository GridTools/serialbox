//===-- Core/UpgradArchive.cpp ------------------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/Archive/Archive.h"
#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Json.h"
#include "serialbox/Core/SerializerImpl.h"
#include "serialbox/Core/UpgradeArchive.h"
#include <ctime>
#include <fstream>

namespace serialbox {

void UpgradeArchive::upgrade(const boost::filesystem::path& directory) {
  try {
    boost::filesystem::path oldMetaDataFile = directory / "Fields.json";

    // Check if Fields.json exists
    if(!boost::filesystem::exists(oldMetaDataFile))
      return;

    boost::filesystem::path newSerializerMetaDataFile = SerializerImpl::SerializerMetaDataFile;
    boost::filesystem::path newArchiveMetaDataFile = Archive::ArchiveMetaDataFile;

    // Check if we already upgraded this archive
    if(boost::filesystem::exists(newSerializerMetaDataFile) &&
       (boost::filesystem::last_write_time(oldMetaDataFile) <
        boost::filesystem::last_write_time(newSerializerMetaDataFile)))
      return;

    // Read Fields.json
    json::json oldMetaDataJson;
    std::ifstream ifs(oldMetaDataFile.string());
    ifs >> oldMetaDataJson;

  } catch(boost::filesystem::filesystem_error& e) {
    throw Exception("filesystem error: %s", e.what());
  }
}

} // namespace serialbox
