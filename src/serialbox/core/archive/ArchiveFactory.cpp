//===-- serialbox/core/archive/ArchiveFactory.cpp -----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Factory to create the different Archives.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/archive/ArchiveFactory.h"
#include "serialbox/core/Exception.h"
#include "serialbox/core/Unreachable.h"
#include "serialbox/core/archive/BinaryArchive.h"
#include "serialbox/core/archive/MockArchive.h"
#include "serialbox/core/archive/NetCDFArchive.h"

namespace serialbox {

std::unique_ptr<Archive> ArchiveFactory::create(const std::string& name, OpenModeKind mode,
                                                const std::string& directory,
                                                const std::string& prefix) {
  if(name == BinaryArchive::Name) {
    return std::make_unique<BinaryArchive>(mode, directory, prefix);
  } else if(name == MockArchive::Name) {
    return std::make_unique<MockArchive>(mode);
#ifdef SERIALBOX_HAS_NETCDF
  } else if(name == NetCDFArchive::Name) {
    return std::make_unique<NetCDFArchive>(mode, directory, prefix);
#endif
  } else {
    std::stringstream ss;
    ss << "cannot create Archive '" << name << "': archive does not exist or is not registred.\n";
    ss << "Registered archives:\n";
    for(const auto& archive : ArchiveFactory::registeredArchives())
      ss << " " << archive << "\n";
    throw Exception(ss.str().c_str());
  }
}

std::vector<std::string> ArchiveFactory::registeredArchives() {
  std::vector<std::string> archives{BinaryArchive::Name, MockArchive::Name
#ifdef SERIALBOX_HAS_NETCDF
                                    ,
                                    NetCDFArchive::Name
#endif
  };
  return archives;
}

std::string ArchiveFactory::archiveFromExtension(std::string filename) {
  std::string extension = std::filesystem::path(filename).extension().string();

  if(extension == ".dat" || extension == ".bin")
    return BinaryArchive::Name;
#ifdef SERIALBOX_HAS_NETCDF
  else if(extension == ".nc")
    return NetCDFArchive::Name;
#endif
  else
    throw Exception("cannot deduce Archive from file extension: %s", filename);
  serialbox_unreachable("invalid file extension");
}

//===------------------------------------------------------------------------------------------===//
//     Writing
//===------------------------------------------------------------------------------------------===//

void ArchiveFactory::writeToFile(std::string filename, const StorageView& storageView,
                                 std::string archiveName, std::string fieldname) {

  LOG(info) << "Attempting to write field \"" << fieldname << "\" via \"" << archiveName
            << "\" archive from " << filename;

  if(archiveName == BinaryArchive::Name) {
    BinaryArchive::writeToFile(filename, storageView);
  }
#ifdef SERIALBOX_HAS_NETCDF
  else if(archiveName == NetCDFArchive::Name) {
    NetCDFArchive::writeToFile(filename, storageView, fieldname);
  }
#endif
  else
    throw Exception("cannot use Archive '%s': archive does not exist or is not registred",
                    filename);

  LOG(info) << "Successfully wrote field \"" << fieldname << "\" to " << filename;
}

//===------------------------------------------------------------------------------------------===//
//     Reading
//===------------------------------------------------------------------------------------------===//

void ArchiveFactory::readFromFile(std::string filename, StorageView& storageView,
                                  std::string archiveName, std::string fieldname) {

  LOG(info) << "Attempting to read field \"" << fieldname << "\" via \"" << archiveName
            << "\" archive from " << filename;

  if(archiveName == BinaryArchive::Name) {
    BinaryArchive::readFromFile(filename, storageView);
  }
#ifdef SERIALBOX_HAS_NETCDF
  else if(archiveName == NetCDFArchive::Name) {
    NetCDFArchive::readFromFile(filename, storageView, fieldname);
  }
#endif
  else
    throw Exception("cannot use Archive '%s': archive does not exist or is not registred",
                    filename);

  LOG(info) << "Successfully read field \"" << fieldname << "\" to " << filename;
}

} // namespace serialbox
