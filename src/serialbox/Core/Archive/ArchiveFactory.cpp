//===-- serialbox/Core/Archive/ArchiveFactory.cpp -----------------------------------*- C++ -*-===//
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

#include "serialbox/Core/Archive/ArchiveFactory.h"
#include "serialbox/Core/Archive/BinaryArchive.h"
#include "serialbox/Core/Archive/NetCDFArchive.h"
#include "serialbox/Core/Unreachable.h"
#include <cstdlib>
#include <iostream>

namespace serialbox {

ArchiveFactory* ArchiveFactory::instance_ = nullptr;

ArchiveFactory::ArchiveFactory() : registeredArchives_() {}

ArchiveFactory& ArchiveFactory::getInstance() noexcept {
  if(!instance_) {
    instance_ = new ArchiveFactory();

    // Register Archives
    instance_->registerArchive(BinaryArchive::Name, BinaryArchive::create);

#ifdef SERIALBOX_HAS_NETCDF
    instance_->registerArchive(NetCDFArchive::Name, NetCDFArchive::create);
#endif
  }

  return (*instance_);
}

std::unique_ptr<Archive> ArchiveFactory::create(const std::string& name, OpenModeKind mode,
                                                const std::string& directory,
                                                const std::string& prefix) {
  auto it = registeredArchives_.find(name);
  if(it != registeredArchives_.end())
    return it->second(mode, directory, prefix);

  std::stringstream ss;
  ss << "cannot create Archive '" << name << "': archive does not exist or is not registred.\n";
  ss << "Registered archives:\n";
  for(auto archive : registeredArchives_)
    ss << " " << archive.first << "\n";
  throw Exception(ss.str().c_str());
}

void ArchiveFactory::registerArchive(const std::string& name, const CreateArchiveFunction& func) {
  if(!registeredArchives_.insert({name, func}).second) {
    std::cerr << "serialbox error: multiple registration of archive '" << name << "'" << std::endl;
    std::abort();
  }
}

std::vector<std::string> ArchiveFactory::registeredArchives() const {
  std::vector<std::string> archives;
  for(auto it = registeredArchives_.begin(), end = registeredArchives_.end(); it != end; ++it)
    archives.push_back(it->first);
  return archives;
}

std::string ArchiveFactory::archiveFromExtension(std::string filename) {
  std::string extension = boost::filesystem::path(filename).extension().string();

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
