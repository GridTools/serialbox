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
#include <cstdlib>
#include <iostream>

namespace serialbox {

ArchiveFactory* ArchiveFactory::instance_ = nullptr;

ArchiveFactory::ArchiveFactory() : registeredArchives_() {}

ArchiveFactory& ArchiveFactory::getInstance() noexcept {
  if(!instance_)
    instance_ = new ArchiveFactory();
  return (*instance_);
}

std::unique_ptr<Archive> ArchiveFactory::create(const std::string& name, OpenModeKind mode,
                                                const std::string& directory,
                                                const std::string& prefix) {
  auto it = registeredArchives_.find(name);
  if(it != registeredArchives_.end())
    return it->second(mode, directory, prefix);
  throw Exception("cannot create Archive '%s': archive does not exist or is not registred", name);
}

void ArchiveFactory::registerArchive(const std::string& name, const CreateArchiveFunction& func) {
  // This function is called during static initialization, if we cannot register the archive we just
  // die.
  if(!registeredArchives_.insert({name, func}).second) {
    std::cerr << "serialbox error: multiple registration of archive '" << name << "'" << std::endl;
    std::abort();
  }
}

} // namespace serialbox
