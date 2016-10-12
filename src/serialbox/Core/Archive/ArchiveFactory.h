//===-- serialbox/Core/Archive/ArchiveFactory.h -------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_ARCHIVE_ARCHIVEFACTORY_H
#define SERIALBOX_CORE_ARCHIVE_ARCHIVEFACTORY_H

#include "serialbox/Core/Archive/Archive.h"
#include "serialbox/Core/Type.h"
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace serialbox {

/// \brief Factory to create Archives
///
/// Archives are registered at runtime via the first call to ArchiveFactory::getInstance.
class ArchiveFactory {
  ArchiveFactory();

public:
  /// \brief Function used to construct archives
  /// 
  /// The ArchiveFactory expects this function signature to construct the respective archive. The
  /// return type should be a pointer the the derived Archive as its dynamic-type.
  ///
  /// The function adheres to the following signature:
  /// \code
  /// std::unique_ptr<Archive> create(OpenModeKind mode,
  ///                                 const std::string& directory,
  ///                                 const std::string& prefix)
  /// \endcode
  /// The arguments have to match does of ArchiveFactory::create.
  using CreateArchiveFunction =
      std::function<std::unique_ptr<Archive>(OpenModeKind, const std::string&, const std::string&)>;

  /// \brief Return the instance of this singleton class
  /// 
  /// The first call will also register all archives.
  static ArchiveFactory& getInstance() noexcept;

  /// \brief Construct an instance of the archive ´name´
  ///
  /// \param name        Name of the archive
  /// \param mode        Policy to open files in the archive
  /// \param directory   Directory in which the archive is opened
  /// \param prefix      Prefix of all files
  ///
  /// \throw Exception   No archive with given name exists or is registered
  std::unique_ptr<Archive> create(const std::string& name, OpenModeKind mode,
                                  const std::string& directory, const std::string& prefix);

  /// \brief Register an archive (this function is called by ´SERIALBOX_REGISTER_ARCHIVE´)
  void registerArchive(const std::string& name, const CreateArchiveFunction& func);

  /// \brief Get a vector of strings of the registered archives
  std::vector<std::string> registeredArchives() const;

private:
  std::map<std::string, CreateArchiveFunction> registeredArchives_;
  static ArchiveFactory* instance_;
};

} // namespace serialbox

#endif
