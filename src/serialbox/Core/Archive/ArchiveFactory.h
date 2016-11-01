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
#include <memory>
#include <string>
#include <vector>

namespace serialbox {

/// \brief Factory to create Archives
///
/// \ingroup core
class ArchiveFactory {
  ArchiveFactory() = delete;

public:
  /// \brief Construct an instance of the archive ´name´
  ///
  /// \param name        Name of the archive
  /// \param mode        Policy to open files in the archive
  /// \param directory   Directory in which the archive is opened
  /// \param prefix      Prefix of all files
  ///
  /// \throw Exception   No archive with given name exists or is registered
  static std::unique_ptr<Archive> create(const std::string& name, OpenModeKind mode,
                                  const std::string& directory, const std::string& prefix);

  /// \brief Get a vector of strings of the registered archives
  static std::vector<std::string> registeredArchives();

  /// \brief Deduce the name of the `archive` according to the extension of the `filename`
  ///
  /// Extensions    | Archives
  /// ------------- | --------
  /// .dat, .bin    | Binary
  /// .nc           | NetCDF
  ///
  static std::string archiveFromExtension(std::string filename);

  /// \brief Directly write field (given by `storageView`) to file
  ///
  /// \param filename     Newly created file (if file already exists, it's contents will be
  ///                     discarded)
  /// \param storageView  StorageView of the field
  /// \param archiveName  Archive used to perform serialization
  /// \param fieldname    Name of the field (might be unused for certain archives)
  static void writeToFile(std::string filename, const StorageView& storageView,
                          std::string archiveName, std::string fieldname);

  /// \brief Directly read field (given by `storageView`) from file
  ///
  /// \param filename     File to read from
  /// \param storageView  StorageView of the field
  /// \param archiveName  Archive used to perform serialization
  /// \param fieldname    Name of the field (might be unused for certain archives)
  static void readFromFile(std::string filename, StorageView& storageView, std::string archiveName,
                           std::string fieldname);
};

} // namespace serialbox

#endif
