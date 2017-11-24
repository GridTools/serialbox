//===-- serialbox/core/archive/NetCDFArchive.h --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the archive based on NetCDF.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_ARCHIVE_NETCDFARCHIVE_H
#define SERIALBOX_CORE_ARCHIVE_NETCDFARCHIVE_H

#include "serialbox/core/Compiler.h"
#ifdef SERIALBOX_HAS_NETCDF

#include "serialbox/core/Json.h"
#include "serialbox/core/archive/Archive.h"
#include "../Filesystem.h"
#include <string>
#include <unordered_map>
#include <vector>

namespace serialbox {

/// \brief Archive based on NetCDF
///
/// \see https://github.com/Unidata/netcdf-c
///
/// \ingroup core
class NetCDFArchive : public Archive {
public:
  /// \brief Name of the NetCDF archive
  static const std::string Name;

  /// \brief Revision of the NetCDF archive
  static const int Version;

  /// \brief Initialize the archive
  ///
  /// \param mode          Policy to open files in the archive
  /// \param directory     Directory to write/read files. If the archive is opened in ´Read´ mode,
  ///                      the directory is expected to supply an ´ArchiveMetaData-prefix.json´.
  ///                      In case the archive is opened in ´Write´ mode, the directory will be
  ///                      cleansed from all files matching the pattern ´prefix_*.nc´, if the
  ///                      directory is  non-existent, it will be created.
  ///                      The ´Append´ mode will try to open ´ArchiveMetaData-prefix.json´ if the
  ///                      directory exists otherwise create it.
  /// \param prefix        Prefix of all files followed by an underscore ´_´ and fieldname
  /// \param skipMetaData  Do not read meta-data from disk
  NetCDFArchive(OpenModeKind mode, const std::string& directory, const std::string& prefix);

  /// \brief Load meta-data from JSON file
  void readMetaDataFromJson();

  /// \brief Convert meta-data to JSON and serialize to file
  void writeMetaDataToJson();

  /// \name Archive implementation
  /// \see Archive
  /// @{
  virtual FieldID write(const StorageView& storageView, const std::string& fieldID,
                        const std::shared_ptr<FieldMetainfoImpl> info) override;

  virtual void read(StorageView& storageView, const FieldID& fieldID,
                    std::shared_ptr<FieldMetainfoImpl> info) const override;

  virtual void updateMetaData() override;

  virtual OpenModeKind mode() const override { return mode_; }

  virtual const std::string& directory() const override { return directory_.string(); }

  virtual const std::string& prefix() const override { return prefix_; }

  virtual const std::string& name() const override { return NetCDFArchive::Name; }

  virtual const std::string& metaDataFile() const override { return metaDatafile_.string(); }

  virtual std::ostream& toStream(std::ostream& stream) const override;

  virtual void clear() override;

  virtual bool isReadingThreadSafe() const override { return false; }

  virtual bool isWritingThreadSafe() const override { return false; }

  virtual bool isSlicedReadingSupported() const override { return false; }

  /// @}

  /// \brief Create a NetCDFArchive
  static std::unique_ptr<Archive> create(OpenModeKind mode, const std::string& directory,
                                         const std::string& prefix);

  /// \brief Directly write field (given by `storageView`) to file
  ///
  /// \param filename     Newly created file (if file already exists, it's contents will be
  ///                     discarded)
  /// \param storageView  StorageView of the field
  /// \param field        Name of the field
  static void writeToFile(std::string filename, const StorageView& storageView,
                          const std::string& field);

  /// \brief Directly read field (given by `storageView`) from file
  ///
  /// This function can be used to implement stateless deserialization methods.
  ////
  /// \param filename     File to read from
  /// \param storageView  StorageView of the field
  /// \param field        Name of the field
  static void readFromFile(std::string filename, StorageView& storageView,
                           const std::string& field);

private:
  OpenModeKind mode_;
  SB_FILESYSTEM::path directory_;
  std::string prefix_;
  SB_FILESYSTEM::path metaDatafile_;

  std::unordered_map<std::string, int> fieldMap_;
  json::json json_;
};

} // namespace serialbox

#endif // SERIALBOX_HAS_NETCDF

#endif
