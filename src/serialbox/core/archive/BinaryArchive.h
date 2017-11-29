//===-- serialbox/core/archive/BinaryArchive.h --------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the non-portable binary archive.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_ARCHIVE_BINARYARCHIVE_H
#define SERIALBOX_CORE_ARCHIVE_BINARYARCHIVE_H

#include "serialbox/core/Compiler.h"
#include "serialbox/core/Filesystem.h"
#include "serialbox/core/Json.h"
#include "serialbox/core/archive/Archive.h"
#include "serialbox/core/hash/Hash.h"
#include <string>
#include <unordered_map>
#include <vector>

namespace serialbox {

/// \brief Non-portable binary archive
///
/// \ingroup core
class BinaryArchive : public Archive {
public:
  /// \brief Name of the binary archive
  static const std::string Name;

  /// \brief Revision of the binary archive
  static const int Version;

  /// \brief Offset within a file
  struct FileOffsetType {
    std::streamoff offset; ///< Binary offset within the file
    std::string checksum;  ///< Checksum of the field
  };

  /// \brief Table of ids and corresponding offsets whithin in each field (i.e file)
  using FieldOffsetTable = std::vector<FileOffsetType>;

  /// \brief Table of all fields owned by this archive, each field has a corresponding file
  using FieldTable = std::unordered_map<std::string, FieldOffsetTable>;

  /// \brief
  BinaryArchive();

  /// \brief Initialize the archive
  ///
  /// \param mode          Policy to open files in the archive
  /// \param directory     Directory to write/read files. If the archive is opened in ´Read´ mode,
  ///                      the directory is expected to supply an ´ArchiveMetaData-prefix.json´.
  ///                      In case the archive is opened in ´Write´ mode, the directory will be
  ///                      cleansed from all files matching the pattern ´prefix_*.dat´, if the
  ///                      directory is  non-existent, it will be created.
  ///                      The ´Append´ mode will try to open ´ArchiveMetaData-prefix.json´ if the
  ///                      directory exists otherwise create it.
  /// \param prefix        Prefix of all files followed by an underscore ´_´ and fieldname
  /// \param skipMetaData  Do not read meta-data from disk
  BinaryArchive(OpenModeKind mode, const std::string& directory, const std::string& prefix,
                bool skipMetaData = false);

  /// \brief Copy constructor [deleted]
  BinaryArchive(const BinaryArchive&) = delete;

  /// \brief Copy assignment [deleted]
  BinaryArchive& operator=(const BinaryArchive&) = delete;

  /// \brief Destructor
  virtual ~BinaryArchive();

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

  virtual std::string directory() const override { return directory_.string(); }

  virtual std::string prefix() const override { return prefix_; }

  virtual std::string name() const override { return BinaryArchive::Name; }

  virtual std::string metaDataFile() const override { return metaDatafile_.string(); }

  virtual std::ostream& toStream(std::ostream& stream) const override;

  virtual void clear() override;

  virtual bool isReadingThreadSafe() const override { return true; }

  virtual bool isWritingThreadSafe() const override { return false; }

  virtual bool isSlicedReadingSupported() const override { return true; }

  /// @}

  /// \brief Clear fieldTable
  void clearFieldTable();

  /// \brief Create a BinaryArchive
  static std::unique_ptr<Archive> create(OpenModeKind mode, const std::string& directory,
                                         const std::string& prefix);

  /// \brief Get field table
  FieldTable& fieldTable() noexcept { return fieldTable_; }
  const FieldTable& fieldTable() const noexcept { return fieldTable_; }

  /// \brief Directly write field (given by `storageView`) to file
  ///
  /// \param filename     Newly created file (if file already exists, it's contents will be
  ///                     discarded)
  /// \param storageView  StorageView of the field
  static void writeToFile(std::string filename, const StorageView& storageView);

  /// \brief Directly read field (given by `storageView`) from file
  ///
  /// This function can be used to implement stateless deserialization methods.
  ///
  /// \param filename     File to read from
  /// \param storageView  StorageView of the field
  static void readFromFile(std::string filename, StorageView& storageView);

  /// \brief Set the hash algorithm
  void setHash(std::unique_ptr<Hash> hash) noexcept { hash_ = std::move(hash); }

  /// \brief Get the hash algorithm
  const std::unique_ptr<Hash>& hash() const noexcept { return hash_; }

private:
  OpenModeKind mode_;
  filesystem::path directory_;
  std::string prefix_;

  filesystem::path metaDatafile_;
  std::unique_ptr<Hash> hash_;
  json::json json_;
  FieldTable fieldTable_;
};

} // namespace serialbox

#endif
