//===-- serialbox/core/archive/ZarrArchive.h ----------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the archive based on the Zarr storage format (v2).
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_ARCHIVE_ZARRARCHIVE_H
#define SERIALBOX_CORE_ARCHIVE_ZARRARCHIVE_H

#include "serialbox/core/Json.h"
#include "serialbox/core/archive/Archive.h"
#include <filesystem>
#include <string>
#include <unordered_map>
#include <vector>

namespace serialbox {

/// \brief Archive based on the Zarr storage format (version 2)
///
/// Each field is stored as a Zarr v2 array in a subdirectory named
/// `<prefix>_<field>.zarr/` within the archive directory. The first
/// dimension of the array corresponds to the save index (FieldID::id),
/// allowing multiple saves of the same field in a single Zarr store.
/// Data is stored without compression in native byte order.
///
/// Directory layout:
/// \code
///   <directory>/
///     ArchiveMetaData-<prefix>.json   -- serialbox metadata
///     <prefix>_<field1>.zarr/
///       .zarray                       -- Zarr array metadata (JSON)
///       0.0.0...0                     -- chunk for save 0
///       1.0.0...0                     -- chunk for save 1
///       ...
///     <prefix>_<field2>.zarr/
///       ...
/// \endcode
///
/// \see https://zarr.readthedocs.io/en/stable/spec/v2.html
///
/// \ingroup core
class ZarrArchive : public Archive {
public:
  /// \brief Name of the Zarr archive
  static const std::string Name;

  /// \brief Revision of the Zarr archive
  static const int Version;

  /// \brief Initialize the archive
  ///
  /// \param mode          Policy to open files in the archive
  /// \param directory     Directory to write/read files. If the archive is opened in 'Read' mode,
  ///                      the directory is expected to supply an 'ArchiveMetaData-prefix.json'.
  ///                      In 'Write' mode, existing field directories matching the pattern
  ///                      '<prefix>_*.zarr' will be removed and recreated.
  ///                      The 'Append' mode will open existing metadata if present.
  /// \param prefix        Prefix of all field directories, followed by an underscore and fieldname
  ZarrArchive(OpenModeKind mode, const std::string& directory, const std::string& prefix);

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

  virtual std::string name() const override { return ZarrArchive::Name; }

  virtual std::string metaDataFile() const override { return metaDatafile_.string(); }

  virtual std::ostream& toStream(std::ostream& stream) const override;

  virtual void clear() override;

  virtual bool isReadingThreadSafe() const override { return false; }

  virtual bool isWritingThreadSafe() const override { return false; }

  virtual bool isSlicedReadingSupported() const override { return false; }

  /// @}

  /// \brief Create a ZarrArchive
  static std::unique_ptr<Archive> create(OpenModeKind mode, const std::string& directory,
                                         const std::string& prefix);

  /// \brief Directly write a field (given by `storageView`) to a Zarr store directory
  ///
  /// Creates a single-save Zarr array at the given path. The path should either
  /// not exist or be an existing Zarr store directory.
  ///
  /// \param zarrPath     Path to the Zarr store directory (conventionally ending in .zarr)
  /// \param storageView  StorageView of the field
  /// \param field        Name of the field (used as Zarr variable name)
  static void writeToFile(std::string zarrPath, const StorageView& storageView,
                          const std::string& field);

  /// \brief Directly read a field (given by `storageView`) from a Zarr store directory
  ///
  /// \param zarrPath     Path to the Zarr store directory
  /// \param storageView  StorageView of the field
  /// \param field        Name of the field
  static void readFromFile(std::string zarrPath, StorageView& storageView,
                           const std::string& field);

private:
  OpenModeKind mode_;
  std::filesystem::path directory_;
  std::string prefix_;
  std::filesystem::path metaDatafile_;

  /// Maps field name to the maximum save id written so far
  std::unordered_map<std::string, int> fieldMap_;
  json::json json_;

  /// \brief Return the path to the Zarr array directory for a given field
  std::filesystem::path fieldDirectory(const std::string& field) const;

  /// \brief Return the path to a chunk file for a given field and save id
  ///
  /// Chunk naming follows Zarr v2: each dimension index separated by '.'.
  /// The first index is the save id; all remaining indices are 0.
  std::filesystem::path chunkFile(const std::string& field, int saveId,
                                  std::size_t numDataDims) const;

  /// \brief Write or update the .zarray metadata file for a field
  ///
  /// \param fieldDir      Path to the Zarr array directory
  /// \param storageView   StorageView describing the field shape and type
  /// \param numSaves      Current total number of saves (length of first dimension)
  void writeZarrayMetadata(const std::filesystem::path& fieldDir, const StorageView& storageView,
                           int numSaves) const;

  /// \brief Read the .zarray metadata from a Zarr array directory
  json::json readZarrayMetadata(const std::filesystem::path& fieldDir) const;

  /// \brief Copy data from the StorageView into a contiguous flat buffer (column-major iteration)
  static std::vector<Byte> storageViewToBuffer(const StorageView& storageView);

  /// \brief Copy data from a contiguous flat buffer into the StorageView (column-major iteration)
  static void bufferToStorageView(const std::vector<Byte>& buffer, StorageView& storageView);

  /// \brief Return the Zarr dtype string for the given TypeID
  ///
  /// Uses the system's native byte order prefix ('<' for little-endian, '>' for big-endian).
  static std::string typeIDtoZarrDtype(TypeID type);

  /// \brief Return the system byte-order character ('>' or '<')
  static char nativeEndianChar();
};

} // namespace serialbox

#endif
