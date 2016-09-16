//===-- serialbox/Core/Archive/BinaryArchive.h --------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/Json.h"
#include "serialbox/Core/Archive/Archive.h"
#include <boost/filesystem.hpp>
#include <boost/noncopyable.hpp>
#include <unordered_map>
#include <vector>

namespace serialbox {

/// \brief Non-portable binary archive
class BinaryArchive : public Archive {
public:
  /// \brief Name of the binary archive
  static const std::string Name;
  
  /// \brief Revision of the binary archive
  static const int Version;
    
  /// \brief Offset
  struct FileOffsetType {
    std::streamoff offset; ///< Binary offset within the file
    std::string checksum;  ///< Checksum of the field
  };

  /// \brief Table of ids and corresponding offsets whithin in each field (i.e file)
  using FieldOffsetTable = std::vector<FileOffsetType>;

  /// \brief Table of all fields owned by this archive, each field has a corresponding file
  using FieldTable = std::unordered_map<std::string, FieldOffsetTable>;

  /// \brief Initialize the archive with a directory and open file policy
  BinaryArchive(const boost::filesystem::path& directory, OpenModeKind mode);
  
  /// \brief Copy constructor [deleted]
  BinaryArchive(const BinaryArchive&) = delete;
  
  /// \brief Copy assignment [deleted
  BinaryArchive& operator=(const BinaryArchive&) = delete;
  
  /// \brief Destructor
  virtual ~BinaryArchive();  

  /// \brief Load meta-data from JSON file
  void readMetaDataFromJson();
  
  /// \brief Convert meta-data to JSON and serialize to file
  void writeMetaDataToJson(); 
  
  /// \brief Set meta data to be out-of-data
  void setMetaDataDirty() noexcept { metaDataDirty_ = true; }
  
  virtual void write(StorageView& storageView, const FieldID& fieldID) throw(Exception) override;
  virtual void read(StorageView& storageView, const FieldID& fieldID) throw(Exception) override;
  virtual void updateMetaData() override;
  virtual const std::string& directory() const override { return directory_.string(); }
  virtual const std::string& name() const override { return BinaryArchive::Name; }
  virtual std::ostream& toStream(std::ostream& stream) const override; 
  
private:
  OpenModeKind mode_;
  boost::filesystem::path directory_;
  std::vector<Byte> binaryData_;
  
  json::json json_;
  FieldTable fieldTable_;
  bool metaDataDirty_;
};

} // namespace serialbox

#endif
