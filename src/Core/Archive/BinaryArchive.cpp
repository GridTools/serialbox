//===-- Core/Archive/BinaryArchive.cpp ----------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/Archive/ArchiveFactory.h"
#include "serialbox/Core/Archive/BinaryArchive.h"
#include "serialbox/Core/SHA256.h"
#include "serialbox/Core/STLExtras.h"
#include "serialbox/Core/Version.h"
#include <fstream>
#include <iostream>

namespace serialbox {

const std::string BinaryArchive::Name = "BinaryArchive";

const int BinaryArchive::Version = 0;

BinaryArchive::~BinaryArchive() {}

void BinaryArchive::readMetaDataFromJson() {

  boost::filesystem::path filename = directory_ / Archive::ArchiveMetaDataFile;
  LOG(INFO) << "Reading MetaData for BinaryArchive ... ";

  fieldTable_.clear();
  json_.clear();

  // Writing always operates on fresh directories
  if(mode_ == OpenModeKind::Write)
    return;

  // Check if metaData file exists
  if(!boost::filesystem::exists(filename)) {
    if(mode_ == OpenModeKind::Append)
      return;
    throw Exception("archive meta data not found in directory '%s'", directory_.string());
  }

  std::ifstream fs(filename.string(), std::ios::in);
  fs >> json_;
  fs.close();

  int serialboxVersion = json_["serialbox_version"];
  std::string archiveName = json_["archive_name"];
  int archiveVersion = json_["archive_version"];

  // Check consistency
  if(!Version::compare(serialboxVersion))
    throw Exception("serialbox version of binary archive (%s) does not match the version "
                    "of the library (%s)",
                    Version::toString(serialboxVersion), SERIALBOX_VERSION_STRING);

  if(archiveName != BinaryArchive::Name)
    throw Exception("archive is not a binary archive");

  if(archiveVersion != BinaryArchive::Version)
    throw Exception("binary archive version (%s) does not match the version of the library (%s)",
                    archiveVersion, BinaryArchive::Version);

  // Deserialize FieldTable
  for(auto it = json_["fields_table"].begin(); it != json_["fields_table"].end(); ++it) {
    FieldOffsetTable fieldOffsetTable;

    // Iterate over savepoint of this field
    for(auto fileOffsetIt = it->begin(); fileOffsetIt != it->end(); ++fileOffsetIt)
      fieldOffsetTable.push_back(FileOffsetType{fileOffsetIt->at(0), fileOffsetIt->at(1)});

    fieldTable_[it.key()] = fieldOffsetTable;
  }
}

void BinaryArchive::writeMetaDataToJson() {
  if(mode_ == OpenModeKind::Read)
    return;

  boost::filesystem::path filename = directory_ / Archive::ArchiveMetaDataFile;
  LOG(INFO) << "Update MetaData for BinaryArchive";

  json_.clear();

  // Tag versions
  json_["serialbox_version"] =
      100 * SERIALBOX_VERSION_MAJOR + 10 * SERIALBOX_VERSION_MINOR + SERIALBOX_VERSION_PATCH;
  json_["archive_name"] = BinaryArchive::Name;
  json_["archive_version"] = BinaryArchive::Version;

  // FieldsTable
  for(auto it = fieldTable_.begin(), end = fieldTable_.end(); it != end; ++it) {
    for(unsigned int id = 0; id < it->second.size(); ++id)
      json_["fields_table"][it->first].push_back({it->second[id].offset, it->second[id].checksum});
  }

  // Write metaData to disk (just overwrite the file, we assume that there is never more than one
  // Archive per data set and thus our in-memory copy is always the up-to-date one)
  std::ofstream fs(filename.string(), std::ios::out | std::ios::trunc);
  fs << json_.dump(4) << std::endl;
  fs.close();
}

BinaryArchive::BinaryArchive(OpenModeKind mode, const std::string& directory,
                             const std::string& prefix)
    : mode_(mode), directory_(directory), prefix_(prefix), json_() {

  LOG(INFO) << "Creating BinaryArchive (mode = " << mode_ << ") from directory " << directory_;

  try {
    bool isDir = boost::filesystem::is_directory(directory_);

    switch(mode_) {
    // We are reading, the directory needs to exist
    case OpenModeKind::Read:
      if(!isDir)
        throw Exception("no such directory: '%s'", directory_.string());
      break;
    // We are writing, the directory has to be empty
    case OpenModeKind::Write:
      if(isDir && !boost::filesystem::is_empty(directory_))
        throw Exception("directory '%s' is not empty", directory_.string());
    // We are appending, create directories if it they don't exist
    case OpenModeKind::Append:
      if(!isDir)
        boost::filesystem::create_directories(directory_);
      break;
    }
  } catch(boost::filesystem::filesystem_error& e) {
    throw Exception(e.what());
  }

  readMetaDataFromJson();
}

void BinaryArchive::updateMetaData() { writeMetaDataToJson(); }

//===------------------------------------------------------------------------------------------===//
//     Writing
//===------------------------------------------------------------------------------------------===//

FieldID BinaryArchive::write(StorageView& storageView, const std::string& field) throw(Exception) {
  if(mode_ == OpenModeKind::Read)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Write' or 'Append'");

  LOG(INFO) << "Attempting to write field \"" << field << "\" to BinaryArchive ...";

  boost::filesystem::path filename(directory_ / (prefix_ + "_" + field + ".dat"));
  std::ofstream fs;

  // Create binary data buffer
  try {
    std::size_t sizeInBytes = storageView.sizeInBytes();
    LOG(INFO) << "Resizing binary buffer to " << sizeInBytes << " bytes from " << binaryData_.size()
              << " bytes";

    binaryData_.resize(sizeInBytes);
  } catch(std::bad_alloc&) {
    throw Exception("out of memory");
  }

  Byte* dataPtr = binaryData_.data();
  const int bytesPerElement = storageView.bytesPerElement();

  // Copy field into contiguous memory
  if(storageView.isMemCopyable()) {
    std::memcpy(dataPtr, storageView.originPtr(), binaryData_.size());
  } else {
    for(auto it = storageView.begin(), end = storageView.end(); it != end;
        ++it, dataPtr += bytesPerElement)
      std::memcpy(dataPtr, it.ptr(), bytesPerElement);
  }

  // Compute hash
  std::string checksum(SHA256::hash(binaryData_.data(), binaryData_.size()));

  // Check if field already exists
  auto it = fieldTable_.find(field);
  FieldID fieldID{field, 0};

  // Field does exists
  if(it != fieldTable_.end()) {
    FieldOffsetTable& fieldOffsetTable = it->second;

    // Check if field has already been serialized by comparing the checksum
    for(std::size_t i = 0; i < fieldOffsetTable.size(); ++i)
      if(checksum == fieldOffsetTable[i].checksum) {
        LOG(INFO) << "Field \"" << field << "\" already serialized (id = " << i << "). Stopping";
        fieldID.id = i;
        return fieldID;
      }

    // Append field at the end
    fs.open(filename.string(), std::ofstream::out | std::ofstream::binary | std::ofstream::app);
    auto offset = fs.tellp();
    fieldID.id = fieldOffsetTable.size();
    fieldOffsetTable.push_back(FileOffsetType{offset, checksum});

    LOG(INFO) << "Appending field \"" << fieldID.name << "\" (id = " << fieldID.id << ") to "
              << filename.filename();
  }
  // Field does not exist, create new file and append data
  else {
    fs.open(filename.string(), std::ofstream::out | std::ofstream::binary | std::ofstream::trunc);
    fieldID.id = 0;

    fieldTable_.insert(
        FieldTable::value_type(fieldID.name, FieldOffsetTable(1, FileOffsetType{0, checksum})));

    LOG(INFO) << "Creating new file " << filename.filename() << " for field \"" << fieldID.name
              << "\" (id = " << fieldID.id << ")";
  }

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename.string());

  // Write binaryData to disk
  fs.write(binaryData_.data(), binaryData_.size());
  fs.close();

  updateMetaData();

  LOG(INFO) << "Successfully wrote field \"" << fieldID.name << "\" (id = " << fieldID.id << ") to "
            << filename.filename();
  return fieldID;
}

//===------------------------------------------------------------------------------------------===//
//     Reading
//===------------------------------------------------------------------------------------------===//

void BinaryArchive::read(StorageView& storageView, const FieldID& fieldID) throw(Exception) {
  if(mode_ != OpenModeKind::Read)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Read'");

  LOG(INFO) << "Attempting to read field \"" << fieldID.name << "\" (id = " << fieldID.id
            << ") via BinaryArchive ... ";

  // Check if field exists
  auto it = fieldTable_.find(fieldID.name);
  if(it == fieldTable_.end())
    throw Exception("no field '%s' registered in BinaryArchive", fieldID.name);

  const FieldOffsetTable& fieldOffsetTable = it->second;

  // Check if id is valid
  if(fieldID.id >= fieldOffsetTable.size())
    throw Exception("invalid id '%i' of field '%s'", fieldID.id, fieldID.name);

  // Create binary data buffer
  try {
    std::size_t sizeInBytes = storageView.sizeInBytes();
    LOG(INFO) << "Resizing binary buffer to " << sizeInBytes << " bytes from " << binaryData_.size()
              << " bytes";

    binaryData_.resize(sizeInBytes);
  } catch(std::bad_alloc&) {
    throw Exception("out of memory");
  }

  // Open file & read into binary buffer
  std::string filename((directory_ / (prefix_ + "_" + fieldID.name + ".dat")).string());
  std::ifstream fs(filename, std::ios::binary);

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename);

  // Set position in the steram
  auto offset = fieldOffsetTable[fieldID.id].offset;
  fs.seekg(offset);

  // Read data into contiguous memory
  fs.read(binaryData_.data(), binaryData_.size());
  fs.close();

  Byte* dataPtr = binaryData_.data();
  const int bytesPerElement = storageView.bytesPerElement();

  // Compute hash and compare
  std::string checksum(SHA256::hash(binaryData_.data(), binaryData_.size()));

  if(checksum != fieldOffsetTable[fieldID.id].checksum)
    throw Exception("hashsum mismatch for field '%s' at id '%i'", fieldID.name, fieldID.id);

  // Copy contiguous memory into field
  if(storageView.isMemCopyable()) {
    std::memcpy(storageView.originPtr(), dataPtr, binaryData_.size());
  } else {
    for(auto it = storageView.begin(), end = storageView.end(); it != end;
        ++it, dataPtr += bytesPerElement)
      std::memcpy(it.ptr(), dataPtr, bytesPerElement);
  }
  LOG(INFO) << "Successfully read field \"" << fieldID.name << "\" (id = " << fieldID.id << ")";
}

std::ostream& BinaryArchive::toStream(std::ostream& stream) const {
  stream << "BinaryArchive = {\n";
  stream << "  directory: " << directory_.string() << "\n";
  stream << "  mode: " << mode_ << "\n";
  stream << "  prefix: " << prefix_ << "\n";
  stream << "  fieldsTable = {\n";
  for(auto it = fieldTable_.begin(), end = fieldTable_.end(); it != end; ++it) {
    stream << "    " << it->first << " = {\n";
    for(std::size_t id = 0; id < it->second.size(); ++id)
      stream << "      [ " << it->second[id].offset << ", " << it->second[id].checksum << " ]\n";
    stream << "    }\n";
  }
  stream << "  }\n";
  stream << "}\n";
  return stream;
}

std::unique_ptr<Archive> BinaryArchive::create(OpenModeKind mode, const std::string& directory,
                                               const std::string& prefix) {
  return make_unique<BinaryArchive>(mode, directory, prefix);
}

SERIALBOX_REGISTER_ARCHIVE(BinaryArchive, BinaryArchive::create)

} // namespace serialbox
