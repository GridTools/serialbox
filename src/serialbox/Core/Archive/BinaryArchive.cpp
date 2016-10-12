//===-- serialbox/Core/Archive/BinaryArchive.cpp ------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/Archive/BinaryArchive.h"
#include "serialbox/Core/Logging.h"
#include "serialbox/Core/STLExtras.h"
#include "serialbox/Core/Version.h"
#include <boost/algorithm/string.hpp>
#include <fstream>

namespace serialbox {

const std::string BinaryArchive::Name = "Binary";

const int BinaryArchive::Version = 0;

BinaryArchive::~BinaryArchive() {}

void BinaryArchive::readMetaDataFromJson() {
  LOG(info) << "Reading MetaData for BinaryArchive ... ";

  // Check if metaData file exists
  if(!boost::filesystem::exists(metaDatafile_)) {
    if(mode_ != OpenModeKind::Read)
      return;
    throw Exception("archive meta data not found in directory '%s'", directory_.string());
  }

  std::ifstream fs(metaDatafile_.string(), std::ios::in);
  fs >> json_;
  fs.close();

  int serialboxVersion = json_["serialbox_version"];
  std::string archiveName = json_["archive_name"];
  int archiveVersion = json_["archive_version"];
  std::string hashAlgorithm = json_["hash_algorithm"];

  // Check consistency
  if(!Version::match(serialboxVersion))
    throw Exception("serialbox version of binary archive (%s) does not match the version "
                    "of the library (%s)",
                    Version::toString(serialboxVersion), SERIALBOX_VERSION_STRING);

  if(archiveName != BinaryArchive::Name)
    throw Exception("archive is not a binary archive");

  if(archiveVersion != BinaryArchive::Version)
    throw Exception("binary archive version (%s) does not match the version of the library (%s)",
                    archiveVersion, BinaryArchive::Version);

  // Appending with different hash algorithms doesn't work
  if(mode_ == OpenModeKind::Append && hashAlgorithm != HashAlgorithm::Name)
    throw Exception("binary archive uses hash algorithm '%s' but library was compiled with '%s'",
                    hashAlgorithm, HashAlgorithm::Name);

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
  LOG(info) << "Update MetaData of BinaryArchive";

  json_.clear();

  // Tag versions
  json_["serialbox_version"] =
      100 * SERIALBOX_VERSION_MAJOR + 10 * SERIALBOX_VERSION_MINOR + SERIALBOX_VERSION_PATCH;
  json_["archive_name"] = BinaryArchive::Name;
  json_["archive_version"] = BinaryArchive::Version;
  json_["hash_algorithm"] = HashAlgorithm::Name;

  // FieldsTable
  for(auto it = fieldTable_.begin(), end = fieldTable_.end(); it != end; ++it) {
    for(unsigned int id = 0; id < it->second.size(); ++id)
      json_["fields_table"][it->first].push_back({it->second[id].offset, it->second[id].checksum});
  }

  // Write metaData to disk (just overwrite the file, we assume that there is never more than one
  // Archive per data set and thus our in-memory copy is always the up-to-date one)
  std::ofstream fs(metaDatafile_.string(), std::ios::out | std::ios::trunc);

  if(!fs.is_open())
    throw Exception("cannot open file: %s", metaDatafile_);

  fs << json_.dump(2) << std::endl;
  fs.close();
}

BinaryArchive::BinaryArchive(OpenModeKind mode, const std::string& directory,
                             const std::string& prefix, bool skipMetaData)
    : mode_(mode), directory_(directory), prefix_(prefix), json_() {

  LOG(info) << "Creating BinaryArchive (mode = " << mode_ << ") from directory " << directory_;

  metaDatafile_ = directory_ / ("ArchiveMetaData-" + prefix_ + ".json");

  try {
    bool isDir = boost::filesystem::is_directory(directory_);

    switch(mode_) {
    // We are reading, the directory needs to exist
    case OpenModeKind::Read:
      if(!isDir)
        throw Exception("no such directory: '%s'", directory_.string());
      break;
    // We are writing or appending, create directories if it they don't exist
    case OpenModeKind::Write:
    case OpenModeKind::Append:
      if(!isDir)
        boost::filesystem::create_directories(directory_);
      break;
    }
  } catch(boost::filesystem::filesystem_error& e) {
    throw Exception(e.what());
  }

  if(!skipMetaData)
    readMetaDataFromJson();

  // Remove all files
  if(mode_ == OpenModeKind::Write)
    clear();
}

void BinaryArchive::updateMetaData() { writeMetaDataToJson(); }

//===------------------------------------------------------------------------------------------===//
//     Writing
//===------------------------------------------------------------------------------------------===//

FieldID BinaryArchive::write(const StorageView& storageView,
                             const std::string& field) throw(Exception) {
  if(mode_ == OpenModeKind::Read)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Write' or 'Append'");

  LOG(info) << "Attempting to write field \"" << field << "\" to BinaryArchive ...";

  boost::filesystem::path filename(directory_ / (prefix_ + "_" + field + ".dat"));
  std::ofstream fs;

  // Create binary data buffer
  std::size_t sizeInBytes = storageView.sizeInBytes();
  std::vector<Byte> binaryData(sizeInBytes);

  Byte* dataPtr = binaryData.data();
  const int bytesPerElement = storageView.bytesPerElement();

  // Copy field into contiguous memory
  if(storageView.isMemCopyable()) {
    std::memcpy(dataPtr, storageView.originPtr(), sizeInBytes);
  } else {
    for(auto it = storageView.begin(), end = storageView.end(); it != end;
        ++it, dataPtr += bytesPerElement)
      std::memcpy(dataPtr, it.ptr(), bytesPerElement);
  }

  // Compute hash
  std::string checksum(HashAlgorithm::hash(binaryData.data(), sizeInBytes));

  // Check if field already exists
  auto it = fieldTable_.find(field);
  FieldID fieldID{field, 0};

  // Field does exists
  if(it != fieldTable_.end()) {
    FieldOffsetTable& fieldOffsetTable = it->second;

    // Check if field has already been serialized by comparing the checksum
    for(std::size_t i = 0; i < fieldOffsetTable.size(); ++i)
      if(checksum == fieldOffsetTable[i].checksum) {
        LOG(info) << "Field \"" << field << "\" already serialized (id = " << i << "). Stopping";
        fieldID.id = i;
        return fieldID;
      }

    // Append field at the end
    fs.open(filename.string(), std::ofstream::out | std::ofstream::binary | std::ofstream::app);
#ifdef SERIALBOX_COMPILER_MSVC
    fs.seekp(0, fs.end);
#endif
    auto offset = fs.tellp();
    fieldID.id = fieldOffsetTable.size();
    fieldOffsetTable.push_back(FileOffsetType{offset, checksum});

    LOG(info) << "Appending field \"" << fieldID.name << "\" (id = " << fieldID.id << ") to "
              << filename.filename();
  }
  // Field does not exist, create new file and append data
  else {
    fs.open(filename.string(), std::ofstream::out | std::ofstream::binary | std::ofstream::trunc);
    fieldID.id = 0;

    fieldTable_.insert(
        FieldTable::value_type(fieldID.name, FieldOffsetTable(1, FileOffsetType{0, checksum})));

    LOG(info) << "Creating new file " << filename.filename() << " for field \"" << fieldID.name
              << "\" (id = " << fieldID.id << ")";
  }

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename.string());

  // Write binaryData to disk
  fs.write(binaryData.data(), sizeInBytes);
  fs.close();

  updateMetaData();

  LOG(info) << "Successfully wrote field \"" << fieldID.name << "\" (id = " << fieldID.id << ") to "
            << filename.filename();
  return fieldID;
}

//===------------------------------------------------------------------------------------------===//
//     Reading
//===------------------------------------------------------------------------------------------===//

void BinaryArchive::read(StorageView& storageView, const FieldID& fieldID) const throw(Exception) {
  if(mode_ != OpenModeKind::Read)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Read'");

  LOG(info) << "Attempting to read field \"" << fieldID.name << "\" (id = " << fieldID.id
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
  std::size_t sizeInBytes = storageView.sizeInBytes();
  std::vector<Byte> binaryData(sizeInBytes);

  // Open file & read into binary buffer
  std::string filename((directory_ / (prefix_ + "_" + fieldID.name + ".dat")).string());
  std::ifstream fs(filename, std::ios::binary);

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename);

  // Set position in the steram
  auto offset = fieldOffsetTable[fieldID.id].offset;
  fs.seekg(offset);

  // Read data into contiguous memory
  fs.read(binaryData.data(), sizeInBytes);
  fs.close();

  Byte* dataPtr = binaryData.data();
  const int bytesPerElement = storageView.bytesPerElement();

  // Copy contiguous memory into field
  if(storageView.isMemCopyable()) {
    std::memcpy(storageView.originPtr(), dataPtr, sizeInBytes);
  } else {
    for(auto it = storageView.begin(), end = storageView.end(); it != end;
        ++it, dataPtr += bytesPerElement)
      std::memcpy(it.ptr(), dataPtr, bytesPerElement);
  }
  LOG(info) << "Successfully read field \"" << fieldID.name << "\" (id = " << fieldID.id << ")";
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

void BinaryArchive::clear() {
  boost::filesystem::directory_iterator end;
  for(boost::filesystem::directory_iterator it(directory_); it != end; ++it) {
    if(boost::filesystem::is_regular_file(it->path()) &&
       boost::algorithm::starts_with(it->path().filename().string(), prefix_ + "_") &&
       boost::filesystem::extension(it->path()) == ".dat") {

      if(!boost::filesystem::remove(it->path()))
        LOG(warning) << "BinaryArchive: cannot remove file " << it->path();
    }
  }
  clearFieldTable();
}

void BinaryArchive::clearFieldTable() {
  fieldTable_.clear();
  json_.clear();
}

std::unique_ptr<Archive> BinaryArchive::create(OpenModeKind mode, const std::string& directory,
                                               const std::string& prefix) {
  return std::make_unique<BinaryArchive>(mode, directory, prefix, false);
}

} // namespace serialbox
