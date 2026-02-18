//===-- serialbox/core/archive/ZarrArchive.cpp --------------------------------------*- C++ -*-===//
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

#include "serialbox/core/archive/ZarrArchive.h"
#include "serialbox/core/Logging.h"
#include "serialbox/core/Unreachable.h"
#include "serialbox/core/Version.h"
#include <boost/algorithm/string.hpp>
#include <cstring>
#include <fstream>
#include <memory>
#include <sstream>

namespace serialbox {

//===------------------------------------------------------------------------------------------===//
//     Constants
//===------------------------------------------------------------------------------------------===//

const std::string ZarrArchive::Name = "Zarr";

const int ZarrArchive::Version = 0;

//===------------------------------------------------------------------------------------------===//
//     Helpers
//===------------------------------------------------------------------------------------------===//

char ZarrArchive::nativeEndianChar() {
  const uint16_t test = 0x0102;
  return (*reinterpret_cast<const uint8_t*>(&test) == 0x01) ? '>' : '<';
}

std::string ZarrArchive::typeIDtoZarrDtype(TypeID type) {
  const char endian = nativeEndianChar();
  switch(type) {
  case TypeID::Boolean:
    return "|b1";
  case TypeID::Int32:
    return std::string(1, endian) + "i4";
  case TypeID::Int64:
    return std::string(1, endian) + "i8";
  case TypeID::Float32:
    return std::string(1, endian) + "f4";
  case TypeID::Float64:
    return std::string(1, endian) + "f8";
  default:
    throw Exception("ZarrArchive: cannot convert type '%s' to Zarr dtype",
                    TypeUtil::toString(type));
  }
}

std::vector<Byte> ZarrArchive::storageViewToBuffer(const StorageView& storageView) {
  const int bytesPerElement = storageView.bytesPerElement();
  std::vector<Byte> buffer(storageView.sizeInBytes());
  Byte* dst = buffer.data();

  if(storageView.isMemCopyable()) {
    std::memcpy(dst, storageView.originPtr(), buffer.size());
  } else {
    for(auto it = storageView.begin(), end = storageView.end(); it != end;
        ++it, dst += bytesPerElement)
      std::memcpy(dst, it.ptr(), bytesPerElement);
  }
  return buffer;
}

void ZarrArchive::bufferToStorageView(const std::vector<Byte>& buffer, StorageView& storageView) {
  const int bytesPerElement = storageView.bytesPerElement();
  const Byte* src = buffer.data();

  if(storageView.isMemCopyable()) {
    std::memcpy(storageView.originPtr(), src, buffer.size());
  } else {
    for(auto it = storageView.begin(), end = storageView.end(); it != end;
        ++it, src += bytesPerElement)
      std::memcpy(it.ptr(), src, bytesPerElement);
  }
}

//===------------------------------------------------------------------------------------------===//
//     Path helpers
//===------------------------------------------------------------------------------------------===//

std::filesystem::path ZarrArchive::fieldDirectory(const std::string& field) const {
  return directory_ / (prefix_ + "_" + field + ".zarr");
}

std::filesystem::path ZarrArchive::chunkFile(const std::string& field, int saveId,
                                             std::size_t numDataDims) const {
  // Zarr v2 chunk key: indices separated by '.'
  // First index is the save id; the remaining numDataDims indices are all 0.
  std::string key = std::to_string(saveId);
  for(std::size_t i = 0; i < numDataDims; ++i)
    key += ".0";
  return fieldDirectory(field) / key;
}

//===------------------------------------------------------------------------------------------===//
//     Zarr metadata
//===------------------------------------------------------------------------------------------===//

void ZarrArchive::writeZarrayMetadata(const std::filesystem::path& fieldDir,
                                      const StorageView& storageView, int numSaves) const {
  // Build active dims (skip size-0 dims)
  std::vector<int> activeDims;
  for(int d : storageView.dims())
    if(d > 0)
      activeDims.push_back(d);

  // Shape: [num_saves, d0, d1, ...]
  json::json shape = json::json::array();
  shape.push_back(numSaves);
  for(int d : activeDims)
    shape.push_back(d);

  // Chunks: [1, d0, d1, ...] — one chunk per save
  json::json chunks = json::json::array();
  chunks.push_back(1);
  for(int d : activeDims)
    chunks.push_back(d);

  json::json zarray;
  zarray["zarr_format"] = 2;
  zarray["shape"] = shape;
  zarray["chunks"] = chunks;
  zarray["dtype"] = typeIDtoZarrDtype(storageView.type());
  zarray["compressor"] = nullptr;
  zarray["fill_value"] = 0;
  zarray["order"] = "C";
  zarray["filters"] = nullptr;

  std::ofstream fs(fieldDir / ".zarray", std::ios::out | std::ios::trunc);
  if(!fs.is_open())
    throw Exception("ZarrArchive: cannot open .zarray metadata file in '%s'",
                    fieldDir.string());
  fs << zarray.dump(2) << "\n";
}

json::json ZarrArchive::readZarrayMetadata(const std::filesystem::path& fieldDir) const {
  std::filesystem::path zarrayPath = fieldDir / ".zarray";
  if(!std::filesystem::exists(zarrayPath))
    throw Exception("ZarrArchive: .zarray not found in '%s'", fieldDir.string());

  std::ifstream fs(zarrayPath.string());
  json::json j;
  fs >> j;
  return j;
}

//===------------------------------------------------------------------------------------------===//
//     Constructor
//===------------------------------------------------------------------------------------------===//

ZarrArchive::ZarrArchive(OpenModeKind mode, const std::string& directory,
                         const std::string& prefix)
    : mode_(mode), directory_(directory), prefix_(prefix) {

  LOG(info) << "Creating ZarrArchive (mode = " << mode_ << ") from directory " << directory_;

  metaDatafile_ = directory_ / ("ArchiveMetaData-" + prefix_ + ".json");

  try {
    bool isDir = std::filesystem::is_directory(directory_);

    switch(mode_) {
    case OpenModeKind::Read:
      if(!isDir)
        throw Exception("no such directory: '%s'", directory_.string());
      break;
    case OpenModeKind::Write:
    case OpenModeKind::Append:
      if(!isDir)
        std::filesystem::create_directories(directory_);
      break;
    }
  } catch(std::filesystem::filesystem_error& e) {
    throw Exception(e.what());
  }

  readMetaDataFromJson();

  if(mode_ == OpenModeKind::Write)
    clear();
}

//===------------------------------------------------------------------------------------------===//
//     MetaData
//===------------------------------------------------------------------------------------------===//

void ZarrArchive::updateMetaData() { writeMetaDataToJson(); }

void ZarrArchive::writeMetaDataToJson() {
  LOG(info) << "Update MetaData of Zarr Archive";

  json_.clear();

  json_["serialbox_version"] =
      100 * SERIALBOX_VERSION_MAJOR + 10 * SERIALBOX_VERSION_MINOR + SERIALBOX_VERSION_PATCH;
  json_["archive_name"] = ZarrArchive::Name;
  json_["archive_version"] = ZarrArchive::Version;

  for(auto it = fieldMap_.begin(), end = fieldMap_.end(); it != end; ++it)
    json_["field_map"][it->first] = it->second;

  std::ofstream fs(metaDatafile_.string(), std::ios::out | std::ios::trunc);
  if(!fs.is_open())
    throw Exception("ZarrArchive: cannot open file: %s", metaDatafile_.string());

  fs << json_.dump(2) << "\n";
  fs.close();
}

void ZarrArchive::readMetaDataFromJson() {
  LOG(info) << "Reading MetaData for Zarr archive ...";

  if(!std::filesystem::exists(metaDatafile_)) {
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

  if(!Version::isCompatible(serialboxVersion))
    throw Exception("serialbox version of Zarr archive (%s) is not compatible with the version "
                    "of the library (%s)",
                    Version::toString(serialboxVersion), SERIALBOX_VERSION_STRING);

  if(archiveName != ZarrArchive::Name)
    throw Exception("archive is not a Zarr archive");

  if(archiveVersion > ZarrArchive::Version)
    throw Exception("Zarr archive version (%i) does not match the version of the library (%i)",
                    archiveVersion, ZarrArchive::Version);

  if(json_.count("field_map")) {
    fieldMap_.clear();
    for(auto it = json_["field_map"].begin(); it != json_["field_map"].end(); ++it)
      fieldMap_.insert({it.key(), static_cast<int>(it.value())});
  }
}

//===------------------------------------------------------------------------------------------===//
//     Writing
//===------------------------------------------------------------------------------------------===//

FieldID ZarrArchive::write(const StorageView& storageView, const std::string& field,
                           const std::shared_ptr<FieldMetainfoImpl> info) {
  if(mode_ == OpenModeKind::Read)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Write' or 'Append'");

  LOG(info) << "Attempting to write field \"" << field << "\" to Zarr archive ...";

  // Collect active (non-zero) data dimensions
  std::vector<int> activeDims;
  for(int d : storageView.dims())
    if(d > 0)
      activeDims.push_back(d);
  const std::size_t numDataDims = activeDims.size();

  std::filesystem::path fieldDir = fieldDirectory(field);

  FieldID fieldID{field, 0};
  auto it = fieldMap_.find(field);

  if(it != fieldMap_.end()) {
    // Subsequent save — increment id and update .zarray shape
    it->second++;
    fieldID.id = it->second;
    writeZarrayMetadata(fieldDir, storageView, fieldID.id + 1);
  } else {
    // First save — create the Zarr array directory and initial metadata
    try {
      std::filesystem::create_directories(fieldDir);
    } catch(std::filesystem::filesystem_error& e) {
      throw Exception(e.what());
    }
    fieldMap_.insert({field, 0});
    writeZarrayMetadata(fieldDir, storageView, 1);
  }

  // Serialize data to a contiguous buffer and write the chunk file
  std::vector<Byte> buffer = storageViewToBuffer(storageView);

  std::filesystem::path chunkPath = chunkFile(field, fieldID.id, numDataDims);
  std::ofstream fs(chunkPath.string(), std::ios::out | std::ios::binary | std::ios::trunc);
  if(!fs.is_open())
    throw Exception("ZarrArchive: cannot open chunk file: %s", chunkPath.string());
  fs.write(buffer.data(), static_cast<std::streamsize>(buffer.size()));
  fs.close();

  // Update archive-level metadata
  updateMetaData();

  LOG(info) << "Successfully wrote field \"" << fieldID.name << "\" (id = " << fieldID.id
            << ") to " << chunkPath.filename();
  return fieldID;
}

void ZarrArchive::writeToFile(std::string zarrPath, const StorageView& storageView,
                              const std::string& field) {
  std::filesystem::path fieldDir(zarrPath);

  // Collect active data dimensions
  std::vector<int> activeDims;
  for(int d : storageView.dims())
    if(d > 0)
      activeDims.push_back(d);
  const std::size_t numDataDims = activeDims.size();

  // Create directory
  std::filesystem::create_directories(fieldDir);

  // Write .zarray (single save, no leading time dimension)
  json::json shape = json::json::array();
  json::json chunks = json::json::array();
  for(int d : activeDims) {
    shape.push_back(d);
    chunks.push_back(d);
  }

  json::json zarray;
  zarray["zarr_format"] = 2;
  zarray["shape"] = shape;
  zarray["chunks"] = chunks;
  zarray["dtype"] = typeIDtoZarrDtype(storageView.type());
  zarray["compressor"] = nullptr;
  zarray["fill_value"] = 0;
  zarray["order"] = "C";
  zarray["filters"] = nullptr;

  std::ofstream mfs((fieldDir / ".zarray").string(), std::ios::out | std::ios::trunc);
  if(!mfs.is_open())
    throw Exception("ZarrArchive: cannot create .zarray in '%s'", fieldDir.string());
  mfs << zarray.dump(2) << "\n";
  mfs.close();

  // Build chunk key: "0.0...0"
  std::string key = "0";
  for(std::size_t i = 1; i < numDataDims; ++i)
    key += ".0";

  std::vector<Byte> buffer = storageViewToBuffer(storageView);
  std::filesystem::path chunkPath = fieldDir / key;
  std::ofstream fs(chunkPath.string(), std::ios::out | std::ios::binary | std::ios::trunc);
  if(!fs.is_open())
    throw Exception("ZarrArchive: cannot open chunk file: %s", chunkPath.string());
  fs.write(buffer.data(), static_cast<std::streamsize>(buffer.size()));
  fs.close();
}

//===------------------------------------------------------------------------------------------===//
//     Reading
//===------------------------------------------------------------------------------------------===//

void ZarrArchive::read(StorageView& storageView, const FieldID& fieldID,
                       std::shared_ptr<FieldMetainfoImpl> info) const {
  LOG(info) << "Attempting to read field \"" << fieldID.name << "\" (id = " << fieldID.id
            << ") via ZarrArchive ...";

  auto it = fieldMap_.find(fieldID.name);
  if(it == fieldMap_.end())
    throw Exception("no field '%s' registered in ZarrArchive", fieldID.name);

  if(fieldID.id > it->second)
    throw Exception("invalid id '%i' of field '%s'", fieldID.id, fieldID.name);

  // Collect active data dimensions
  std::size_t numDataDims = 0;
  for(int d : storageView.dims())
    if(d > 0)
      numDataDims++;

  std::filesystem::path chunkPath = chunkFile(fieldID.name, fieldID.id, numDataDims);
  if(!std::filesystem::exists(chunkPath))
    throw Exception("ZarrArchive: chunk file not found: %s", chunkPath.string());

  const std::size_t expectedBytes = storageView.sizeInBytes();
  std::vector<Byte> buffer(expectedBytes);

  std::ifstream fs(chunkPath.string(), std::ios::in | std::ios::binary);
  if(!fs.is_open())
    throw Exception("ZarrArchive: cannot open chunk file: %s", chunkPath.string());
  fs.read(buffer.data(), static_cast<std::streamsize>(expectedBytes));
  fs.close();

  bufferToStorageView(buffer, storageView);

  LOG(info) << "Successfully read field \"" << fieldID.name << "\" (id = " << fieldID.id << ")";
}

void ZarrArchive::readFromFile(std::string zarrPath, StorageView& storageView,
                               const std::string& field) {
  std::filesystem::path fieldDir(zarrPath);

  if(!std::filesystem::exists(fieldDir))
    throw Exception("ZarrArchive: Zarr store not found: %s", zarrPath);

  // Collect active data dimensions
  std::size_t numDataDims = 0;
  for(int d : storageView.dims())
    if(d > 0)
      numDataDims++;

  // Build chunk key for a single-save store: "0.0...0"
  std::string key = "0";
  for(std::size_t i = 1; i < numDataDims; ++i)
    key += ".0";

  std::filesystem::path chunkPath = fieldDir / key;
  if(!std::filesystem::exists(chunkPath))
    throw Exception("ZarrArchive: chunk file not found: %s", chunkPath.string());

  const std::size_t expectedBytes = storageView.sizeInBytes();
  std::vector<Byte> buffer(expectedBytes);

  std::ifstream fs(chunkPath.string(), std::ios::in | std::ios::binary);
  if(!fs.is_open())
    throw Exception("ZarrArchive: cannot open chunk file: %s", chunkPath.string());
  fs.read(buffer.data(), static_cast<std::streamsize>(expectedBytes));
  fs.close();

  bufferToStorageView(buffer, storageView);
}

//===------------------------------------------------------------------------------------------===//
//     Misc
//===------------------------------------------------------------------------------------------===//

void ZarrArchive::clear() {
  std::filesystem::directory_iterator end;
  for(std::filesystem::directory_iterator it(directory_); it != end; ++it) {
    const auto& p = it->path();
    if(std::filesystem::is_directory(p) &&
       boost::algorithm::starts_with(p.filename().string(), prefix_ + "_") &&
       p.extension() == ".zarr") {
      std::filesystem::remove_all(p);
    }
  }
  fieldMap_.clear();
}

std::ostream& ZarrArchive::toStream(std::ostream& stream) const {
  stream << "ZarrArchive = {\n";
  stream << "  directory: " << directory_.string() << "\n";
  stream << "  mode: " << mode_ << "\n";
  stream << "  prefix: " << prefix_ << "\n";
  stream << "  fieldMap = {\n";
  for(auto it = fieldMap_.begin(), end = fieldMap_.end(); it != end; ++it)
    stream << "    " << it->first << ": " << it->second << "\n";
  stream << "  }\n";
  stream << "}\n";
  return stream;
}

std::unique_ptr<Archive> ZarrArchive::create(OpenModeKind mode, const std::string& directory,
                                             const std::string& prefix) {
  return std::make_unique<ZarrArchive>(mode, directory, prefix);
}

} // namespace serialbox
