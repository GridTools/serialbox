//===-- serialbox/core/archive/BinaryArchive.cpp ------------------------------------*- C++ -*-===//
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

#include "serialbox/core/archive/BinaryArchive.h"
#include "serialbox/core/Logging.h"
#include "serialbox/core/STLExtras.h"
#include "serialbox/core/Version.h"
#include "serialbox/core/hash/HashFactory.h"
#include <boost/algorithm/string.hpp>
#include <fstream>

namespace serialbox {

//===------------------------------------------------------------------------------------------===//
//     BinaryBuffer
//===------------------------------------------------------------------------------------------===//

/// \brief Contiguous buffer with support for sliced loading
class BinaryBuffer {
public:
  /// \brief Allocate the buffer
  BinaryBuffer(const StorageView& storageView) {
    const auto& slice = storageView.getSlice();

    if(slice.empty()) {
      buffer_.resize(storageView.sizeInBytes());
      offset_ = 0;
    } else {
      const auto& dims = storageView.dims();
      const auto& triple = slice.sliceTriples().back();
      const int bytesPerElement = storageView.bytesPerElement();

      // Allocate a buffer which can be efficently loaded. The buffer will treat the
      // dimensions dim_{1}, ..., dim_{N-1} as full while last the dimension dim_{N} as sliced but
      // without incorporating the step. This is necessary as we only want to call ::write once.

      // Compute dimensions
      dims_ = dims;
      dims_.back() = triple.stop - triple.start;

      // Compute strides (col-major)
      strides_.resize(dims_.size());

      int stride = 1;
      strides_[0] = stride;

      for(int i = 1; i < dims_.size(); ++i) {
        stride *= dims_[i - 1];
        strides_[i] = stride;
      }

      // Compute size
      std::size_t size = 1;
      for(std::size_t i = 0; i < dims_.size(); ++i)
        size *= (dims_[i] == 0 ? 1 : dims_[i]);

      // Compute initial offset in bytes
      offset_ = (strides_.back() * triple.start) * bytesPerElement;

      buffer_.resize(size * bytesPerElement);
    }
  }

  /// \brief Copy data from buffer to `storageView` while handling slicing
  void copyBufferToStorageView(StorageView& storageView) {
    const auto& slice = storageView.getSlice();

    if(slice.empty()) {
      Byte* dataPtr = buffer_.data();
      const int bytesPerElement = storageView.bytesPerElement();

      if(storageView.isMemCopyable()) {
        std::memcpy(storageView.originPtr(), dataPtr, buffer_.size());
      } else {
        for(auto it = storageView.begin(), end = storageView.end(); it != end;
            ++it, dataPtr += bytesPerElement)
          std::memcpy(it.ptr(), dataPtr, bytesPerElement);
      }

    } else {
      const int numDims = dims_.size();
      const auto& triples = slice.sliceTriples();
      const int bytesPerElement = storageView.bytesPerElement();
      Byte* dataPtr = buffer_.data();

      // Compute intial indices in the buffer
      std::vector<int> index(numDims);
      for(int i = 0; i < numDims - 1; ++i)
        index[i] = triples[i].start;
      index.back() = 0;

      // Iterate over the the storageView and the Buffer
      Byte* curPtr = buffer_.data();
      for(auto it = storageView.begin(), end = storageView.end(); it != end; ++it) {

        // Compute position of current element
        int pos = 0;
        for(int i = 0; i < numDims; ++i)
          pos += bytesPerElement * (strides_[i] * index[i]);
        curPtr = dataPtr + pos;

        // Memcopy the current elemment to the storageView
        std::memcpy(it.ptr(), curPtr, bytesPerElement);

        // Compute the index of the next element in the buffer
        for(int i = 0; i < numDims; ++i)
          if((index[i] += triples[i].step) < triples[i].stop)
            break;
          else
            index[i] = triples[i].start;
      }
    }
  }

  /// \brief Copy data from `storageView` to buffer
  void copyStorageViewToBuffer(const StorageView& storageView) {
    Byte* dataPtr = buffer_.data();
    const int bytesPerElement = storageView.bytesPerElement();

    if(storageView.isMemCopyable()) {
      std::memcpy(dataPtr, storageView.originPtr(), buffer_.size());
    } else {
      for(auto it = storageView.begin(), end = storageView.end(); it != end;
          ++it, dataPtr += bytesPerElement)
        std::memcpy(dataPtr, it.ptr(), bytesPerElement);
    }
  }

  /// \brief Get Buffer size
  std::size_t size() const noexcept { return buffer_.size(); }

  /// \brief Get pointer to the beginning of the buffer
  Byte* data() noexcept { return buffer_.data(); }
  const Byte* data() const noexcept { return buffer_.data(); }

  /// \brief Get initial offset of the data on disk in bytes
  std::size_t offset() const noexcept { return offset_; }

private:
  std::vector<Byte> buffer_;

  std::vector<int> strides_;
  std::vector<int> dims_;
  std::size_t offset_;
};

//===------------------------------------------------------------------------------------------===//
//     BinaryArchive
//===------------------------------------------------------------------------------------------===//

const std::string BinaryArchive::Name = "Binary";

const int BinaryArchive::Version = 0;

BinaryArchive::BinaryArchive(OpenModeKind mode, const std::string& directory,
                             const std::string& prefix, bool skipMetaData)
    : mode_(mode), directory_(directory), prefix_(prefix), json_() {

  LOG(info) << "Creating BinaryArchive (mode = " << mode_ << ") from directory " << directory_;

  metaDatafile_ = directory_ / ("ArchiveMetaData-" + prefix_ + ".json");
  hash_ = HashFactory::create(HashFactory::defaultHash());

  try {
    bool isDir = filesystem::is_directory(directory_);

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
        filesystem::create_directories(directory_);
      break;
    }
  } catch(filesystem::filesystem_error& e) {
    throw Exception(e.what());
  }

  if(!skipMetaData)
    readMetaDataFromJson();

  // Remove all files
  if(mode_ == OpenModeKind::Write)
    clear();
}

BinaryArchive::~BinaryArchive() {}

void BinaryArchive::readMetaDataFromJson() {
  LOG(info) << "Reading MetaData for BinaryArchive ... ";

  // Check if metaData file exists
  if(!filesystem::exists(metaDatafile_)) {
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
  if(!Version::isCompatible(serialboxVersion))
    throw Exception("serialbox version of binary archive (%s) does not match the version "
                    "of the library (%s)",
                    Version::toString(serialboxVersion), SERIALBOX_VERSION_STRING);

  if(archiveName != BinaryArchive::Name)
    throw Exception("archive is not a binary archive");

  if(archiveVersion != BinaryArchive::Version)
    throw Exception("binary archive version (%s) does not match the version of the library (%s)",
                    archiveVersion, BinaryArchive::Version);

  // Set the correct hash algorithm if we are not writing
  if(mode_ != OpenModeKind::Write)
    hash_ = HashFactory::create(hashAlgorithm);

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
  json_["hash_algorithm"] = hash_->name();

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

void BinaryArchive::updateMetaData() { writeMetaDataToJson(); }

//===------------------------------------------------------------------------------------------===//
//     Writing
//===------------------------------------------------------------------------------------------===//

FieldID BinaryArchive::write(const StorageView& storageView, const std::string& field,
                             const std::shared_ptr<FieldMetainfoImpl> info) {
  if(mode_ == OpenModeKind::Read)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Write' or 'Append'");

  LOG(info) << "Attempting to write field \"" << field << "\" to BinaryArchive ...";

  filesystem::path filename(directory_ / (prefix_ + "_" + field + ".dat"));
  std::ofstream fs;

  // Create binary data buffer
  BinaryBuffer binaryBuffer(storageView);
  binaryBuffer.copyStorageViewToBuffer(storageView);

  // Compute hash
  std::string checksum(hash_->hash(binaryBuffer.data(), binaryBuffer.size()));

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
    fs.open(filename.string(), std::ios::out | std::ios::binary | std::ios::trunc);
    fieldID.id = 0;

    fieldTable_.insert(
        FieldTable::value_type(fieldID.name, FieldOffsetTable(1, FileOffsetType{0, checksum})));

    LOG(info) << "Creating new file " << filename.filename() << " for field \"" << fieldID.name
              << "\" (id = " << fieldID.id << ")";
  }

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename.string());

  // Write binaryData to disk
  fs.write(binaryBuffer.data(), binaryBuffer.size());
  fs.close();

  updateMetaData();

  LOG(info) << "Successfully wrote field \"" << fieldID.name << "\" (id = " << fieldID.id << ") to "
            << filename.filename();
  return fieldID;
}

void BinaryArchive::writeToFile(std::string filename, const StorageView& storageView) {
  // Create binary data buffer
  BinaryBuffer binaryBuffer(storageView);
  binaryBuffer.copyStorageViewToBuffer(storageView);

  // Write data to disk
  std::ofstream fs(filename, std::ios::out | std::ios::binary | std::ios::trunc);

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename);

  fs.write(binaryBuffer.data(), binaryBuffer.size());
  fs.close();
}

//===------------------------------------------------------------------------------------------===//
//     Reading
//===------------------------------------------------------------------------------------------===//

void BinaryArchive::read(StorageView& storageView, const FieldID& fieldID,
                         std::shared_ptr<FieldMetainfoImpl> info) const {
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
  BinaryBuffer binaryBuffer(storageView);

  // Open file & read into binary buffer
  std::string filename((directory_ / (prefix_ + "_" + fieldID.name + ".dat")).string());
  std::ifstream fs(filename, std::ios::binary);

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename);

  // Set position in the stream
  auto offset = fieldOffsetTable[fieldID.id].offset + binaryBuffer.offset();
  fs.seekg(offset);

  // Read data into contiguous memory
  fs.read(binaryBuffer.data(), binaryBuffer.size());
  fs.close();

  binaryBuffer.copyBufferToStorageView(storageView);

  LOG(info) << "Successfully read field \"" << fieldID.name << "\" (id = " << fieldID.id << ")";
}

void BinaryArchive::readFromFile(std::string filename, StorageView& storageView) {
  filesystem::path filepath(filename);

  if(!filesystem::exists(filepath))
    throw Exception("cannot open %s: file does not exist", filepath);

  // Create binary data buffer
  BinaryBuffer binaryBuffer(storageView);

  std::ifstream fs(filepath.string(), std::ios::in | std::ios::binary);

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename);

  // Read data into contiguous memory
  fs.read(binaryBuffer.data(), binaryBuffer.size());
  fs.close();

  binaryBuffer.copyBufferToStorageView(storageView);
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
  filesystem::directory_iterator end;
  for(filesystem::directory_iterator it(directory_); it != end; ++it) {
    if(filesystem::is_regular_file(it->path()) &&
       boost::algorithm::starts_with(it->path().filename().string(), prefix_ + "_") &&
       filesystem::path(it->path()).extension() == ".dat") {

      if(!filesystem::remove(it->path()))
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
