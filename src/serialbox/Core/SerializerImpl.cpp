//===-- serialbox/Core/SerializerImpl.h ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the shared implementation of all Serializers.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Archive/ArchiveFactory.h"
#include "serialbox/Core/Archive/BinaryArchive.h"
#include "serialbox/Core/Compiler.h"
#include "serialbox/Core/STLExtras.h"
#include "serialbox/Core/SerializerImpl.h"
#include "serialbox/Core/Type.h"
#include "serialbox/Core/Unreachable.h"
#include "serialbox/Core/Version.h"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <fstream>
#include <memory>
#include <type_traits>

namespace serialbox {

template <class VecType>
static std::string vecToString(VecType&& vec) {
  std::stringstream ss;
  if(!vec.empty()) {
    for(std::size_t i = 0; i < vec.size() - 1; ++i)
      ss << vec[i] << ", ";
    ss << vec.back();
  }
  return ss.str();
}

SerializerImpl::SerializerImpl(OpenModeKind mode, const std::string& directory,
                               const std::string& archiveName, std::string prefix)
    : mode_(mode), directory_(directory), prefix_(prefix) {

  LOG(INFO) << "Creating Serializer (mode = " << mode_ << ") from directory " << directory_;

  // Validate integrity of directory (non-existent directories are created by the archive)
  try {
    bool directoryExists = boost::filesystem::exists(directory_);

    switch(mode) {
    case OpenModeKind::Read: {
      if(!directoryExists)
        throw Exception("cannot create Serializer: directory %s does not exist", directory_);
      break;
    }
    case OpenModeKind::Write: {
      if(directoryExists && !boost::filesystem::is_empty(directory_))
        throw Exception("cannot create Serializer: directory %s is not empty", directory_);
      break;
    }
    case OpenModeKind::Append:
      break;
    }
  } catch(boost::filesystem::filesystem_error& e) {
    throw Exception("filesystem error: %s", e.what());
  }

  // Construct Archive and meta-data
  constructMetaDataFromJson();
  constructArchive(archiveName);
}

void SerializerImpl::clear() noexcept {
  savepointVector_.clear();
  fieldMap_.clear();
  globalMetaInfo_.clear();
  archive_.release();
}

std::vector<std::string> SerializerImpl::fieldnames() const {
  std::vector<std::string> fields;
  fields.reserve(fieldMap_.size());
  for(auto it = fieldMap_.begin(), end = fieldMap_.end(); it != end; ++it)
    fields.push_back(it->first);
  return fields;
}

void SerializerImpl::checkStorageView(const std::string& name,
                                      const StorageView& storageView) const {

  // Check if field exists
  auto fieldIt = fieldMap_.findField(name);
  if(fieldIt == fieldMap_.end())
    throw Exception("field '%s' is not registerd within the Serializer", name);

  const FieldMetaInfo& fieldInfo = fieldIt->second;

  // Check if types match
  if(fieldInfo.type() != storageView.type())
    throw Exception("field '%s' has type '%s' but was registrered as type '%s'", name,
                    TypeUtil::toString(fieldInfo.type()), TypeUtil::toString(storageView.type()));

  // Check if dimensions match
  if(storageView.dims().size() != fieldInfo.dims().size() ||
     !std::equal(storageView.dims().begin(), storageView.dims().end(), fieldInfo.dims().begin())) {
    throw Exception("dimensions of field '%s' do not match regsitered ones:"
                    "\nRegistred as: [ %s ]"
                    "\nGiven     as: [ %s ]",
                    name, vecToString(fieldInfo.dims()), vecToString(storageView.dims()));
  }
}

//===------------------------------------------------------------------------------------------===//
//     Writing
//===------------------------------------------------------------------------------------------===//

void SerializerImpl::write(const std::string& name, const Savepoint& savepoint,
                           StorageView& storageView) {
  LOG(INFO) << "Serializing field \"" << name << "\" at savepoint \"" << savepoint << "\" ... ";

  if(mode_ == OpenModeKind::Read)
    throw Exception("serializer not open in write mode, but write operation requested");

  //
  // 1) Check if field is registred within the Serializer and perform some consistency checks
  //
  checkStorageView(name, storageView);

  //
  // 2) Locate savepoint and register it if necessary
  //
  int savepointIdx = savepointVector_.find(savepoint);

  if(savepointIdx == -1) {
    LOG(INFO) << "Registering new savepoint \"" << savepoint << "\"";
    savepointIdx = savepointVector_.insert(savepoint);
  }

  //
  // 3) Check if field can be added to Savepoint
  //
  if(savepointVector_.hasField(savepointIdx, name))
    throw Exception("field '%s' already saved at savepoint '%s'", name,
                    savepointVector_[savepointIdx].toString());

  //
  // 4) Pass the StorageView to the backend Archive and perform actual data-serialization.
  //
  FieldID fieldID = archive_->write(storageView, name);

  //
  // 5) Register FieldID within Savepoint.
  //
  savepointVector_.addField(savepointIdx, fieldID);

  //
  // 6) Update meta-data on disk
  //
  updateMetaData();

  LOG(INFO) << "Successfully serialized field \"" << name << "\"";
}

//===------------------------------------------------------------------------------------------===//
//     Reading
//===------------------------------------------------------------------------------------------===//

void SerializerImpl::read(const std::string& name, const Savepoint& savepoint,
                          StorageView& storageView) {
  LOG(INFO) << "Deserializing field \"" << name << "\" at savepoint \"" << savepoint << "\" ... ";

  if(mode_ != OpenModeKind::Read)
    throw Exception("serializer not open in read mode, but read operation requested");

  //
  // 1) Check if field is registred within the Serializer and perform some consistency checks
  //
  checkStorageView(name, storageView);

  //
  // 2) Check if savepoint exists and obtain fieldID
  //
  int savepointIdx = savepointVector_.find(savepoint);

  if(savepointIdx == -1)
    throw Exception("savepoint '%s' does not exist", savepoint.toString());

  FieldID fieldID = savepointVector_.getFieldID(savepointIdx, name);

  //
  // 3) Pass the StorageView to the backend Archive and perform actual data-deserialization.
  //
  archive_->read(storageView, fieldID);

  LOG(INFO) << "Successfully deserialized field \"" << name << "\"";
}

//===------------------------------------------------------------------------------------------===//
//     JSON Serialization
//===------------------------------------------------------------------------------------------===//

void SerializerImpl::constructMetaDataFromJson() {
  LOG(INFO) << "Constructing Serializer from MetaData ... ";

  clear();

  if(mode_ == OpenModeKind::Write)
    return;

  // Check if we deal with an older version of serialbox and perform necessary upgrades
  upgradeMetaData();

  // Try open MetaData.json file
  boost::filesystem::path filename = directory_ / SerializerImpl::SerializerMetaDataFile;

  if(!boost::filesystem::exists(filename)) {
    if(mode_ == OpenModeKind::Append)
      return;
    else
      throw Exception("cannot create Serializer: MetaData.json not found in %s", directory_);
  }

  json::json jsonNode;
  try {
    std::ifstream fs(filename.string(), std::ios::in);
    fs >> jsonNode;
    fs.close();
  } catch(std::exception& e) {
    throw Exception("JSON parser error: %s", e.what());
  }

  try {
    // Check consistency
    if(!jsonNode.count("serialbox_version"))
      throw Exception("node 'serialbox_version' not found");

    int serialboxVersion = jsonNode["serialbox_version"];

    if(!Version::compare(serialboxVersion))
      throw Exception(
          "serialbox version of MetaData (%s) does not match the version of the library (%s)",
          Version::toString(serialboxVersion), SERIALBOX_VERSION_STRING);

    // Check if prefix match
    if(!jsonNode.count("prefix"))
      throw Exception("node 'prefix' not found");

    if(jsonNode["prefix"] != prefix_)
      throw Exception("inconsistent prefixes: expected '%s' got '%s'", jsonNode["prefix"], prefix_);

    // Construct globalMetaInfo
    if(jsonNode.count("global_meta_info"))
      globalMetaInfo_.fromJSON(jsonNode["global_meta_info"]);

    // Construct Savepoints
    if(jsonNode.count("savepoint_vector"))
      savepointVector_.fromJSON(jsonNode["savepoint_vector"]);

    // Construct FieldMap
    if(jsonNode.count("field_map"))
      fieldMap_.fromJSON(jsonNode["field_map"]);

  } catch(Exception& e) {
    throw Exception("error while parsing %s: %s", filename, e.what());
  }
}

std::ostream& operator<<(std::ostream& stream, const SerializerImpl& s) {
  stream << "Serializer = {\n";
  stream << "  mode: " << s.mode_ << "\n";
  stream << "  directory: " << s.directory_ << "\n";
  stream << "  " << s.savepointVector_ << "\n";
  stream << "  " << s.fieldMap_ << "\n";
  stream << "  " << s.globalMetaInfo_ << "\n";
  stream << "}\n";
  return stream;
}

json::json SerializerImpl::toJSON() const {
  LOG(INFO) << "Converting Serializer MetaData to JSON";

  json::json jsonNode;

  // Tag version
  jsonNode["serialbox_version"] =
      100 * SERIALBOX_VERSION_MAJOR + 10 * SERIALBOX_VERSION_MINOR + SERIALBOX_VERSION_PATCH;

  // Serialize prefix
  jsonNode["prefix"] = prefix_;

  // Serialize globalMetaInfo
  jsonNode["global_meta_info"] = globalMetaInfo_.toJSON();

  // Serialize SavepointVector
  jsonNode["savepoint_vector"] = savepointVector_.toJSON();

  // Serialize FieldMap
  jsonNode["field_map"] = fieldMap_.toJSON();

  return jsonNode;
}

void SerializerImpl::updateMetaData() {
  LOG(INFO) << "Update MetaData of Serializer";

  json::json jsonNode = toJSON();
  boost::filesystem::path filename = directory_ / SerializerImpl::SerializerMetaDataFile;

  // Write metaData to disk (just overwrite the file, we assume that there is never more than one
  // Serializer per data set and thus our in-memory copy is always the up-to-date one)
  std::ofstream fs(filename.string(), std::ios::out | std::ios::trunc);
  if(!fs.is_open())
    throw Exception("cannot open file: %s", filename);
  fs << jsonNode.dump(1) << std::endl;
  fs.close();

  // Update archive meta-data if necessary
  archive_->updateMetaData();
}

void SerializerImpl::constructArchive(const std::string& archiveName) {
  archive_ = ArchiveFactory::getInstance().create(archiveName, mode_, directory_.string(), prefix_);
}

//===------------------------------------------------------------------------------------------===//
//     Upgrade
//===------------------------------------------------------------------------------------------===//

void SerializerImpl::upgradeMetaData() {
  boost::filesystem::path oldMetaDataFile = directory_ / (prefix_ + ".json");
  boost::filesystem::path newMetaDataFile = directory_ / SerializerImpl::SerializerMetaDataFile;
  boost::filesystem::path newArchiveMetaDataFile = directory_ / Archive::ArchiveMetaDataFile;

  //
  // Check if upgrade is necessary
  //

  try {
    // Check if prefix.json exists
    if(!boost::filesystem::exists(oldMetaDataFile))
      return;

    LOG(INFO) << "Detected old serialbox meta-data " << oldMetaDataFile;

    // Check if we already upgraded this archive
    if(boost::filesystem::exists(newMetaDataFile) &&
       (boost::filesystem::last_write_time(oldMetaDataFile) <
        boost::filesystem::last_write_time(newMetaDataFile))) {
      return;
    }

    // Remove the new-meta data (if it exists)
    if(boost::filesystem::exists(newMetaDataFile))
      boost::filesystem::remove(newMetaDataFile);

    if(boost::filesystem::exists(newArchiveMetaDataFile))
      boost::filesystem::remove(newArchiveMetaDataFile);

  } catch(boost::filesystem::filesystem_error& e) {
    throw Exception("filesystem error: %s", e.what());
  }

  LOG(INFO) << "Upgrading meta-data to serialbox version (" << SERIALBOX_VERSION_STRING << ") ...";

  json::json oldJson;
  std::ifstream ifs(oldMetaDataFile.string());
  if(!ifs.is_open())
    throw Exception("upgrade failed: cannot open %s", oldMetaDataFile);
  ifs >> oldJson;
  ifs.close();

  //
  // Upgrade MetaInfo
  //

  // Try to guess the precision of the floating point type. We try to match the floating point type
  // of the fields while defaulting to double.
  TypeID globalMetaInfoFloatType = TypeID::Float64;
  if(oldJson.count("FieldsTable") && oldJson["FieldsTable"].size() > 0) {
    if(oldJson["FieldsTable"][0]["__elementtype"] == "float")
      globalMetaInfoFloatType = TypeID::Float32;
  }

  LOG(INFO) << "Deduced float type of global meta-info as: " << globalMetaInfoFloatType;

  if(oldJson.count("GlobalMetainfo")) {

    LOG(INFO) << "Upgrading global meta-info ...";

    for(auto it = oldJson["GlobalMetainfo"].begin(), end = oldJson["GlobalMetainfo"].end();
        it != end; ++it) {

      LOG(INFO) << "Inserting global meta-info: key = " << it.key() << ", value = " << it.value();

      std::string key = it.key();
      if(!boost::algorithm::starts_with(key, "__")) {
        if(it.value().is_string()) {
          std::string value = it.value();
          addGlobalMetaInfo(key, value);
        } else if(it.value().is_boolean()) {
          addGlobalMetaInfo(key, bool(it.value()));
        } else if(it.value().is_number_integer()) {
          addGlobalMetaInfo(key, int(it.value()));
        } else if(it.value().is_number_float()) {
          if(globalMetaInfoFloatType == TypeID::Float32)
            addGlobalMetaInfo(key, float(it.value()));
          else
            addGlobalMetaInfo(key, double(it.value()));
        } else
          throw Exception("failed to upgrade: Cannot deduce type of globalMetaInfo '%s'", it.key());
      }
    }

    LOG(INFO) << "Successfully upgraded global meta-info";
  }

  //
  // Upgrade FieldsTable
  //

  if(oldJson.count("FieldsTable")) {

    LOG(INFO) << "Upgrading fields table ...";

    const auto& fieldsTable = oldJson["FieldsTable"];
    for(std::size_t i = 0; i < fieldsTable.size(); ++i) {
      auto& fieldInfo = fieldsTable[i];

      LOG(INFO) << "Inserting field: " << fieldInfo["__name"];

      // Get Type
      TypeID type = TypeID::Float64;
      if(fieldInfo["__elementtype"] == "int")
        type = TypeID::Int32;
      else if(fieldInfo["__elementtype"] == "float")
        type = TypeID::Float32;
      else if(fieldInfo["__elementtype"] == "double")
        type = TypeID::Float64;

      // Get dimension
      std::vector<int> dims(3, 1);
      dims[0] = int(fieldInfo["__isize"]);
      dims[1] = int(fieldInfo["__jsize"]);
      dims[2] = int(fieldInfo["__ksize"]);

      if(fieldInfo.count("__lsize"))
        dims.push_back(int(fieldInfo["__lsize"]));

      // Add Halos as meta-info
      MetaInfoMap metaInfo;
      metaInfo.insert("__iminushalosize", int(fieldInfo["__iminushalosize"]));
      metaInfo.insert("__jminushalosize", int(fieldInfo["__jminushalosize"]));
      metaInfo.insert("__kminushalosize", int(fieldInfo["__kminushalosize"]));
      metaInfo.insert("__iplushalosize", int(fieldInfo["__iplushalosize"]));
      metaInfo.insert("__jplushalosize", int(fieldInfo["__jplushalosize"]));
      metaInfo.insert("__kplushalosize", int(fieldInfo["__kplushalosize"]));

      // Add name as meta-info
      std::string name = fieldInfo["__name"];
      metaInfo.insert("__name", name);

      // Add rank as meta-info
      metaInfo.insert("__rank", int(fieldInfo["__rank"]));

      // Iterate field meta-info
      for(auto it = fieldInfo.begin(), end = fieldInfo.end(); it != end; ++it) {
        std::string key = it.key();
        if(!boost::algorithm::starts_with(key, "__")) {
          if(it.value().is_string()) {
            std::string value = it.value();
            metaInfo.insert(it.key(), value);
          } else if(it.value().is_boolean()) {
            metaInfo.insert(it.key(), bool(it.value()));
          } else if(it.value().is_number_integer()) {
            metaInfo.insert(it.key(), int(it.value()));
          } else if(it.value().is_number_float()) {
            if(globalMetaInfoFloatType == TypeID::Float32)
              metaInfo.insert(it.key(), float(it.value()));
            else
              metaInfo.insert(it.key(), double(it.value()));
          } else
            throw Exception("failed to upgrade: Cannot deduce type of meta-info '%s' of field '%s'",
                            it.key(), name);
        }
      }

      fieldMap_.insert(name, type, dims, metaInfo);
    }

    LOG(INFO) << "Successfully upgraded fields table";
  }

  //
  // Upgrade SavepointVector and ArchiveMetaData
  //
  BinaryArchive archive(OpenModeKind::Append, directory_.string(), prefix_);
  BinaryArchive::FieldTable& fieldTable = archive.fieldTable();

  if(oldJson.count("OffsetTable")) {

    LOG(INFO) << "Upgrading offset table ...";

    const auto& offsetTable = oldJson["OffsetTable"];
    for(std::size_t i = 0; i < offsetTable.size(); ++i) {
      auto& offsetTableEntry = offsetTable[i];

      // Create savepoint
      std::string name = offsetTableEntry["__name"];
      Savepoint savepoint(name);

      // Add meta-info to savepoint
      for(auto it = offsetTableEntry.begin(), end = offsetTableEntry.end(); it != end; ++it) {
        std::string key = it.key();
        if(!boost::algorithm::starts_with(key, "__")) {
          if(it.value().is_string()) {
            std::string value = it.value();
            savepoint.addMetaInfo(it.key(), value);
          } else if(it.value().is_boolean()) {
            savepoint.addMetaInfo(it.key(), bool(it.value()));
          } else if(it.value().is_number_integer()) {
            savepoint.addMetaInfo(it.key(), int(it.value()));
          } else if(it.value().is_number_float()) {
            if(globalMetaInfoFloatType == TypeID::Float32)
              savepoint.addMetaInfo(it.key(), float(it.value()));
            else
              savepoint.addMetaInfo(it.key(), double(it.value()));
          } else
            throw Exception(
                "failed to upgrade: Cannot deduce type of meta-info '%s' of savepoint '%s'",
                it.key(), name);
        }
      }

      LOG(INFO) << "Adding savepoint: " << savepoint;

      // Register savepoint
      int savepointIdx = savepointVector_.insert(savepoint);
      CHECK_NE(savepointIdx, -1);

      // Add fields to savepoint and field table of the archive
      for(auto it = offsetTableEntry["__offsets"].begin(),
               end = offsetTableEntry["__offsets"].end();
          it != end; ++it) {
        std::string fieldname = it.key();

        FieldID fieldID{fieldname, 0};
        BinaryArchive::FileOffsetType fileOffset{it.value()[0], it.value()[1]};

        // Insert offsets into the field table (This mimics the write operation of the
        // Binary archive)
        auto fieldTableIt = fieldTable.find(fieldname);
        if(fieldTableIt != fieldTable.end()) {
          BinaryArchive::FieldOffsetTable& fieldOffsetTable = fieldTableIt->second;
          bool fieldAlreadySerialized = false;

          // Check if field has already been serialized by comparing the checksum
          for(std::size_t i = 0; i < fieldOffsetTable.size(); ++i)
            if(fileOffset.checksum == fieldOffsetTable[i].checksum) {
              fieldAlreadySerialized = true;
              fieldID.id = i;
              break;
            }

          // Append field at the end
          if(!fieldAlreadySerialized) {
            CHECK_NE(fileOffset.offset, 0);
            fieldID.id = fieldOffsetTable.size();
            fieldOffsetTable.push_back(fileOffset);
          }
        } else {
          CHECK_EQ(fileOffset.offset, 0);
          fieldID.id = 0;
          fieldTable.insert(BinaryArchive::FieldTable::value_type(
              fieldname, BinaryArchive::FieldOffsetTable(1, fileOffset)));
        }

        // Add field to savepoint
        savepointVector_.addField(savepointIdx, fieldID);

        LOG(INFO) << "Adding field '" << fieldID << "' to savepoint " << savepoint;
      }
    }

    LOG(INFO) << "Successfully upgraded offset table";
  }

  // Flush archive to disk
  archive.updateMetaData();

  // Write new serializer meta-data and archive meta-data to disk
  constructArchive("BinaryArchive");
  updateMetaData();

  // Clear all objects and release the archive
  clear();

  LOG(INFO) << "Successfully upgraded MetaData to serialbox version (" << SERIALBOX_VERSION_STRING
            << ")";
}

} // namespace serialbox
