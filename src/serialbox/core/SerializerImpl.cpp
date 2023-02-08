//===-- serialbox/core/SerializerImpl.h ---------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/SerializerImpl.h"
#include "serialbox/core/Compiler.h"
#include "serialbox/core/FieldMapSerializer.h"
#include "serialbox/core/MetainfoMapImplSerializer.h"
#include "serialbox/core/SavepointVectorSerializer.h"
#include "serialbox/core/Type.h"
#include "serialbox/core/Unreachable.h"
#include "serialbox/core/Version.h"
#include "serialbox/core/archive/ArchiveFactory.h"
#include "serialbox/core/archive/BinaryArchive.h"
#include "serialbox/core/hash/HashFactory.h"
#include <boost/algorithm/string.hpp>
#include <filesystem>
#include <fstream>
#include <memory>
#include <type_traits>

#ifdef SERIALBOX_ASYNC_API
#include <future>
#include <thread>
#endif

namespace serialbox {

void to_json(json::json& jsonNode, SerializerImpl const& ser) {
  LOG(info) << "Converting Serializer MetaData to JSON";

  // Tag version
  jsonNode["serialbox_version"] =
      100 * SERIALBOX_VERSION_MAJOR + 10 * SERIALBOX_VERSION_MINOR + SERIALBOX_VERSION_PATCH;

  // Serialize prefix
  jsonNode["prefix"] = ser.prefix();

  // Serialize globalMetainfo
  jsonNode["global_meta_info"] = ser.globalMetainfo();

  // Serialize SavepointVector
  jsonNode["savepoint_vector"] = ser.savepointVector();

  // Serialize FieldMap
  jsonNode["field_map"] = ser.fieldMap();
}

int SerializerImpl::enabled_ = 0;

SerializerImpl::SerializerImpl(OpenModeKind mode, const std::string& directory,
                               const std::string& prefix, const std::string& archiveName)
    : mode_(mode), directory_(directory), prefix_(prefix) {

  if(enabled_ == 0) {
    const char* envvar = std::getenv("SERIALBOX_SERIALIZATION_DISABLED");
    enabled_ = (envvar && std::atoi(envvar) > 0) ? -1 : 1;
  }

  metaDataFile_ = directory_ / ("MetaData-" + prefix + ".json");

  savepointVector_ = std::make_shared<SavepointVector>();
  fieldMap_ = std::make_shared<FieldMap>();
  globalMetainfo_ = std::make_shared<MetainfoMapImpl>();

  LOG(info) << "Creating Serializer (mode = " << mode_ << ") from directory " << directory_;

  // Validate integrity of directory (non-existent directories are created by the archive)

  try {
    if(mode_ == OpenModeKind::Read && !std::filesystem::exists(directory_))
      throw Exception("cannot create Serializer: directory %s does not exist", directory_);
  } catch(std::filesystem::filesystem_error& e) {
    throw Exception("std::filesystem error: %s", e.what());
  }

  // Check if we deal with an older version of serialbox and perform necessary upgrades, otherwise
  // construct from meta-datafrom JSON
  if(!upgradeMetaData()) {
    constructMetaDataFromJson();
    constructArchive(archiveName);
  }

  // If mode is writing drop all files
  if(mode_ == OpenModeKind::Write)
    clear();
}

void SerializerImpl::clear() noexcept {
  savepointVector_->clear();
  fieldMap_->clear();
  globalMetainfo_->clear();
  archive_->clear();
}

std::vector<std::string> SerializerImpl::fieldnames() const {
  std::vector<std::string> fields;
  fields.reserve(fieldMap_->size());
  for(auto it = fieldMap_->begin(), end = fieldMap_->end(); it != end; ++it)
    fields.push_back(it->first);
  return fields;
}

static inline bool dimsEqual(const std::vector<int>& dims1, const std::vector<int>& dims2) {
  if(dims1.size() != dims2.size())
    return false;

  // If a dimensions is negative, 0 or 1 it is ignored. We have to do this as gridtools treats empty
  // dimensions as 0 while STELLA and Frotran usually set them to 1.
  for(std::size_t i = 0; i < dims1.size(); ++i)
    if(dims1[i] != dims2[i] && !(dims1[i] <= 1 && dims2[i] <= 1))
      return false;
  return true;
}

std::shared_ptr<FieldMetainfoImpl>
SerializerImpl::checkStorageView(const std::string& name, const StorageView& storageView) const {

  // Check if field exists
  auto fieldIt = fieldMap_->findField(name);
  if(fieldIt == fieldMap_->end())
    throw Exception("field '%s' is not registerd within the Serializer", name);

  const FieldMetainfoImpl& fieldInfo = *fieldIt->second;

  // Check if types match
  if(fieldInfo.type() != storageView.type())
    throw Exception("field '%s' has type '%s' but was registrered as type '%s'", name,
                    TypeUtil::toString(fieldInfo.type()), TypeUtil::toString(storageView.type()));

  // Check if dimensions match
  if(!dimsEqual(fieldInfo.dims(), storageView.dims())) {
    throw Exception("dimensions of field '%s' do not match regsitered ones:"
                    "\nRegistred as: [ %s ]"
                    "\nGiven     as: [ %s ]",
                    name, ArrayUtil::toString(fieldInfo.dims()),
                    ArrayUtil::toString(storageView.dims()));
  }
  return fieldIt->second;
}

//===------------------------------------------------------------------------------------------===//
//     Writing
//===------------------------------------------------------------------------------------------===//

void SerializerImpl::write(const std::string& name, const SavepointImpl& savepoint,
                           const StorageView& storageView) {
  if(SerializerImpl::serializationStatus() < 0)
    return;

  LOG(info) << "Serializing field \"" << name << "\" at savepoint \"" << savepoint << "\" ... ";

  if(mode_ == OpenModeKind::Read)
    throw Exception("serializer not open in write mode, but write operation requested");

  //
  // 1) Check if field is registered within the Serializer and perform some consistency checks
  //
  auto info = checkStorageView(name, storageView);

  //
  // 2) Locate savepoint and register it if necessary
  //
  int savepointIdx = savepointVector_->find(savepoint);

  if(savepointIdx == -1) {
    LOG(info) << "Registering new savepoint \"" << savepoint << "\"";
    savepointIdx = savepointVector_->insert(savepoint);
  }

  //
  // 3) Check if field can be added to Savepoint
  //
  if(savepointVector_->hasField(savepointIdx, name))
    throw Exception("field '%s' already saved at savepoint '%s'", name,
                    (*savepointVector_)[savepointIdx].toString());

  //
  // 4) Pass the StorageView to the backend Archive and perform actual data-serialization.
  //
  FieldID fieldID = archive_->write(storageView, name, info);

  //
  // 5) Register FieldID within Savepoint.
  //
  savepointVector_->addField(savepointIdx, fieldID);

  //
  // 6) Update meta-data on disk
  //
  updateMetaData();

  LOG(info) << "Successfully serialized field \"" << name << "\"";
}

//===------------------------------------------------------------------------------------------===//
//     Reading
//===------------------------------------------------------------------------------------------===//

void SerializerImpl::read(const std::string& name, const SavepointImpl& savepoint,
                          StorageView& storageView, bool alsoPrevious) {
  if(SerializerImpl::serializationStatus() < 0)
    return;

  LOG(info) << "Deserializing field \"" << name << "\" at savepoint \"" << savepoint << "\" ... ";

  //
  // 1) Check if field is registred within the Serializer and perform some consistency checks
  //
  auto info = checkStorageView(name, storageView);

  //
  // 2) Check if savepoint exists and obtain fieldID
  //
  int savepointIdx = savepointVector_->find(savepoint);

  if(savepointIdx == -1)
    throw Exception("savepoint '%s' does not exist", savepoint.toString());

  FieldID fieldID;
  while(savepointIdx >= 0) {
    if(savepointVector_->hasField(savepointIdx, name) || !alsoPrevious) {
      fieldID = savepointVector_->getFieldID(savepointIdx, name);
      break;
    } else {
      // If alsoPrevious is speicifed AND the field was not found,
      // keep searching backwards
      --savepointIdx;
    }
  }

  if(savepointIdx == -1)
    throw Exception("field '%s' not found at or before savepoint '%s'", name, savepoint.toString());

  //
  // 3) Pass the StorageView to the backend Archive and perform actual data-deserialization.
  //
  archive_->read(storageView, fieldID, info);

  LOG(info) << "Successfully deserialized field \"" << name << "\"";
}

void SerializerImpl::readSliced(const std::string& name, const SavepointImpl& savepoint,
                                StorageView& storageView, Slice slice) {
  if(!archive_->isSlicedReadingSupported())
    throw Exception("archive '%s' does not support sliced reading", archive_->name());

  storageView.setSlice(slice);
  this->read(name, savepoint, storageView);
}

// This is the global task vector. If we would put the tasks inside SerializerImpl, we would have a
// conditional member in a class (i.e depending on a macro) which can cause trouble if someone
// compiled the library with SERIALBOX_ASYNC_API but doesn't use it when linking the library which
// will smash the stack!
#ifdef SERIALBOX_ASYNC_API
namespace global {
static std::vector<std::future<void>> tasks;
}
#endif

void SerializerImpl::readAsyncImpl(const std::string name, const SavepointImpl savepoint,
                                   StorageView storageView) {
  this->read(name, savepoint, storageView);
}

void SerializerImpl::readAsync(const std::string& name, const SavepointImpl& savepoint,
                               StorageView& storageView) {
#ifdef SERIALBOX_ASYNC_API
  if(!archive_->isReadingThreadSafe())
    this->read(name, savepoint, storageView);
  else
    // Bad things can happen if we forward the refrences and directly call the SerializerImpl::read,
    // we thus just make a copy of the arguments.
    global::tasks.emplace_back(std::async(std::launch::async, &SerializerImpl::readAsyncImpl, this,
                                          name, savepoint, storageView));
#else
  this->read(name, savepoint, storageView);
#endif
}

void SerializerImpl::waitForAll() {
#ifdef SERIALBOX_ASYNC_API
  try {
    for(auto& task : global::tasks)
      task.get();
  } catch(std::exception& e) {
    global::tasks.clear();
    throw Exception(e.what());
  }
  global::tasks.clear();
#endif
}

//===------------------------------------------------------------------------------------------===//
//     JSON Serialization
//===------------------------------------------------------------------------------------------===//

void SerializerImpl::constructMetaDataFromJson() {
  LOG(info) << "Constructing Serializer from MetaData ... ";

  // Try open meta-data file
  if(!std::filesystem::exists(metaDataFile_)) {
    if(mode_ != OpenModeKind::Read)
      return;
    else
      throw Exception("cannot create Serializer: MetaData-%s.json not found in %s", prefix_,
                      directory_);
  }

  json::json jsonNode;
  try {
    std::ifstream fs(metaDataFile_.string(), std::ios::in);
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

    if(!Version::isCompatible(serialboxVersion))
      throw Exception(
          "serialbox version of MetaData (%s) does not match the version of the library (%s)",
          Version::toString(serialboxVersion), SERIALBOX_VERSION_STRING);

    // Check if prefix match
    if(!jsonNode.count("prefix"))
      throw Exception("node 'prefix' not found");

    if(jsonNode["prefix"] != prefix_)
      throw Exception("inconsistent prefixes: expected '%s' got '%s'", jsonNode["prefix"], prefix_);

    // Construct globalMetainfo
    if(jsonNode.count("global_meta_info"))
      *globalMetainfo_ = jsonNode.at("global_meta_info");

    // Construct Savepoints
    if(jsonNode.count("savepoint_vector"))
      *savepointVector_ = jsonNode["savepoint_vector"];

    // Construct FieldMap
    if(jsonNode.count("field_map"))
      *fieldMap_ = jsonNode["field_map"]; // TODO probably fieldMap_ shouldn't be a shared_ptr

  } catch(Exception& e) {
    throw Exception("error while parsing %s: %s", metaDataFile_, e.what());
  }
}

std::string SerializerImpl::toString() const {
  std::stringstream ss;
  ss << "mode = " << mode_ << "\n";
  ss << "directory = " << directory_ << "\n";
  ss << "prefix = \"" << prefix_ << "\"\n";
  ss << "archive = \"" << archive_->name() << "\"\n";
  ss << "metainfo = " << *globalMetainfo_ << "\n";
  ss << "savepoints = [";
  for(const auto& sp : savepointVector_->savepoints())
    ss << "\n  " << *sp;
  ss << (savepointVector_->savepoints().empty() ? "" : "\n") << "]\n";
  ss << "fieldmetainfo = [";
  for(auto it = fieldMap_->begin(), end = fieldMap_->end(); it != end; ++it)
    ss << "\n  " << it->first << ": " << *it->second;
  ss << (fieldMap_->empty() ? "" : "\n") << "]";
  return ss.str();
}

std::ostream& operator<<(std::ostream& stream, const SerializerImpl& s) {
  return (stream << s.toString());
}

void SerializerImpl::updateMetaData() {
  LOG(info) << "Update MetaData of Serializer";

  if(mode_ == OpenModeKind::Read)
    throw Exception("Trying to write meta data in Read mode.");

  json::json jsonNode = *this;

  // Write metaData to disk (just overwrite the file, we assume that there is never more than one
  // Serializer per data set and thus our in-memory copy is always the up-to-date one)
  std::ofstream fs(metaDataFile_.string(), std::ios::out | std::ios::trunc);
  if(!fs.is_open())
    throw Exception("cannot open file: %s", metaDataFile_);
  fs << jsonNode.dump(1) << std::endl;
  fs.close();

  // Update archive meta-data
  archive_->updateMetaData();
}

void SerializerImpl::constructArchive(const std::string& archiveName) {
  archive_ = ArchiveFactory::create(archiveName, mode_, directory_.string(), prefix_);
}

//===------------------------------------------------------------------------------------------===//
//     Upgrade
//===------------------------------------------------------------------------------------------===//

bool SerializerImpl::upgradeMetaData() {
  std::filesystem::path oldMetaDataFile = directory_ / (prefix_ + ".json");

  //
  // Check if upgrade is necessary
  //

  try {
    // Check if prefix.json exists
    if(!std::filesystem::exists(oldMetaDataFile))
      return false;

    LOG(info) << "Detected old serialbox meta-data " << oldMetaDataFile;

    // Check if we already upgraded this archive
    if(std::filesystem::exists(metaDataFile_) &&
       (std::filesystem::last_write_time(oldMetaDataFile) <
        std::filesystem::last_write_time(metaDataFile_))) {
      return false;
    }
  } catch(std::filesystem::filesystem_error& e) {
    throw Exception("std::filesystem error: %s", e.what());
  }

  LOG(info) << "Upgrading meta-data to serialbox version (" << SERIALBOX_VERSION_STRING << ") ...";

  if(mode_ != OpenModeKind::Read)
    throw Exception("old serialbox archives cannot be opened in 'Write' or 'Append' mode");

  json::json oldJson;
  std::ifstream ifs(oldMetaDataFile.string());
  if(!ifs.is_open())
    throw Exception("upgrade failed: cannot open %s", oldMetaDataFile);
  ifs >> oldJson;
  ifs.close();

  //
  // Upgrade Metainfo
  //

  // Try to guess the precision of the floating point type. We try to match the floating point type
  // of the fields while defaulting to double.
  TypeID globalMetainfoFloatType = TypeID::Float64;
  if(oldJson.count("FieldsTable") && oldJson["FieldsTable"].size() > 0) {
    if(oldJson["FieldsTable"][0]["__elementtype"] == "float")
      globalMetainfoFloatType = TypeID::Float32;
  }

  LOG(info) << "Deduced float type of global meta-info as: " << globalMetainfoFloatType;

  if(oldJson.count("GlobalMetainfo")) {

    LOG(info) << "Upgrading global meta-info ...";

    for(auto it = oldJson["GlobalMetainfo"].begin(), end = oldJson["GlobalMetainfo"].end();
        it != end; ++it) {

      LOG(info) << "Inserting global meta-info: key = " << it.key() << ", value = " << it.value();

      std::string key = it.key();
      if(!boost::algorithm::starts_with(key, "__")) {
        if(it.value().is_string()) {
          std::string value = it.value();
          addGlobalMetainfo(key, value);
        } else if(it.value().is_boolean()) {
          addGlobalMetainfo(key, bool(it.value()));
        } else if(it.value().is_number_integer()) {
          addGlobalMetainfo(key, int(it.value()));
        } else if(it.value().is_number_float()) {
          if(globalMetainfoFloatType == TypeID::Float32)
            addGlobalMetainfo(key, float(it.value()));
          else
            addGlobalMetainfo(key, double(it.value()));
        } else
          throw Exception("failed to upgrade: cannot deduce type of globalMetainfo '%s'", it.key());
      }
    }

    LOG(info) << "Successfully upgraded global meta-info";
  }

  //
  // Upgrade FieldsTable
  //

  if(oldJson.count("FieldsTable")) {

    LOG(info) << "Upgrading fields table ...";

    const auto& fieldsTable = oldJson["FieldsTable"];
    for(std::size_t i = 0; i < fieldsTable.size(); ++i) {
      auto& fieldInfo = fieldsTable[i];
      std::string name = fieldInfo["__name"];

      LOG(info) << "Inserting field: " << name;

      // Get Type
      std::string elementtype = fieldInfo["__elementtype"];
      TypeID type = TypeID::Float64;

      if(elementtype == "int")
        type = TypeID::Int32;
      else if(elementtype == "float")
        type = TypeID::Float32;
      else if(elementtype == "double")
        type = TypeID::Float64;

      // Get dimension
      std::vector<int> dims(3, 1);
      dims[0] = int(fieldInfo["__isize"]);
      dims[1] = int(fieldInfo["__jsize"]);
      dims[2] = int(fieldInfo["__ksize"]);

      if(fieldInfo.count("__lsize"))
        dims.push_back(int(fieldInfo["__lsize"]));

      // Iterate field meta-info
      MetainfoMapImpl metaInfo;
      for(auto it = fieldInfo.begin(), end = fieldInfo.end(); it != end; ++it) {
        std::string key = it.key();
        if(it.value().is_string()) {
          std::string value = it.value();
          metaInfo.insert(key, value);
        } else if(it.value().is_boolean()) {
          metaInfo.insert(key, bool(it.value()));
        } else if(it.value().is_number_integer()) {
          metaInfo.insert(key, int(it.value()));
        } else if(it.value().is_number_float()) {
          if(globalMetainfoFloatType == TypeID::Float32)
            metaInfo.insert(key, float(it.value()));
          else
            metaInfo.insert(key, double(it.value()));
        } else
          throw Exception("failed to upgrade: Cannot deduce type of meta-info '%s' of field '%s'",
                          key, name);
      }

      fieldMap_->insert(name, type, dims, metaInfo);
    }

    LOG(info) << "Successfully upgraded fields table";
  }

  //
  // Upgrade SavepointVector and ArchiveMetaData
  //

  // Construct archive but don't parse the meta-data (we will do it ourselves below)
  archive_ = std::make_unique<BinaryArchive>(mode_, directory_.string(), prefix_, true);

  // Old serialbox always uses SHA256
  static_cast<BinaryArchive*>(archive_.get())->setHash(HashFactory::create("SHA256"));

  BinaryArchive::FieldTable& fieldTable = static_cast<BinaryArchive*>(archive_.get())->fieldTable();

  if(oldJson.count("OffsetTable")) {

    LOG(info) << "Upgrading offset table ...";

    const auto& offsetTable = oldJson["OffsetTable"];
    for(std::size_t i = 0; i < offsetTable.size(); ++i) {
      auto& offsetTableEntry = offsetTable[i];

      // Create savepoint
      std::string name = offsetTableEntry["__name"];
      SavepointImpl savepoint(name);

      // Add meta-info to savepoint
      for(auto it = offsetTableEntry.begin(), end = offsetTableEntry.end(); it != end; ++it) {
        std::string key = it.key();
        if(!boost::algorithm::starts_with(key, "__")) {
          if(it.value().is_string()) {
            std::string value = it.value();
            savepoint.addMetainfo(it.key(), value);
          } else if(it.value().is_boolean()) {
            savepoint.addMetainfo(it.key(), bool(it.value()));
          } else if(it.value().is_number_integer()) {
            savepoint.addMetainfo(it.key(), int(it.value()));
          } else if(it.value().is_number_float()) {
            if(globalMetainfoFloatType == TypeID::Float32)
              savepoint.addMetainfo(it.key(), float(it.value()));
            else
              savepoint.addMetainfo(it.key(), double(it.value()));
          } else
            throw Exception(
                "failed to upgrade: Cannot deduce type of meta-info '%s' of savepoint '%s'",
                it.key(), name);
        }
      }

      LOG(info) << "Adding savepoint: " << savepoint;

      // Register savepoint
      int savepointIdx = savepointVector_->insert(savepoint);
      assert(savepointIdx != -1);

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
            assert(fileOffset.offset != 0);
            fieldID.id = fieldOffsetTable.size();
            fieldOffsetTable.push_back(fileOffset);
          }
        } else {
          assert(fileOffset.offset == 0);
          fieldID.id = 0;
          fieldTable.insert(BinaryArchive::FieldTable::value_type(
              fieldname, BinaryArchive::FieldOffsetTable(1, fileOffset)));
        }

        // Add field to savepoint
        savepointVector_->addField(savepointIdx, fieldID);

        LOG(info) << "Adding field '" << fieldID << "' to savepoint " << savepoint;
      }
    }

    LOG(info) << "Successfully upgraded offset table";
  }

  // Try to write the mata data to disk so that we can avoid such an upgrade in the future. However,
  // if we read from a location where we have no write permission, this should be non-fatal.
  try {
    updateMetaData();
  } catch(Exception& e) {
    LOG(warning) << "Failed to write upgraded meta-data to disk: " << e.what();
  }

  LOG(info) << "Successfully upgraded MetaData to serialbox version (" << SERIALBOX_VERSION_STRING
            << ")";

  return true;
}

} // namespace serialbox
