//===-- Core/SerializerImpl.h -------------------------------------------------------*- C++ -*-===//
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

#include "serialbox/Core/UpgradeArchive.h"
#include "serialbox/Core/Archive/ArchiveFactory.h"
#include "serialbox/Core/Compiler.h"
#include "serialbox/Core/STLExtras.h"
#include "serialbox/Core/SerializerImpl.h"
#include "serialbox/Core/Type.h"
#include "serialbox/Core/Version.h"
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
  } else
    ss << "<empty>";
  return ss.str();
}

SerializerImpl::SerializerImpl(OpenModeKind mode, const std::string& directory,
                               const std::string& archiveName)
    : mode_(mode), directory_(directory) {

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
  
  savepointVector_.clear();
  fieldMap_.clear();
  globalMetaInfo_.clear();

  if(mode_ == OpenModeKind::Write)
    return;
      
  // Check if we deal with an older version of serialbox
  UpgradeArchive::upgrade(directory_);

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

  // Serialize globalMetaInfo
  jsonNode["global_meta_info"] = globalMetaInfo_.toJSON();

  // Serialize SavepointVector
  jsonNode["savepoint_vector"] = savepointVector_.toJSON();

  // Serialize FieldMap
  jsonNode["field_map"] = fieldMap_.toJSON();

  return jsonNode;
}

void SerializerImpl::updateMetaData() {
  
  if(mode_ == OpenModeKind::Read)
    return;
  
  LOG(INFO) << "Update MetaData of Serializer";

  json::json jsonNode = toJSON();
  boost::filesystem::path filename = directory_ / SerializerImpl::SerializerMetaDataFile;

  // Write metaData to disk (just overwrite the file, we assume that there is never more than one
  // Serializer per data set and thus our in-memory copy is always the up-to-date one)
  std::ofstream fs(filename.string(), std::ios::out | std::ios::trunc);
  fs << jsonNode.dump(1) << std::endl;
  fs.close();

  // Update archive meta-data if necessary
  archive_->updateMetaData();
}

void SerializerImpl::constructArchive(const std::string& archiveName) {
  archive_ = ArchiveFactory::getInstance().create(archiveName, mode_, directory_.string(), "field");
}

} // namespace serialbox
