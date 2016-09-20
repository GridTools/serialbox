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

#include "serialbox/Core/Archive/BinaryArchive.h"
#include "serialbox/Core/Compiler.h"
#include "serialbox/Core/STLExtras.h"
#include "serialbox/Core/SerializerImpl.h"
#include "serialbox/Core/Version.h"
#include <boost/filesystem.hpp>
#include <fstream>
#include <memory>
#include <type_traits>

namespace serialbox {

SerializerImpl::SerializerImpl(OpenModeKind mode, const std::string& directory,
                               const std::string& archiveName)
    : mode_(mode), directory_(directory) {

  // Validate integrity of directory
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

SerializerImpl::SerializerImpl(OpenModeKind mode, const std::string& directory,
                               std::vector<SavepointImpl>& savepoints, FieldMap& fieldMap,
                               MetaInfoMap& globalMetaInfo, std::unique_ptr<Archive>& archive)
    : mode_(mode), directory_(directory), savepoints_(std::move(savepoints)),
      fieldMap_(std::move(fieldMap)), globalMetaInfo_(std::move(globalMetaInfo)),
      archive_(std::move(archive)) {}

void SerializerImpl::constructMetaDataFromJson() {
  savepoints_.clear();
  fieldMap_.clear();
  globalMetaInfo_.clear();

  if(mode_ == OpenModeKind::Write)
    return;

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

  if(jsonNode.empty())
    return;

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
    if(jsonNode.count("savepoints"))
      for(auto it = jsonNode["savepoints"].begin(), end = jsonNode["savepoints"].end(); it != end;
          ++it)
        savepoints_.emplace_back(*it);

    // Construct FieldMap
    if(jsonNode.count("field_map"))
      fieldMap_.fromJSON(jsonNode["field_map"]);

  } catch(Exception& e) {
    throw Exception("error while parsing '%s': %s", directory_, e.what());
  }
}

void SerializerImpl::constructArchive(const std::string& archiveName) {
  archive_ = make_unique<BinaryArchive>(directory_, mode_);
}

void SerializerImpl::updateMetaData() {
  if(mode_ == OpenModeKind::Read)
    return;

  json::json jsonNode;
  boost::filesystem::path filename = directory_ / SerializerImpl::SerializerMetaDataFile;

  // Tag version
  jsonNode["serialbox_version"] =
      100 * SERIALBOX_VERSION_MAJOR + 10 * SERIALBOX_VERSION_MINOR + SERIALBOX_VERSION_PATCH;

  // Serialize globalMetaInfo
  jsonNode["global_meta_info"] = globalMetaInfo_.toJSON();

  // Serialize Savepoints
  for(auto it = savepoints_.cbegin(), end = savepoints_.cend(); it != end; ++it)
    jsonNode["savepoints"].push_back(it->toJSON());

  // Serialize FieldMap
  jsonNode["field_map"] = fieldMap_.toJSON();

  // Write metaData to disk (just overwrite the file, we assume that there is never more than one
  // Serializer per data set and thus our in-memory copy is always the up-to-date one)
  std::ofstream fs(filename.string(), std::ios::out | std::ios::trunc);
  fs << jsonNode.dump(4) << std::endl;
  fs.close();

  // Update archive meta-data if necessary
  archive_->updateMetaData();
}

} // namespace serialbox
