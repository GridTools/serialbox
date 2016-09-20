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

#ifndef SERIALBOX_CORE_SERIALIZERIMPL_H
#define SERIALBOX_CORE_SERIALIZERIMPL_H

#include "serialbox/Core/Archive/Archive.h"
#include "serialbox/Core/FieldMap.h"
#include "serialbox/Core/MetaInfoMap.h"
#include "serialbox/Core/SavepointImpl.h"
#include <boost/filesystem.hpp>
#include <iosfwd>

namespace serialbox {

/// \brief Shared implementation of the Serializers
///
/// Direct usage of this class is discouraged, use the Serializer classes provided by the Frontends
/// instead.
class SerializerImpl {
public:
  /// \brief
  static constexpr const char* SerializerMetaDataFile = "MetaData.json";

  /// \brief Copy constructor [deleted]
  SerializerImpl(const SerializerImpl&) = delete;

  /// \brief Move constructor
  SerializerImpl(SerializerImpl&&) = default;

  /// \brief Copy assignment [deleted]
  SerializerImpl& operator=(const SerializerImpl&) = delete;

  /// \brief Move assignment
  SerializerImpl& operator=(SerializerImpl&&) = default;

  /// \brief Construct from JSON meta-data
  ///
  /// \param mode         Mode of the Serializer
  /// \param directory    Directory of the Archive and meta-data
  /// \param archiveName  String passed to the ArchiveFactory to construct the Archive
  ///
  /// This will read MetaData.json to initialize the savepoint vector, the fieldsTable and
  /// globalMetaInfo. Further, it will construct the Archive by reading the ArchiveMetaData.json.
  ///
  /// \throw Exception  Invalid directory or corrupted meta-data files
  SerializerImpl(OpenModeKind mode, const std::string& directory, const std::string& archiveName);

  /// \brief Construct members externally and \b move them in
  ///
  /// \param mode
  /// \param savepoints
  /// \param fieldMap
  /// \param globalMetaInfo
  /// \param archive
  SerializerImpl(OpenModeKind mode, const std::string& directory,
                 std::vector<SavepointImpl>& savepoints, FieldMap& fieldMap,
                 MetaInfoMap& globalMetaInfo, std::unique_ptr<Archive>& archive);

  /// \brief Add a new key-value pair to the global meta-info of the Serializer
  ///
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  /// \throw Exception  Value cannot be inserted as it already exists
  template <class StringType, class ValueType>
  void addGlobalMetainfo(StringType&& key, ValueType&& value) {
    if(!globalMetaInfo_.insert(std::forward<StringType>(key), std::forward<ValueType>(value)))
      throw Exception("cannot add element with key '%s' to globalMetaInfo: element already exists",
                      key);
  }

  /// \brief Query globalMetaInfo map for key ´key´ and retrieve value as type ´T´
  ///
  /// \param key    Key of the new element
  /// \throw Exception  Key ´key´ does not exist in the globalMetaInfo map or value cannot be 
  ///                   converted to type ´T´
  template <class T, class StringType>
  const T& getGlobalMetainfoAs(StringType&& key) const {
    try {
      return globalMetaInfo_.at(key).template as<T>();
    } catch(Exception& e) {
      throw Exception("cannot get element with key '%s' from globalMetaInfo: %s", key, e.what());
    }
  }
  
  /// \brief Register a new field within the Serializer
  ///
  /// \param name  Name of the the new field
  /// \param Args  Arguments forwarded to the constructor of FieldMetaInfo
  /// 
  /// \throw Exception  Field with same name already exists
  template <class StringType, typename... Args>
  void registerField(StringType&& key, Args&&... args) {
    if(!fieldMap_.insert(std::forward<StringType>(key), std::forward<Args>(args)...))
      throw Exception("cannot register field '%s': field already exists");
  }
  
  /// \brief Query FieldMap for field with name ´name´ and return refrence to FieldMetaInfo
  ///
  /// \param name  Name of the field
  /// \throw Exception  Field with name `name` does not exist in FieldMap
  template <class StringType>
  const FieldMetaInfo& getFieldMetaInfoOf(StringType&& name) const {
    return fieldMap_.getFieldMetaInfoOf(std::forward<StringType>(name));
  }
  
  /// \brief Register a savepoint given by ´name´ with empty meta-information
  ///
  /// \param name   Name of the savepoint
  /// \throw Exception  Savepoint is already registered
  template <class StringType>
  void registerSavepoint(StringType&& name) {
    // TODO check...
    // Maybe build an index data strucutre to speed up the check
    savepoints_.emplace_back(name);
  }
  
  /// \brief Register a savepoint given by ´name´ with meta-information ´metaInfo´
  ///
  /// \param name       Name of the savepoint
  /// \param metaInfo   MetaInformation of the Savepoint
  /// \throw Exception  Savepoint is already registered
  template <class StringType, class MetaInfoType>
  void registerSavepoint(StringType&& name, MetaInfoType&& metaInfo) {
    //TODO check...
    savepoints_.emplace_back(name, metaInfo);    
  }

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SerializerImpl& s);

  /// \brief Convert meta-data to JSON and serialize to MetaData.json and ArchiveMetaData.json
  ///
  /// This will ensure MetaData.json is up-to-date with the in-memory versions of the savepoint
  /// vector, fieldsTable and globalMetaInfo as well as the meta-data of the Archive.
  void updateMetaData();

  /// \brief Get refrence to savepoint vector
  std::vector<SavepointImpl>& savepoints() noexcept { return savepoints_; }
  const std::vector<SavepointImpl>& savepoints() const noexcept { return savepoints_; }

  /// \brief Get refrence to the field map
  FieldMap& fieldMap() noexcept { return fieldMap_; }
  const FieldMap& fieldMap() const noexcept { return fieldMap_; }

  /// \brief Get refrence to global meta information
  MetaInfoMap& globalMetaInfo() noexcept { return globalMetaInfo_; }
  const MetaInfoMap& globalMetaInfo() const noexcept { return globalMetaInfo_; }

protected:
  /// \brief Construct meta-data from JSON
  ///
  /// This will read MetaData.json to initialize the savepoint vector as well as the fieldsTable and
  /// globalMetaInfo. Further, it will construct the Archive by reading ArchiveMetaData.json.
  void constructMetaDataFromJson();

  /// \brief Construct Archive from JSON
  ///
  /// \param archiveName  String passed to the ArchiveFactory to construct the Archive
  void constructArchive(const std::string& archiveName);

protected:
  OpenModeKind mode_;
  boost::filesystem::path directory_;

  std::vector<SavepointImpl> savepoints_;
  FieldMap fieldMap_;
  MetaInfoMap globalMetaInfo_;

  std::unique_ptr<Archive> archive_;
};

} // namespace serialbox

#endif
