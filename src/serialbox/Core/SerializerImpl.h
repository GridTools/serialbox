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
#include "serialbox/Core/Json.h"
#include "serialbox/Core/MetaInfoMap.h"
#include "serialbox/Core/SavepointVector.h"
#include "serialbox/Core/StorageView.h"
#include <boost/filesystem.hpp>
#include <iosfwd>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Shared implementation of the Serializer
///
/// Direct usage of this class is discouraged, use the Serializer classes provided by the Frontends
/// instead.
class SerializerImpl {
public:
  /// \brief Get the status of serialization
  ///
  /// The status is represented as an integer which can take the following values:
  ///
  /// - 0: the variable is not yet initialized -> the serialization is enabled if the environment
  ///   variable `SERIALBOX_SERIALIZATION_DISABLE` is not set to a positive value. The first
  ///   Serializer which is initialized has to set this value either to +1 or to -1 according to
  ///   the environment.
  /// - +1: the serialization is enabled, independently of the environment
  /// - -1: the serialization is disabled, independently of the environment
  ///
  /// \return serialization status
  static int serializationStatus() noexcept { return enabled_; }

  /// \brief Enable serialization
  ///
  /// Serialization is enabled by default, but it can be disabled either by setting the environment
  /// variable `SERIALBOX_SERIALIZATION_DISABLE` to a positive value or by calling the function
  /// disableSerialization. With this function you enable the serialization independently of the
  /// current environment.
  ///
  /// The serialization can only globally enabled or disabled. There is no way to enable or
  /// disable only a specific serializer.
  static void enableSerialization() noexcept { enabled_ = 1; }

  /// \brief Disable serialization
  ///
  /// Serialization is enabled by default, but it can be disabled either by setting the environment
  /// variable `SERIALBOX_SERIALIZATION_DISABLE` to a positive value or by calling the funtcion
  /// disableSerialization. With this function you disable the serialization independently of the
  /// current environment.
  ///
  /// The serialization can only be globally enabled or disabled. There is no way to enable or
  /// disable only a specific serializer.
  static void disableSerialization() noexcept { enabled_ = -1; }

  /// \brief Copy constructor [deleted]
  SerializerImpl(const SerializerImpl&) = delete;

  /// \brief Move constructor
  SerializerImpl(SerializerImpl&&) = default;

  /// \brief Copy assignment [deleted]
  SerializerImpl& operator=(const SerializerImpl&) = delete;

  /// \brief Move assignment
  SerializerImpl& operator=(SerializerImpl&&) = default;

  /// \brief Construct Serializer
  ///
  /// \param mode         Mode of the Serializer
  /// \param directory    Directory of the Archive and Serializer meta-data
  /// \param prefix       Prefix of all filenames
  /// \param archiveName  String passed to the ArchiveFactory to construct the Archive
  ///
  /// This will read `MetaData-prefix.json` to initialize the savepoint vector, the fieldMap and
  /// globalMetaInfo. Further, it will construct the Archive by reading the
  /// `ArchiveMetaData-prefix.json`.
  ///
  /// \throw Exception  Invalid directory or corrupted meta-data files
  SerializerImpl(OpenModeKind mode, const std::string& directory, const std::string& prefix,
                 const std::string& archiveName);

  /// \brief Access the mode of the serializer
  OpenModeKind mode() const noexcept { return mode_; }

  /// \brief Access the directory in which the Serializer and Archive are opened
  const boost::filesystem::path& directory() const noexcept { return directory_; }

  /// \brief Access prefix of all filenames
  const std::string& prefix() const noexcept { return prefix_; }

  /// \brief Name of the archive in use
  const std::string& archiveName() const noexcept { return archive_->name(); }

  /// \brief Access the path to the meta-data file
  const boost::filesystem::path& metaDataFile() const noexcept { return metaDataFile_; }

  /// \brief Drop all field and savepoint meta-data.
  ///
  /// This will also call Archive::clear() which may \b remove all related files on the disk.
  void clear() noexcept;

  //===----------------------------------------------------------------------------------------===//
  //     Global Meta-Information
  //===----------------------------------------------------------------------------------------===//

  /// \brief Add a new key-value pair to the global meta-information of the Serializer
  ///
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  ///
  /// \throw Exception  Value cannot be inserted as it already exists
  template <class StringType, class ValueType>
  void addGlobalMetaInfo(StringType&& key, ValueType&& value) {
    if(!globalMetaInfo_->insert(std::forward<StringType>(key), std::forward<ValueType>(value)))
      throw Exception("cannot add element with key '%s' to globalMetaInfo: element already exists",
                      key);
  }

  /// \brief Query globalMetaInfo map for key `key` and retrieve value as type `T`
  ///
  /// \param key    Key of the requested element
  ///
  /// \throw Exception  Key `key` does not exist in the globalMetaInfo map or value cannot be
  ///                   converted to type `T`
  template <class T, class StringType>
  T getGlobalMetainfoAs(StringType&& key) const {
    try {
      return globalMetaInfo_->at(key).template as<T>();
    } catch(Exception& e) {
      throw Exception("cannot get element with key '%s' from globalMetaInfo: %s", key, e.what());
    }
  }

  /// \brief Get a refrence to the global meta information
  MetaInfoMap& globalMetaInfo() noexcept { return *globalMetaInfo_; }
  const MetaInfoMap& globalMetaInfo() const noexcept { return *globalMetaInfo_; }

  /// \brief Get pointer to the global meta information
  std::shared_ptr<MetaInfoMap>& globalMetaInfoPtr() noexcept { return globalMetaInfo_; }
  const std::shared_ptr<MetaInfoMap>& globalMetaInfoPtr() const noexcept { return globalMetaInfo_; }

  //===----------------------------------------------------------------------------------------===//
  //     Register and Query Fields
  //===----------------------------------------------------------------------------------------===//

  /// \brief Register a new field within the Serializer
  ///
  /// \param name  Name of the the new field
  /// \param Args  Arguments forwarded to the constructor of FieldMetaInfo
  ///
  /// \throw Exception  Field with same name already exists
  template <class StringType, typename... Args>
  void registerField(StringType&& name, Args&&... args) {
    if(!fieldMap_->insert(std::forward<StringType>(name), std::forward<Args>(args)...))
      throw Exception("cannot register field '%s': field already exists", name);
  }

  /// \brief Check if field `name` has been registred within the Serializer
  ///
  /// \param name  Name of the the new field
  /// \return True iff the field is present
  template <class StringType>
  bool hasField(StringType&& name) const noexcept {
    return fieldMap_->hasField(std::forward<StringType>(name));
  }

  /// \brief Add `key = value` or `key = {value1, ..., valueN}` meta-information to field `name`
  ///
  /// \param name   Name of the field
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  /// \return Value indicating whether the element was successfully inserted or not
  ///
  /// \throw Exception  Field with name `name` does not exist in FieldMap
  template <class StringType, class KeyType, class ValueType>
  bool addFieldMetaInfo(StringType&& name, KeyType&& key, ValueType&& value) {
    return fieldMap_->getMetaInfoOf(name).insert(std::forward<KeyType>(key),
                                                 std::forward<ValueType>(value));
  }

  /// \brief Query FieldMap for field with `name` and return a refrence to FieldMetaInfo
  ///
  /// \param name  Name of the field
  ///
  /// \throw Exception  Field `name` does not exist in FieldMap
  template <class StringType>
  const FieldMetaInfo& getFieldMetaInfoOf(StringType&& name) const {
    return fieldMap_->getFieldMetaInfoOf(std::forward<StringType>(name));
  }

  /// \brief Get a vector of all registered fields
  /// \return Vector with the names of the registered fields
  std::vector<std::string> fieldnames() const;

  /// \brief Get refrence to the field map
  FieldMap& fieldMap() noexcept { return *fieldMap_; }
  const FieldMap& fieldMap() const noexcept { return *fieldMap_; }

  /// \brief Get pointer to the field map
  std::shared_ptr<FieldMap>& fieldMapPtr() noexcept { return fieldMap_; }
  const std::shared_ptr<FieldMap>& fieldMapPtr() const noexcept { return fieldMap_; }

  //===----------------------------------------------------------------------------------------===//
  //     Register and Query Savepoints
  //===----------------------------------------------------------------------------------------===//

  /// \brief Register a savepoint
  ///
  /// \param Args  Arguments forwarded to the constructor of Savepoint
  /// \return True iff the savepoint was successfully inserted
  template <typename... Args>
  bool registerSavepoint(Args&&... args) noexcept {
    return (savepointVector_->insert(SavepointImpl(std::forward<Args>(args)...)) != -1);
  }

  /// \brief Add a field to the savepoint
  /// \return True iff the field was successfully addeed to the savepoint
  bool addFieldToSavepoint(const SavepointImpl& savepoint, const FieldID& fieldID) noexcept {
    return savepointVector_->addField(savepoint, fieldID);
  }

  /// \brief Get the FielID of field `field` at savepoint `savepoint`
  ///
  /// \throw Exception  Savepoint or field at savepoint do not exist
  FieldID getFieldIDAtSavepoint(const SavepointImpl& savepoint, const std::string& field) const {
    return savepointVector_->getFieldID(savepoint, field);
  }

  /// \brief Get refrence to savepoint vector
  const SavepointVector::savepoint_vector_type& savepoints() const noexcept {
    return savepointVector_->savepoints();
  }
  SavepointVector::savepoint_vector_type& savepoints() noexcept {
    return savepointVector_->savepoints();
  }

  /// \brief Get refrence to SavepointVector
  const SavepointVector& savepointVector() const noexcept { return *savepointVector_; }
  SavepointVector& savepointVector() noexcept { return *savepointVector_; }

  /// \brief Get pointer to SavepointVector
  const std::shared_ptr<SavepointVector>& savepointVectorPtr() const noexcept {
    return savepointVector_;
  }
  std::shared_ptr<SavepointVector>& savepointVectorPtr() noexcept { return savepointVector_; }

  //===----------------------------------------------------------------------------------------===//
  //     Writing
  //===----------------------------------------------------------------------------------------===//

  /// \brief Serialize field `name` (given as `storageView`) at `savepoint` to disk
  ///
  /// The method perfoms the following steps:
  ///
  /// 1. Check if field `name` is registred within the Serializer and perform a consistency check
  ///    concering the data-type and dimensions of the StorageView compared to to the registered
  ///    field.
  ///
  /// 2. Locate the `savepoint` in the savepoint vector and, if the `savepoint` does not exist,
  ///    register it within the Serializer.
  ///
  /// 3. Check if field `name` can be added to Savepoint.
  ///
  /// 4. Pass the StorageView to the backend Archive and perform actual data-serialization.
  ///
  /// 5. Register field `name` within the Savepoint.
  ///
  /// 6. Update meta-data on disk via SerializerImpl::updateMetaData()
  ///
  /// \param name           Name of the field
  /// \param savepoint      Savepoint at which the field will be serialized
  /// \param storageView    StorageView of the field
  ///
  /// \throw Exception
  ///
  /// \see Archive
  void write(const std::string& name, const SavepointImpl& savepoint,
             const StorageView& storageView);

  //===----------------------------------------------------------------------------------------===//
  //     Reading
  //===----------------------------------------------------------------------------------------===//

  /// \brief Deserialize field `name` (given as `storageView`) at `savepoint` from disk
  ///
  /// The method perfoms the following steps:
  ///
  /// 1. Check if field `name` is registred within the Serializer and perform a consistency check
  ///    concering the data-type and dimensions of the StorageView compared to to the registered
  ///    field.
  ///
  /// 2. Check if savepoint exists and has a field `name`.
  ///
  /// 3. Pass the StorageView to the backend Archive and perform actual data-deserialization.
  ///
  /// \param name           Name of the field
  /// \param savepoint      Savepoint at which the field will be deserialized
  /// \param storageView    StorageView of the field
  ///
  /// \throw Exception
  ///
  /// \see Archive
  void read(const std::string& name, const SavepointImpl& savepoint, StorageView& storageView);

  //===----------------------------------------------------------------------------------------===//
  //     JSON Serialization
  //===----------------------------------------------------------------------------------------===//

  /// \brief Convert meta-data to JSON and serialize to MetaData-prefix.json and
  /// ArchiveMetaData-prefix.json
  ///
  /// This will ensure MetaData-prefix.json is up-to-date with the in-memory versions of the
  /// savepointVector, fieldMap and globalMetaInfo as well as the meta-data of the Archive.
  void updateMetaData();

  /// \brief Convert all members of the Serializer to JSON
  json::json toJSON() const;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SerializerImpl& s);

protected:
  /// \brief Construct meta-data from JSON
  ///
  /// This will read MetaData-prefix.json to initialize the savepoint vector, the fieldMap and
  /// globalMetaInfo.
  void constructMetaDataFromJson();

  /// \brief Construct Archive from JSON
  ///
  /// This will read ArchiveMetaData-prefix.json and initialize the archive.
  ///
  /// \param archiveName  String passed to the ArchiveFactory to construct the Archive
  void constructArchive(const std::string& archiveName);

  /// \brief Check if `storageView` is consistent with the field `name`
  ///
  /// \return FieldMetaInfo of field `name`
  /// \throw Exception    Inconsistency is detected
  std::shared_ptr<FieldMetaInfo> checkStorageView(const std::string& name,
                                                  const StorageView& storageView) const;

  /// \brief Check if the current directory contains meta-information of an older version of
  /// serialbox and upgrade it if necessary
  ///
  /// The function will check if there is a `prefix.json` file which is newer than
  /// `MetaData-prefix.json` and, if ture, convert `prefix.json` to `MetaData-prefix.json` and
  /// `ArchiveMetaData.json`.
  ///
  /// Older versions of serialbox can only be \b opened in `OpenModeKind::Read` and will use the
  /// BinaryArchive.
  ///
  /// \return True iff the upgrade was successful
  ///
  /// \throw Exception
  bool upgradeMetaData();

protected:
  OpenModeKind mode_;
  boost::filesystem::path directory_;
  boost::filesystem::path metaDataFile_;
  std::string prefix_;

  std::shared_ptr<SavepointVector> savepointVector_;
  std::shared_ptr<FieldMap> fieldMap_;
  std::shared_ptr<MetaInfoMap> globalMetaInfo_;

  std::unique_ptr<Archive> archive_;

  // This variable can take three values:
  //
  //  0: the variable is not yet initialized -> the serialization is enabled if the environment
  //     variable SERIALBOX_SERIALIZATION_DISABLE is not set to  a positive value. The first
  //     serializer which is initialized has to set this value either to +1 or to -1 according to
  //     the environment.
  // +1: the serialization is enabled, independently of the environment
  // -1: the serialization is disabled, independently of the environment
  //
  // The value is initialized to 0
  static int enabled_;
};

/// @}

} // namespace serialbox

#endif
