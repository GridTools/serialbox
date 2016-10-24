//===-- serialbox/Core/Frontend/gridtools/Serializer.h ------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the gridtools implementation of serializer.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_SERIALIZER_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_SERIALIZER_H

#include "serialbox/Core/Frontend/gridtools/Exception.h"
#include "serialbox/Core/Frontend/gridtools/FieldMetaInfo.h"
#include "serialbox/Core/Frontend/gridtools/MetaInfoMap.h"
#include "serialbox/Core/Frontend/gridtools/Savepoint.h"
#include "serialbox/Core/Frontend/gridtools/StorageViewHelper.h"
#include "serialbox/Core/Frontend/gridtools/Type.h"
#include "serialbox/Core/SerializerImpl.h"
#include <memory>

namespace serialbox {

/// \namespace gridtools
/// \brief Namespace of the gridtools frontend
namespace gridtools {

/// \brief Serializer implemenation of the gridtools frontend
///
/// \ingroup gridtools
class serializer {
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
  static int serializationStatus() noexcept { return SerializerImpl::serializationStatus(); }

  /// \brief Enable serialization
  ///
  /// Serialization is enabled by default, but it can be disabled either by setting the environment
  /// variable `SERIALBOX_SERIALIZATION_DISABLE` to a positive value or by calling the function
  /// disableSerialization. With this function you enable the serialization independently of the
  /// current environment.
  ///
  /// The serialization can only globally enabled or disabled. There is no way to enable or
  /// disable only a specific serializer.
  static void enableSerialization() noexcept { SerializerImpl::enableSerialization(); }

  /// \brief Disable serialization
  ///
  /// Serialization is enabled by default, but it can be disabled either by setting the environment
  /// variable `SERIALBOX_SERIALIZATION_DISABLE` to a positive value or by calling the funtcion
  /// disableSerialization. With this function you disable the serialization independently of the
  /// current environment.
  ///
  /// The serialization can only be globally enabled or disabled. There is no way to enable or
  /// disable only a specific serializer.
  static void disableSerialization() noexcept { SerializerImpl::disableSerialization(); }

  /// \brief Construct the serializer
  ///
  /// \param mode          Mode of the Serializer
  /// \param directory     Directory of the Archive and Serializer meta-data
  /// \param prefix        Prefix of all filenames
  /// \param archive_name  String passed to the ArchiveFactory to construct the Archive
  ///
  /// \throw exception  Invalid directory or corrupted meta-data files
  serializer(open_mode mode, const std::string& directory, const std::string& prefix,
             std::string archive_name = "Binary")
      : serializerImpl_(std::make_shared<SerializerImpl>(mode, directory, prefix, archive_name)) {}

  /// \brief Copy constructor
  ///
  /// This performs a shallow copy of the Serializer.
  serializer(const serializer&) = default;

  /// \brief Move constructor
  serializer(serializer&&) = default;

  /// \brief Copy assignment
  ///
  /// This performs a shallow copy of the Serializer.
  serializer& operator=(const serializer&) = delete;

  /// \brief Move assignment
  serializer& operator=(serializer&&) = default;

  /// \brief Access the mode of the serializer
  open_mode mode() const noexcept { return serializerImpl_->mode(); }

  /// \brief Access the directory in which the Serializer and Archive are opened
  const std::string& directory() const noexcept { return serializerImpl_->directory().string(); }

  /// \brief Access prefix of all filenames
  const std::string& prefix() const noexcept { return serializerImpl_->prefix(); }

  /// \brief Access the path to the meta-data file
  const std::string& meta_data_file() const noexcept {
    return serializerImpl_->metaDataFile().string();
  }

  /// \brief Name of the archive in use
  const std::string& archive_name() const noexcept { return serializerImpl_->archiveName(); }

  /// \brief Drop all field and savepoint meta-data.
  ///
  /// This will also call Archive::clear() which may \b remove all related files on the disk.
  void clear() noexcept { serializerImpl_->clear(); }

  /// \brief Convert meta-data to JSON and serialize to MetaData-prefix.json and
  /// ArchiveMetaData-prefix.json
  ///
  /// This will ensure MetaData-prefix.json is up-to-date with the in-memory version of the
  /// Serializer data-structures. This method is implicitly called by serializer::write().
  void update_meta_data() { serializerImpl_->updateMetaData(); }

  //===----------------------------------------------------------------------------------------===//
  //     Global Meta-Information
  //===----------------------------------------------------------------------------------------===//

  /// \brief Add a new key-value pair to the global meta-information of the serializer
  ///
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  ///
  /// \throw exception  Value cannot be inserted as it already exists
  template <class StringType, class ValueType>
  void add_global_meta_info(StringType&& key, ValueType&& value) {
    serializerImpl_->addGlobalMetaInfo(std::forward<StringType>(key),
                                       std::forward<ValueType>(value));
  }

  /// \brief Query global meta_info_map for `key` and retrieve value as type `T`
  ///
  /// \param key    Key of the requested element
  ///
  /// \throw exception  Key `key` does not exist in the globalMetaInfo map or value cannot be
  ///                   converted to type `T`
  template <class T, class StringType>
  T get_global_meta_info_as(StringType&& key) const {
    return serializerImpl_->getGlobalMetainfoAs<T>(std::forward<StringType>(key));
  }

  /// \brief Get a refrence to the global meta information
  meta_info_map global_meta_info() noexcept {
    return meta_info_map(serializerImpl_->globalMetaInfoPtr());
  }

  //===----------------------------------------------------------------------------------------===//
  //     Register and Query Fields
  //===----------------------------------------------------------------------------------------===//

  /// \brief Register a new field within the serializer
  ///
  /// \param name  Name of the the new field
  /// \param Args  Arguments forwarded to the constructor of FieldMetaInfo
  ///
  /// \throw exception  Field with same name already exists
  template <class StringType, typename... Args>
  void register_field(StringType&& name, Args&&... args) {
    serializerImpl_->registerField(std::forward<StringType>(name),
                                   *field_meta_info(std::forward<Args>(args)...).impl());
  }

  /// \brief Check if field `name` has been registred within the serializer
  ///
  /// \param name  Name of the the new field
  /// \return True iff the field is present
  template <class StringType>
  bool has_field(StringType&& name) const noexcept {
    return serializerImpl_->hasField(std::forward<StringType>(name));
  }

  /// \brief Add `key = value` or `key = {value1, ..., valueN}` meta-information to field `name`
  ///
  /// \param name   Name of the field
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  /// \return Value indicating whether the element was successfully inserted or not
  ///
  /// \throw exception  Field with name `name` does not exist in FieldMap
  template <class StringType, class KeyType, class ValueType>
  bool add_meta_info_to_field(StringType&& name, KeyType&& key, ValueType&& value) {
    return serializerImpl_->addFieldMetaInfo(
        std::forward<StringType>(name), std::forward<KeyType>(key), std::forward<ValueType>(value));
  }

  /// \brief Get field_meta_info of field `name`
  ///
  /// \param name  Name of the field
  ///
  /// \throw exception  Field `name` does not exist
  template <class StringType>
  field_meta_info get_field_meta_info(StringType&& name) const {
    return field_meta_info(
        serializerImpl_->fieldMap().getFieldMetaInfoPtrOf(std::forward<StringType>(name)));
  }

  /// \brief Get a vector of all registered fields
  /// \return Vector with the names of the registered fields
  std::vector<std::string> fieldnames() const { return serializerImpl_->fieldnames(); }

  //===----------------------------------------------------------------------------------------===//
  //     Register and Query Savepoints
  //===----------------------------------------------------------------------------------------===//

  /// \brief Register a savepoint
  ///
  /// \param Args  Arguments forwarded to the constructor of Savepoint
  /// \return True iff the savepoint was successfully inserted
  template <typename... Args>
  bool register_savepoint(Args&&... args) noexcept {
    return serializerImpl_->registerSavepoint(*savepoint(std::forward<Args>(args)...).impl());
  }

  /// \brief Check if savepoint exists
  ///
  /// \param sp     Savepoint to check for
  /// \return True iff the savepoint was successfully inserted
  template <typename... Args>
  bool has_savepoint(const savepoint sp) const noexcept {
    return serializerImpl_->savepointVector().exists(*sp.impl());
  }

  /// \brief Get a refrence to savepoint vector
  const std::vector<savepoint>& savepoints() {
    const auto& savepoints = serializerImpl_->savepointVector().savepoints();
    if(!savepoints_ || (savepoints.size() != savepoints_->size())) {
      savepoints_ = std::make_shared<std::vector<savepoint>>();
      for(std::size_t i = 0; i < savepoints.size(); ++i)
        savepoints_->emplace_back(savepoints[i]);
    }
    return *savepoints_;
  }

  //===----------------------------------------------------------------------------------------===//
  //     Writing
  //===----------------------------------------------------------------------------------------===//

  /// \brief Serialize field `name` given as `storage` at `savepoint` to disk
  ///
  /// \param name             Name of the field
  /// \param sp               Savepoint at which the field will be serialized
  /// \param storage          gridtools storage i.e object of type `gridtools::storage_type`
  /// \param register_field   Register field if not yet present
  ///
  /// \throw exception  Serialization failed
  ///
  /// \see
  ///   SerializerImpl::write
  template <class StorageType>
  void write(const std::string& name, const savepoint& sp, const StorageType& storage,
             bool register_field = true) {

    if(register_field && !serializerImpl_->fieldMap().hasField(name))
      this->register_field(name, storage);

    StorageView storageView(
        internal::get_origin_ptr(storage, 0), ToTypeID<typename StorageType::value_type>::value,
        std::move(internal::get_dims(storage)), std::move(internal::get_strides(storage)));

    serializerImpl_->write(name, *sp.impl(), storageView);
  }

  //===----------------------------------------------------------------------------------------===//
  //     Reading
  //===----------------------------------------------------------------------------------------===//

  /// \brief Deserialize field `name` given as `storage` at `savepoint` to disk
  ///
  /// \param name       Name of the field
  /// \param sp         Savepoint at which the field will be deserialized
  /// \param storage    gridtools storage i.e object of type `gridtools::storage_type`
  ///
  /// \throw exception  Deserialization failed
  ///
  /// \see
  ///   SerializerImpl::read
  template <class StorageType>
  void read(const std::string& name, const savepoint& sp, StorageType& storage) {
    StorageView storageView(
        internal::get_origin_ptr(storage, 0), ToTypeID<typename StorageType::value_type>::value,
        std::move(internal::get_dims(storage)), std::move(internal::get_strides(storage)));

    serializerImpl_->read(name, *sp.impl(), storageView);
  }

private:
  std::shared_ptr<SerializerImpl> serializerImpl_;
  std::shared_ptr<std::vector<savepoint>> savepoints_;
};

} // namespace gridtools

} // namespace serialbox

#endif
