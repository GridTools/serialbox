//===-- serialbox/Core/FieldMap.h ---------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the field map which stores the meta-information of each field.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FIELDMAP_H
#define SERIALBOX_CORE_FIELDMAP_H

#include "serialbox/Core/FieldMetaInfo.h"
#include "serialbox/Core/Json.h"
#include <unordered_map>

namespace serialbox {

/// \brief Hash-map to query the meta-information of the registered fields
class FieldMap {
public:
  /// \brief Type of the underlying hash-map
  using map_type = std::unordered_map<std::string, std::shared_ptr<FieldMetaInfo>>;

  /// \brief Type of an entry of the MetaInfoMap
  /// (`std::pair<std::string, std::shared_ptr<FieldMetaInfo>>`)
  using value_type = map_type::value_type;

  /// \brief Type of the key (`std::string`) i.e name of the fields
  using key_type = map_type::key_type;

  /// \brief Type of the value (`std::shared_ptr<FieldMetaInfo>`)
  using mapped_type = map_type::mapped_type;

  /// \brief An unsigned integral type (`std::size_t`)
  using size_type = map_type::size_type;

  /// \brief A forward iterator to `value_type`
  using iterator = map_type::iterator;

  /// \brief A forward iterator to `const value_type`
  using const_iterator = map_type::const_iterator;

  /// \brief Default constructor (empty table)
  FieldMap() : map_(){};

  /// \brief Construct from json
  explicit FieldMap(const json::json jsonNode) { fromJSON(jsonNode); }

  /// \brief Copy constructor [deleted]
  FieldMap(const FieldMap&) = default;

  /// \brief Move constructor
  FieldMap(FieldMap&&) = default;

  /// \brief Copy assignment
  FieldMap& operator=(const FieldMap&) = default;

  /// \brief Move assignment
  FieldMap& operator=(FieldMap&&) = default;

  /// \brief Check if field with name `name` exists in the FieldMap
  ///
  /// \param name  Name of the field
  /// \return True iff the field is present
  template <class StringType>
  bool hasField(StringType&& name) const noexcept {
    return (map_.find(name) != map_.end());
  }

  /// \brief Search the FieldMap for a field with name `name` and returns an iterator to it if
  /// found, otherwise returns an iterator to FieldMap::end
  ///
  /// \param name  Name of the field
  /// \return  Iterator to field if found, otherwise an iterator to `FieldMap::end`
  template <class StringType>
  iterator findField(StringType&& name) noexcept {
    return (map_.find(name));
  }

  template <class StringType>
  const_iterator findField(StringType&& name) const noexcept {
    return (map_.find(name));
  }

  /// \brief Get FieldMetaInfo of field `name`
  ///
  /// \param name  Name of the field
  /// \throw Exception Field `name` does not exist in FieldMap
  template <class StringType>
  FieldMetaInfo& getFieldMetaInfoOf(StringType&& name) {
    return *getFieldMetaInfoPtrOf(std::forward<StringType>(name));
  }

  template <class StringType>
  const FieldMetaInfo& getFieldMetaInfoOf(StringType&& name) const {
    return *getFieldMetaInfoPtrOf(std::forward<StringType>(name));
  }
  
  /// \brief Get pointer to FieldMetaInfo of field `name`
  ///
  /// \param name  Name of the field
  /// \throw Exception Field `name` does not exist in FieldMap
  template <class StringType>
  std::shared_ptr<FieldMetaInfo>& getFieldMetaInfoPtrOf(StringType&& name) {
    iterator fieldIt = map_.find(name);
    if(fieldIt != map_.end())
      return fieldIt->second;
    throw Exception("field '%s' does not exist", name);
  }

  template <class StringType>
  const std::shared_ptr<FieldMetaInfo>& getFieldMetaInfoPtrOf(StringType&& name) const {
    const_iterator fieldIt = map_.find(name);
    if(fieldIt != map_.end())
      return fieldIt->second;
    throw Exception("field '%s' does not exis", name);
  }

  /// \brief Get MetaInfo of field with name `name`
  ///
  /// \param name  Name of the field
  /// \throw Exception Field with name `name` does not exist in FieldMap
  template <class StringType>
  MetaInfoMap& getMetaInfoOf(StringType&& name) {
    return getFieldMetaInfoOf(name).metaInfo();
  }

  template <class StringType>
  const MetaInfoMap& getMetaInfoOf(StringType&& name) const {
    return getFieldMetaInfoOf(name).metaInfo();
  }

  /// \brief Get Dimensions of field with name `name`
  ///
  /// \param name  Name of the field
  /// \throw Exception Field with name `name` does not exist in FieldMap
  template <class StringType>
  std::vector<int>& getDimsOf(StringType&& name) {
    iterator fieldIt = map_.find(name);
    if(fieldIt != map_.end())
      return fieldIt->second->dims();
    throw Exception("field '%s' does not exist in FieldMap", name);
  }

  template <class StringType>
  const std::vector<int>& getDimsOf(StringType&& name) const {
    const_iterator fieldIt = map_.find(name);
    if(fieldIt != map_.end())
      return fieldIt->second->dims();
    throw Exception("field '%s' does not exist in FieldMap", name);
  }

  /// \brief Get Type of field with name `name`
  ///
  /// \param name  Name of the field
  /// \throw Exception Field with name `name` does not exist in FieldMap
  template <class StringType>
  TypeID getTypeOf(StringType&& name) const {
    const_iterator fieldIt = map_.find(name);
    if(fieldIt != map_.end())
      return fieldIt->second->type();
    throw Exception("field '%s' does not exist in FieldMap", name);
  }

  /// \brief Insert a new field in the map
  ///
  /// The field is inserted only if its name is not equivalent to the name of any other field
  /// already in the map (fields in a FieldMap are unique).
  ///
  /// \param name  Name of the the new field
  /// \param Args  Arguments forwarded to the constructor of FieldMetaInfo
  /// \return Value indicating whether the field was successfully inserted or not
  template <class StringType, typename... Args>
  bool insert(StringType&& key, Args&&... args) {
    return map_.insert({key, std::make_shared<FieldMetaInfo>(std::forward<Args>(args)...)}).second;
  }

  /// \brief Returns the number of elements in the FieldMap
  std::size_t size() const noexcept { return map_.size(); }

  /// \brief Returns a bool value indicating whether the FieldMap is empty
  bool empty() const noexcept { return map_.empty(); }

  /// \brief All the elements in the FieldMap are dropped: their destructors are called, and they
  /// are removed from the container, leaving it with a size of 0
  void clear() noexcept { map_.clear(); }

  /// \brief Returns an iterator pointing to the first element in the FieldMap
  iterator begin() noexcept { return map_.begin(); }
  const_iterator begin() const noexcept { return map_.begin(); }

  /// \brief Returns an iterator pointing to the past-the-end element in the FieldMap
  iterator end() noexcept { return map_.end(); }
  const_iterator end() const noexcept { return map_.end(); }

  /// \brief Swap with other
  void swap(FieldMap& other) noexcept { map_.swap(other.map_); }

  /// \brief Test for equality
  bool operator==(const FieldMap& right) const noexcept { return (map_ == right.map_); }

  /// \brief Test for inequality
  bool operator!=(const FieldMap& right) const noexcept { return (!(*this == right)); }

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const FieldMap& s);

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

private:
  map_type map_;
};

} // namespace serialbox

#endif
