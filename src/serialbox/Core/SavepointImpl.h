//===-- serialbox/Core/SavepointImpl.h ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the implementation of the Savepoint.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SAVEPOINTIMPL_H
#define SERIALBOX_CORE_SAVEPOINTIMPL_H

#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Json.h"
#include "serialbox/Core/MetaInfoMap.h"
#include <functional>
#include <iosfwd>
#include <string>
#include <utility>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Shared implementation of the Savepoint
///
/// Savepoints have a specialization of std::hash and can thus be used in hash-maps such as
/// std::unordered_map.
///
/// Direct usage of this class is discouraged, use the Savepoint classes provided by the Frontends
/// instead.
class SavepointImpl {
public:
  /// \brief Construct an empty savepoint (wihtout `metaInfo` i.e this->empty() == true)
  template <class StringType,
            class = typename std::enable_if<!std::is_same<StringType, json::json>::value>::type>
  explicit SavepointImpl(const StringType& name)
      : name_(name), metaInfo_(std::make_shared<MetaInfoMap>()){};

  /// \brief Construct savepoint with `name` and `metaInfo`
  template <class StringType, class MetaInfoType>
  SavepointImpl(StringType&& name, MetaInfoType&& metaInfo)
      : name_(name),
        metaInfo_(std::make_shared<MetaInfoMap>(std::forward<MetaInfoType>(metaInfo))){};

  /// \brief Copy constructor
  SavepointImpl(const SavepointImpl& other) { *this = other; };

  /// \brief Move constructor
  SavepointImpl(SavepointImpl&&) = default;

  /// \brief Construct from JSON
  explicit SavepointImpl(const json::json& jsonNode) { fromJSON(jsonNode); }

  /// \brief Copy assignment
  SavepointImpl& operator=(const SavepointImpl& other);

  /// \brief Move assignment
  SavepointImpl& operator=(SavepointImpl&&) = default;

  /// \brief Add a new `key = value` pair to the `metaInfo` of the Savepoint
  ///
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  ///
  /// \throw Exception  Value cannot be inserted as it already exists
  template <class StringType, class ValueType>
  void addMetaInfo(StringType&& key, ValueType&& value) {
    if(!metaInfo_->insert(std::forward<StringType>(key), std::forward<ValueType>(value)))
      throw Exception("cannot add element with key '%s' to metaInfo: element already exists", key);
  }

  /// \brief Query `metaInfo` for key `key` and retrieve value as type `T`
  ///
  /// \param key    Key of the requested element
  ///
  /// \throw Exception  Key `key` does not exist in the metaInfo map or value cannot be
  ///                   converted to type `T`
  template <class T, class StringType>
  T getMetaInfoAs(StringType&& key) const {
    try {
      return metaInfo_->at(key).template as<T>();
    } catch(Exception& e) {
      throw Exception("cannot get element with key '%s' from metaInfo: %s", key, e.what());
    }
  }

  /// \brief Test for equality
  bool operator==(const SavepointImpl& right) const {
    return (name_ == right.name_) && (*metaInfo_ == *right.metaInfo_);
  }

  /// \brief Test for inequality
  bool operator!=(const SavepointImpl& right) const { return (!(*this == right)); }

  /// \brief Swap with other
  void swap(SavepointImpl& other) noexcept {
    name_.swap(other.name_);
    metaInfo_->swap(*other.metaInfo_);
  }

  /// \brief Access name
  const std::string& name() const noexcept { return name_; }

  /// \brief Access meta-info
  MetaInfoMap& metaInfo() noexcept { return *metaInfo_; }
  const MetaInfoMap& metaInfo() const noexcept { return *metaInfo_; }

  /// \brief Returns a bool value indicating whether the savepoint is empty (i.e has no
  /// meta-information attached)
  bool empty() const noexcept { return metaInfo_->empty(); }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

  /// \brief Convert savepoint to string
  std::string toString() const;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s);

  /// \brief Get meta-info pointer
  std::shared_ptr<MetaInfoMap>& metaInfoPtr() noexcept { return metaInfo_; }
  const std::shared_ptr<MetaInfoMap>& metaInfoPtr() const noexcept { return metaInfo_; }

protected:
  std::string name_;                      ///< Name of this savepoint
  std::shared_ptr<MetaInfoMap> metaInfo_; ///< Meta-information of this savepoint
};

/// @}

} // namespace serialbox

namespace std {

/// \brief Specialization of `std::hash<T>` for [T = serialbox::Savepoint]
///
/// Savepoints are hashed on their name (std::string). Although, the name of a savepoint is not
/// unique, it is a reasoanble compromise as we assume there are only O(1) savepoints sharing the
/// same name.
template <>
struct hash<serialbox::SavepointImpl> {
  std::size_t operator()(const serialbox::SavepointImpl& s) const noexcept {
    return std::hash<std::string>()(s.name());
  }
};

} // namespace std

#endif
