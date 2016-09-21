//===-- serialbox/Core/Savepoint.h ----------------------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_SAVEPOINT_H
#define SERIALBOX_CORE_SAVEPOINT_H

#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Json.h"
#include "serialbox/Core/MetaInfoMap.h"
#include <functional>
#include <iosfwd>
#include <string>
#include <utility>

namespace serialbox {

/// \brief Implementation of the Savepoint
///
/// Savepoints have a specialization of std::hash and can thus be used in hash-maps such as
/// std::unordered_map.
class Savepoint {
public:
  /// \brief Construct an empty savepoint (wihtout ´metaInfo´ i.e this->empty() == true)
  template <class StringType,
            class = typename std::enable_if<!std::is_same<StringType, json::json>::value>::type>
  explicit Savepoint(const StringType& name) : name_(name), metaInfo_(){};

  /// \brief Construct savepoint with ´name´ and ´metaInfo´
  template <class StringType, class MetaInfoType>
  Savepoint(StringType&& name, MetaInfoType&& metaInfo) : name_(name), metaInfo_(metaInfo){};

  /// \brief Copy constructor
  Savepoint(const Savepoint&) = default;

  /// \brief Move constructor
  Savepoint(Savepoint&&) = default;

  /// \brief Construct from JSON
  explicit Savepoint(const json::json& jsonNode) { fromJSON(jsonNode); }

  /// \brief Copy assignment
  Savepoint& operator=(const Savepoint&) = default;

  /// \brief Move assignment
  Savepoint& operator=(Savepoint&&) = default;

  /// \brief Add a new key-value pair to the ´metaInfo´ of the Savepoint
  ///
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  ///
  /// \throw Exception  Value cannot be inserted as it already exists
  template <class StringType, class ValueType>
  void addMetaInfo(StringType&& key, ValueType&& value) {
    if(!metaInfo_.insert(std::forward<StringType>(key), std::forward<ValueType>(value)))
      throw Exception("cannot add element with key '%s' to metaInfo: element already exists", key);
  }

  /// \brief Query ´metaInfo´ for key ´key´ and retrieve value as type ´T´
  ///
  /// \param key    Key of the requested element
  ///
  /// \throw Exception  Key ´key´ does not exist in the metaInfo map or value cannot be
  ///                   converted to type ´T´
  template <class T, class StringType>
  const T& getMetaInfoAs(StringType&& key) const {
    try {
      return metaInfo_.at(key).template as<T>();
    } catch(Exception& e) {
      throw Exception("cannot get element with key '%s' from metaInfo: %s", key, e.what());
    }
  }

  /// \brief Test for equality
  bool operator==(const Savepoint& right) const {
    return (name_ == right.name_) && (metaInfo_ == right.metaInfo_);
  }

  /// \brief Test for inequality
  bool operator!=(const Savepoint& right) const { return (!(*this == right)); }

  /// \brief Swap with other
  void swap(Savepoint& other) noexcept {
    name_.swap(other.name_);
    metaInfo_.swap(other.metaInfo_);
  }

  /// \brief Access name
  const std::string& name() const noexcept { return name_; }

  /// \brief Access meta-info
  MetaInfoMap& metaInfo() noexcept { return metaInfo_; }
  const MetaInfoMap& metaInfo() const noexcept { return metaInfo_; }

  /// \brief Returns a bool value indicating whether the savepoint is empty (i.e has no
  /// meta-information attached)
  bool empty() const noexcept { return metaInfo_.empty(); }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);
  
  /// \brief Convert savepoint to string
  std::string toString() const;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const Savepoint& s);

protected:
  std::string name_;     ///< Name of this savepoint
  MetaInfoMap metaInfo_; ///< Meta-information of this savepoint
};

} // namespace serialbox

namespace std {

/// \brief Specialization of std::hash<T> for [T = serialbox::Savepoint]
template <>
struct hash<serialbox::Savepoint> {
  std::size_t operator()(const serialbox::Savepoint& s) const noexcept {
    return std::hash<std::string>()(s.name());
  }
};
}

#endif
