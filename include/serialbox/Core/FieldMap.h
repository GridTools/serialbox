//===-- serialbox/Core/FieldMap.h --------------------------------------------------*- C++ -*-===//
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
  using map_type = std::unordered_map<std::string, FieldMetaInfo>;

  /// \brief Type of an entry of the MetaInfoMap (`std::pair<std::string, FieldMetaInfo>`)
  using value_type = map_type::value_type;

  /// \brief Type of the key (`std::string`)
  using key_type = map_type::key_type;

  /// \brief Type of the value (`FieldMetaInfo`)
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
  FieldMap(const FieldMap&) = delete;

  /// \brief Move constructor
  FieldMap(FieldMap&&) = default;

  /// \brief Copy assignment [deleted]
  FieldMap& operator=(const FieldMap&) = delete;

  /// \brief Move assignment
  FieldMap& operator=(FieldMap&&) = default;

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
