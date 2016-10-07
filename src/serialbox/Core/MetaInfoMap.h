//===-- serialbox/Core/MetaInfoMap.h ------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the meta-information map.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_METAINFOMAP_H
#define SERIALBOX_CORE_METAINFOMAP_H

#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Json.h"
#include "serialbox/Core/MetaInfoValue.h"
#include "serialbox/Core/Type.h"
#include <iosfwd>
#include <string>
#include <unordered_map>
#include <utility>

namespace serialbox {

/// \brief Hash-map of meta-information of the form ´key = value´ pair
///
/// They keys are strings (std::string), while the values can be booleans, integers (32 and 64 bit),
/// floating point numbers (32 and 64 bit) or strings.
class MetaInfoMap {
public:
  /// \brief Type of the underlying hash-map
  using map_type = std::unordered_map<std::string, MetaInfoValue>;

  /// \brief Type of an entry of the MetaInfoMap (`std::pair<std::string, MetaInfoMap::Value>`)
  using value_type = map_type::value_type;

  /// \brief Type of the key (`std::string`)
  using key_type = map_type::key_type;

  /// \brief Type of the value (`MetaInfoMap::Value`)
  using mapped_type = map_type::mapped_type;

  /// \brief An unsigned integral type (`std::size_t`)
  using size_type = map_type::size_type;

  /// \brief A forward iterator to `value_type`
  using iterator = map_type::iterator;

  /// \brief A forward iterator to `const value_type`
  using const_iterator = map_type::const_iterator;

  /// \brief Default constructor (empty map)
  MetaInfoMap() : map_(){};

  /// \brief Construct from json
  explicit MetaInfoMap(const json::json& jsonNode) { fromJSON(jsonNode); }

  /// \brief Construct from initalizer-list
  explicit MetaInfoMap(std::initializer_list<value_type> list) : map_(list){};

  /// \brief Copy constructor
  MetaInfoMap(const MetaInfoMap&) = default;

  /// \brief Move constructor
  MetaInfoMap(MetaInfoMap&&) = default;

  /// \brief Copy assignment
  MetaInfoMap& operator=(const MetaInfoMap&) = default;

  /// \brief Move assignment
  MetaInfoMap& operator=(MetaInfoMap&&) = default;

  /// \brief Check if key exists in the map
  ///
  /// \param key  Key to be searched for
  ///
  /// \return True iff the key is present
  template <class StringType>
  bool hasKey(StringType&& key) const noexcept {
    return (map_.find(key) != map_.end());
  }

  /// \brief Search the container for an element with a key equivalent to ´key´ and return an
  /// iterator to it if found, otherwise it returns an iterator to MetaInfoMap::end
  ///
  /// \param key  Key to be searched for
  ///
  /// \return An iterator to the element, if an element with specified key is found, or
  /// MetaInfoMap::end otherwise
  template <class StringType>
  iterator find(StringType&& key) noexcept {
    return map_.find(key);
  }
  template <class StringType>
  const_iterator find(StringType&& key) const noexcept {
    return map_.find(key);
  }

  /// \brief Insert a new element in the map
  ///
  /// The element is inserted only if its key is not equivalent to the key of any other element
  /// already in the map (keys in a MetaInfoMap are unique).
  ///
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  ///
  /// \return Value indicating whether the element was successfully inserted or not
  template <class KeyType, class ValueType>
  bool insert(KeyType&& key, ValueType&& value) noexcept {
    return (map_.insert({key, MetaInfoValue(std::forward<ValueType>(value))}).second);
  }

  /// \brief Convert value of element with key ´key´ to type ´T´
  ///
  /// If the type ´T´ is different than the internally stored type, the function does its best to
  /// convert the value to ´T´ in a meaningful way.
  ///
  /// \param key    Key of the new element
  ///
  /// \return Copy of the value of the element as type ´T´
  ///
  /// \throw Exception  Key ´key´ does not exist or conversion results in truncation of the value
  ///
  /// \see MetaInfoValue::as
  template <class ValueType, class KeyType>
  ValueType as(KeyType&& key) const {
    return at(key).template as<ValueType>();
  }

  /// \brief Removes from the MetaInfoMap either a single element or a range of
  /// elements [first,last)
  iterator erase(const_iterator position) { return map_.erase(position); }
  size_type erase(const key_type& key) { return map_.erase(key); }
  iterator erase(const_iterator first, const_iterator last) { return map_.erase(first, last); }

  /// \brief Return a reference to mapped value given by key
  mapped_type& operator[](const key_type& key) { return map_[key]; }
  mapped_type& operator[](key_type&& key) { return map_[key]; }

  /// \brief Return a reference to mapped value given by key
  ///
  /// \throw Exception  No mapped value with given key exists
  mapped_type& at(const key_type& key);
  const mapped_type& at(const key_type& key) const;

  /// \brief Returns the number of elements in the MetaInfoMap
  std::size_t size() const noexcept { return map_.size(); }

  /// \brief Returns a bool value indicating whether the MetaInfoMap is empty, i.e. whether its size
  /// is 0
  bool empty() const noexcept { return map_.empty(); }

  /// \brief All the elements in the MetaInfoMap are dropped: their destructors are called, and they
  /// are removed from the container, leaving it with a size of 0
  void clear() noexcept { map_.clear(); }

  /// \brief Returns an iterator pointing to the first element in the MetaInfoMap
  iterator begin() noexcept { return map_.begin(); }
  const_iterator begin() const noexcept { return map_.begin(); }

  /// \brief Returns an iterator pointing to the past-the-end element in the MetaInfoMap
  iterator end() noexcept { return map_.end(); }
  const_iterator end() const noexcept { return map_.end(); }

  /// \brief Swap with other
  void swap(MetaInfoMap& other) noexcept { map_.swap(other.map_); }

  /// \brief Test for equality
  bool operator==(const MetaInfoMap& right) const noexcept { return (map_ == right.map_); }

  /// \brief Test for inequality
  bool operator!=(const MetaInfoMap& right) const noexcept { return (!(*this == right)); }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const MetaInfoMap& s);

private:
  map_type map_;
};

} // namespace serialbox

#endif
