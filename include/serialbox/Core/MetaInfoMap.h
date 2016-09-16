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
#include "serialbox/Core/Logging.h"
#include "serialbox/Core/Type.h"
#include "serialbox/Core/Unreachable.h"
#include <boost/any.hpp>
#include <iosfwd>
#include <string>
#include <unordered_map>

namespace serialbox {

/// \brief A map of meta-information
///
/// The objects are stored in a hash-map (std::unordered_map) in form of key = value pair. 
/// They keys are strings (std::string), while the values can be booleans, integers (32 and 64 bit),
/// floating point numbers (32 and 64 bit) or strings.
class MetaInfoMap {
public:
  /// \brief Constructor
  /// @{
  MetaInfoMap(const json::json jsonNode) { fromJSON(jsonNode); }
  MetaInfoMap() = default;
  MetaInfoMap(const MetaInfoMap&) = default;
  MetaInfoMap(MetaInfoMap&&) = default;

  MetaInfoMap& operator=(const MetaInfoMap&) = default;
  MetaInfoMap& operator=(MetaInfoMap&&) = default;
  /// @}

  /// \brief Represent a value of the MetaInfoMap given as a type-id and type-erased data
  struct Value {
    /// \name Constructors
    /// @{

    Value() = default;
    Value(const Value&) = default;
    Value(Value&&) = default;

    /// \brief Construct with given value
    ///
    /// \tparam ValueType  Type of the captured value (needs to be supported)
    /// \param  value      Value to caputre
    template <class ValueType,
              class DecayedValueType = typename std::decay<ValueType>::type,
              class = typename std::enable_if<isSupported<DecayedValueType>::value>::type>
    explicit Value(ValueType&& value) {
      type_ = ToTypeID<DecayedValueType>::value;
      any_ = boost::any(DecayedValueType(value));
    }

    Value& operator=(const Value&) = default;
    Value& operator=(Value&&) = default;
    /// @}

    /// \brief Convert value to type T
    /// 
    /// \throws Exception  TypeID of type T does not match TypeID of the captured value
    template <class T>
    T& as() {
      if(ToTypeID<T>::value != type_)
        throw Exception("conversion error: cannot convert [type = %s] to [T = %s]",
                        TypeUtil::toString(type_), TypeUtil::toString(ToTypeID<T>::value));
      return (*boost::any_cast<T>(&any_));
    }

    template <class T>
    const T& as() const {
      if(ToTypeID<T>::value != type_)
        throw Exception("conversion error: cannot convert [type = %s] to [T = %s]",
                        TypeUtil::toString(type_), TypeUtil::toString(ToTypeID<T>::value));
      return (*boost::any_cast<T>(&any_));
    }

    /// \brief Implicitly convert value to type T
    /// 
    /// \throws Exception  TypeID of type T does not match TypeID of the captured value
    template <class T>
    operator T() const {
      return as<T>();
    }
    
    template <class T>
    operator T() {
      return as<T>();
    }

    /// \brief Swap with other
    void swap(Value& other) noexcept {
      any_.swap(other.any_);
      std::swap(type_, other.type_);
    }

    /// \brief Test for equality
    bool operator==(const Value& right) const noexcept;

    /// \brief Test for inequality
    bool operator!=(const Value& right) const noexcept { return (!(*this == right)); }

    /// \brief Get TypeID
    TypeID type() const noexcept { return type_; }

    /// \brief Get boost::any
    boost::any any() const noexcept { return any_; }

  private:
    TypeID type_;    ///< Type of the data
    boost::any any_; ///< Type-erased value of the data
  };

  using map_type = std::unordered_map<std::string, Value>;
  using value_type = map_type::value_type;
  using key_type = map_type::key_type;
  using size_type = map_type::size_type;
  using mapped_type = map_type::mapped_type;
  using iterator = map_type::iterator;
  using const_iterator = map_type::const_iterator;

  /// \brief Check if key exists in the set
  ///
  /// \param key  Key to be searched for
  /// \return True iff the key is present
  template <class StringType>
  bool hasKey(StringType&& key) const noexcept {
    return (map_.find(key) != map_.end());
  }

  /// \brief Inserts a new element in the map
  ///
  /// The element is inserted only if its key is not equivalent to the key of any other element
  /// already in the set (keys in a MetaInfoMap are unique).
  ///
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  ///
  /// \return Value indicating whether the element was successfully inserted or not
  template <class KeyType, class ValueType>
  bool insert(KeyType&& key, ValueType&& value) noexcept {
    return (map_.insert({key, Value(std::forward<ValueType>(value))}).second);
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

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const MetaInfoMap& s);

  /// \name Serialization
  /// @{

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  /// 
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

  /// @}

private:
  map_type map_;
};

} // namespace serialbox

#endif
