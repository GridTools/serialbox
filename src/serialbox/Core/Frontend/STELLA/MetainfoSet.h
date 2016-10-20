//===-- serialbox/Core/Frontend/STELLA/MetainfoSet.h --------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the meta-information set implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_METAINFOSET_H
#define SERIALBOX_CORE_FRONTEND_STELLA_METAINFOSET_H

#include "serialbox/Core/Frontend/STELLA/ForwardDecl.h"
#include <boost/any.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/type_traits.hpp>
#include <utility>
#include <vector>

namespace serialbox {

namespace stella {

/// \brief Meta-information set
///
/// Objects of this class contain a set of metainformation in form of key = value pair. The keys
/// are strings, while the values can be integers, booleans, floating point numbers (either single
/// or double precision) or strings.
///
/// \ingroup STELLA
class MetainfoSet {
public:
  /// \brief Construct empty map
  MetainfoSet();

  /// \brief Construct with MetaInfoMap
  explicit MetainfoSet(const boost::shared_ptr<MetaInfoMap>& map);

  /// \brief Copy constructor
  MetainfoSet(const MetainfoSet& other);

  /// \brief Assignment operator
  ///
  /// Discards all previously stored information and copies data from the other object, performing
  /// a deep copy.
  ///
  /// \param other  The object to copy from
  MetainfoSet& operator=(const MetainfoSet& other);

  /// Removes all metainfo and frees all memory
  void Cleanup();

  /// \brief Gives access to all existing keys
  ///
  /// The order in which the keys are returned is ensured not to change if no keys are added or
  /// removed.
  ///
  /// \return A vector containing all keys is returned
  std::vector<std::string> keys() const;

  /// \brief Check if key exists already in set
  ///
  /// \return True is returned iff the key does exist
  bool HasKey(const std::string& key) const;

  /// \brief Add a new key-value pair into the set. The key must not exist yet
  ///
  /// \param key    The key of the pair
  /// \param value  The value
  ///
  /// \throw SerializationException     The key exists already
  /// @{
  void AddMetainfo(const std::string& key, const int& value);
  void AddMetainfo(const std::string& key, const bool& value);
  void AddMetainfo(const std::string& key, const float& value);
  void AddMetainfo(const std::string& key, const double& value);
  void AddMetainfo(const std::string& key, const std::string& value);
  void AddMetainfo(const std::string& key, const char* value) {
    AddMetainfo(key, std::string(value));
  }
  /// @}

  /// \brief Gives access to the internal representation of the requested metainfo
  ///
  /// \param key  The identification of the metainfo value which is requested
  /// \return The metainfo object is returned as constant reference
  ///
  /// \throw SerializationException     The key exists already
  const boost::any& AsAny(const std::string& key) const;

  /// \brief Extracts a value in bool representation
  ///
  /// If the type is different than bool, the function does its best to convert the value to bool
  /// in a meaningful way.
  ///
  /// \param key  The identification of the metainfo value which is requested
  /// \return The value of the metainfo is returned as copy
  ///
  /// \throw SerializationException   The key does not exist
  bool AsBool(const std::string& key) const;

  /// \brief Extracts a value in int representation
  ///
  /// If the type is different than int, the function does its best to convert the value to int in a
  /// meaningful way, and throws a SerializationException if the value is not convertible to int,
  /// e.g., in case of a floating point holding a non-integer value.
  ///
  /// \param key  The identification of the metainfo value which is requested
  /// \return The value of the metainfo is returned as copy
  ///
  /// \throw SerializationException     The key does not exist or the value is not convertible to
  ///                                   integer
  int AsInt(const std::string& key) const;

  /// \brief Extracts a value in single precision floating point representation
  ///
  /// If the type is different than float, the function converts the value to float.
  ///
  /// \param key  The identification of the metainfo value which is requested
  /// \return The value of the metainfo is returned as copy
  ///
  /// \throw SerializationException   The key does not exist
  float AsFloat(const std::string& key) const;

  /// Extracts a value in double precision floating point representation
  ///
  /// If the type is different than double, the function converts the value to double.
  ///
  /// \param key  The identification of the metainfo value which is requested
  /// \return The value of the metainfo is returned as copy
  ///
  /// \throw SerializationException   The key does not exist
  double AsDouble(const std::string& key) const;

  /// \brief Extracts a value in RealType representation, where RealType is either `float` or
  /// `double`
  ///
  /// If the type is different than RealType, the function converts the value to RealType.
  ///
  /// \param key  The identification of the metainfo value which is requested
  /// \return The value of the metainfo is returned as copy
  ///
  /// \throw SerializationException   The key does not exist
  template <typename RealType>
  RealType AsReal(const std::string& key) const {
    BOOST_STATIC_ASSERT(
        (boost::is_same<RealType, float>::value || boost::is_same<RealType, double>::value));

    if(boost::is_same<RealType, float>::value)
      return AsFloat(key);
    else if(boost::is_same<RealType, double>::value)
      return AsDouble(key);
  }

  /// \brief Extracts a value assuming its type is string
  ///
  /// If the type is different than string, the function constructs a string representation of it.
  ///
  /// \param key  The identification of the metainfo value which is requested
  /// \return The value of the metainfo is returned as copy
  ///
  /// \throw SerializationException   The key does not exist
  std::string AsString(const std::string& key) const;

  /// \brief Creates a string that carries the metainfo in a human-readable form
  ///
  /// \return The string representing the set is returned
  std::string ToString() const;

  /// \brief Number elements in the set
  ///
  /// \return The size of the set is returned
  std::size_t size() const;

  /// \brief Comparison operator
  bool operator==(const MetainfoSet& other) const;

  /// \brief Set implementation pointer
  void setImpl(const boost::shared_ptr<MetaInfoMap>& metaInfoMap);

  /// \brief Get implementation pointer
  boost::shared_ptr<MetaInfoMap>& getImpl();
  const boost::shared_ptr<MetaInfoMap>& getImpl() const;

private:
  boost::shared_ptr<MetaInfoMap> mapImpl_;
};

} // namespace stella

} // namespace serialbox

#endif
