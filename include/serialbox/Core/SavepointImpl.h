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
/// This file contains the shared implementation of all Savepoints.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SAVEPOINTIMPL_H
#define SERIALBOX_CORE_SAVEPOINTIMPL_H

#include "serialbox/Core/FieldID.h"
#include "serialbox/Core/Json.h"
#include "serialbox/Core/MetaInfoMap.h"
#include <iosfwd>
#include <string>
#include <vector>

namespace serialbox {

/// \brief Shared implementation of the Savepoints
///
/// Direct usage of this class is discouraged, use the Savepoint classes provided by the Frontends
/// instead.
class SavepointImpl {
public:
  /// \brief Construct an empty savepoint (i.e this->empty() == true)
  template <class StringType,
            class = typename std::enable_if<!std::is_same<StringType, json::json>::value>::type>
  explicit SavepointImpl(const StringType& name) : name_(name), metaInfo_(), fields_(){};

  /// \brief Construct a field-less savepoint with ´name´ and ´metaInfo´
  template <class StringType, class MetaInfoType>
  SavepointImpl(StringType&& name, MetaInfoType&& metaInfo)
      : name_(name), metaInfo_(metaInfo), fields_(){};

  /// \brief Copy constructor [deleted]
  SavepointImpl(const SavepointImpl&) = delete;

  /// \brief Move constructor
  SavepointImpl(SavepointImpl&&) = default;

  /// \brief Construct from JSON
  explicit SavepointImpl(const json::json& jsonNode) { fromJSON(jsonNode); }

  /// \brief Construct members externally
  SavepointImpl(const std::string name, const MetaInfoMap& metaInfo,
                const std::vector<FieldID>& fields)
      : name_(name), metaInfo_(metaInfo), fields_(fields) {}

  /// \brief Copy assignment [deleted]
  SavepointImpl& operator=(const SavepointImpl&) = delete;

  /// \brief Move assignment
  SavepointImpl& operator=(SavepointImpl&&) = default;

  /// \brief Test for equality
  bool operator==(const SavepointImpl& right) const {
    return (name_ == right.name_) && (metaInfo_ == right.metaInfo_) && (fields_ == right.fields_);
  }

  /// \brief Test for inequality
  bool operator!=(const SavepointImpl& right) const { return (!(*this == right)); }

  /// \brief Swap with other
  void swap(SavepointImpl& other) noexcept;

  /// \brief Access name
  std::string& name() noexcept { return name_; }
  const std::string& name() const noexcept { return name_; }

  /// \brief Access meta-info
  MetaInfoMap& metaInfo() noexcept { return metaInfo_; }
  const MetaInfoMap& metaInfo() const noexcept { return metaInfo_; }

  /// \brief Access fields
  std::vector<FieldID>& fields() noexcept { return fields_; }
  const std::vector<FieldID>& fields() const noexcept { return fields_; }

  /// \brief Register field within savepoint
  ///
  /// This function provides strong exception safety.
  ///
  /// \param name       Name of the newly registered field
  /// \throw Exception  Field with given name already exists
  void registerField(FieldID fieldID);

  /// \brief Return number of registered fields
  std::size_t numFields() const noexcept { return fields_.size(); }

  /// \brief Check if field exists
  ///
  /// \param name  Name of the field to check
  inline bool hasField(const std::string& name) const noexcept { return hasFieldImpl(name); }
  inline bool hasField(const std::string& name) noexcept { return hasFieldImpl(name); }

  /// \brief Get field ID of field given by name
  ///
  /// \param name       Name of the field newly registered
  /// \throw Exception  Field with given name does not exist
  const FieldID& getFieldID(const std::string& name) const;

  /// \brief Returns a bool value indicating whether the savepoint is empty
  bool empty() const noexcept { return metaInfo_.empty() && fields_.empty(); }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s);

protected:
  inline bool hasFieldImpl(const std::string& name) const noexcept {
    for(const auto& field : fields_)
      if(field.name == name)
        return true;
    return false;
  }

protected:
  std::string name_;            ///< Name of this savepoint
  MetaInfoMap metaInfo_;        ///< Meta-information of this savepoint
  std::vector<FieldID> fields_; ///< Fields captured by this savepoint
};

} // namespace serialbox

#endif
