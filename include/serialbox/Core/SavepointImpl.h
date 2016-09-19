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

namespace serialbox {

/// \brief Shared implementation of the Savepoints
///
/// Direct usage of this class is discouraged, use the Savepoint classes provided by the Frontends
/// instead.
class SavepointImpl {
public:
  /// \brief Copy constructor [deleted]
  SavepointImpl(const SavepointImpl&) = delete;

  /// \brief Move constructor
  SavepointImpl(SavepointImpl&&) = default;

  /// \brief Construct members externally
  SavepointImpl(const std::string name, const MetaInfoMap& metaInfo,
                const std::vector<FieldID>& fields)
      : name_(name), metaInfo_(metaInfo), fields_(fields) {}

  /// \brief Copy assignment [deleted]
  SavepointImpl& operator=(const SavepointImpl&) = delete;

  /// \brief Move assignment
  SavepointImpl& operator=(SavepointImpl&&) = default;

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
  /// \param name       Name of the newly registered field
  /// \throw Exception  Field with given name already exists
  void registerField(FieldID fieldID);

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

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s);

private:
  inline bool hasFieldImpl(const std::string& name) const noexcept {
    for(const auto& field : fields_)
      if(field.name == name)
        return true;
    return false;
  }

private:
  std::string name_;            ///< Name of this savepoint
  MetaInfoMap metaInfo_;        ///< Meta-information of this savepoint
  std::vector<FieldID> fields_; ///< Fields captured by this savepoint
};

} // namespace serialbox

#endif
