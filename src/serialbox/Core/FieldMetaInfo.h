//===-- serialbox/Core/FieldMetaInfo.h ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the meta information of a field.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FIELDMAPMETAINFO_H
#define SERIALBOX_CORE_FIELDMAPMETAINFO_H

#include "serialbox/Core/Json.h"
#include "serialbox/Core/MetaInfoMap.h"
#include <memory>

namespace serialbox {

/// \brief Meta-information of a data field
class FieldMetaInfo {
public:
  /// \brief Default constructor
  FieldMetaInfo() : type_(TypeID::Invalid), dims_(), metaInfo_(std::make_shared<MetaInfoMap>()) {}

  /// \brief Construct members externally
  FieldMetaInfo(TypeID type, const std::vector<int>& dims, const MetaInfoMap& metaInfo)
      : type_(type), dims_(dims), metaInfo_(std::make_shared<MetaInfoMap>(metaInfo)) {}

  /// \brief Construct members externally
  FieldMetaInfo(TypeID type, const std::vector<int>& dims)
      : type_(type), dims_(dims), metaInfo_(std::make_shared<MetaInfoMap>()) {}

  /// \brief Construct from JSON
  explicit FieldMetaInfo(const json::json& jsonNode) : FieldMetaInfo() { fromJSON(jsonNode); }

  /// \brief Copy constructor
  FieldMetaInfo(const FieldMetaInfo& other) { *this = other; }

  /// \brief Move constructor
  FieldMetaInfo(FieldMetaInfo&&) = default;

  /// \brief Copy assignment
  FieldMetaInfo& operator=(const FieldMetaInfo& other);

  /// \brief Move assignment
  FieldMetaInfo& operator=(FieldMetaInfo&&) = default;

  /// \brief Swap with other
  void swap(FieldMetaInfo& other) noexcept;

  /// \brief Test for equality
  bool operator==(const FieldMetaInfo& right) const noexcept;

  /// \brief Test for inequality
  bool operator!=(const FieldMetaInfo& right) const noexcept { return (!(*this == right)); }

  /// \brief Access TypeID
  TypeID& type() noexcept { return type_; }
  const TypeID& type() const noexcept { return type_; }

  /// \brief Access dimensions
  std::vector<int>& dims() noexcept { return dims_; }
  const std::vector<int>& dims() const noexcept { return dims_; }

  /// \brief Access meta-info map
  MetaInfoMap& metaInfo() noexcept { return *metaInfo_; }
  const MetaInfoMap& metaInfo() const noexcept { return *metaInfo_; }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

  /// \brief Convert to string
  std::string toString() const;
  
  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const FieldMetaInfo& f);

  /// \brief Get meta-info pointer
  std::shared_ptr<MetaInfoMap>& metaInfoPtr() noexcept { return metaInfo_; }
  const std::shared_ptr<MetaInfoMap>& metaInfoPtr() const noexcept { return metaInfo_; }

private:
  TypeID type_;
  std::vector<int> dims_;
  std::shared_ptr<MetaInfoMap> metaInfo_;
};

} // namespace serialbox

#endif
