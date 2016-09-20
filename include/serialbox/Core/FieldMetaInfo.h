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

#include "serialbox/Core/MetaInfoMap.h"
#include "serialbox/Core/Json.h"

namespace serialbox {

/// \brief Meta-infomration of a data field
class FieldMetaInfo {
public:
  /// \brief Default constructor
  FieldMetaInfo() : type_(TypeID::Invalid), dims_(), metaInfo_() {}

  /// \brief Construct members externally
  FieldMetaInfo(TypeID type, const std::vector<int>& dims, const MetaInfoMap& metaInfo)
      : type_(type), dims_(dims), metaInfo_(metaInfo) {}

  /// \brief Construct from JSON
  explicit FieldMetaInfo(const json::json& jsonNode) { fromJSON(jsonNode); }

  /// \brief Copy constructor
  FieldMetaInfo(const FieldMetaInfo&) = default;

  /// \brief Move constructor
  FieldMetaInfo(FieldMetaInfo&&) = default;

  /// \brief Copy assignment
  FieldMetaInfo& operator=(const FieldMetaInfo&) = default;

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
  MetaInfoMap& metaInfo() noexcept { return metaInfo_; }  
  const MetaInfoMap& metaInfo() const noexcept { return metaInfo_; }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);
  
  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const FieldMetaInfo& f);

private:
  TypeID type_;
  std::vector<int> dims_;
  MetaInfoMap metaInfo_;
};

} // namespace serialbox

#endif
