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

#ifndef SERIALBOX_CORE_FIELDMAPINFO_H
#define SERIALBOX_CORE_FIELDMAPINFO_H

#include "serialbox/Core/MetaInfoMap.h"
#include <unordered_map>

namespace serialbox {

/// \brief Meta-infomration of a data field
class FieldMetaInfo {
public:
  /// \brief Construct from JSON
  FieldMetaInfo(const json::json& jsonNode);
  
  /// \brief Copy constructor
  FieldMetaInfo(const FieldMetaInfo&) = default;

  /// \brief Move constructor
  FieldMetaInfo(FieldMetaInfo&&) = default;

  /// \brief Copy assignment
  FieldMetaInfo& operator=(const FieldMetaInfo&) = default;

  /// \brief Move assignment
  FieldMetaInfo& operator=(FieldMetaInfo&&) = default;

  /// \brief Swap with other
  void swap(FieldMetaInfo& other) noexcept {
    std::swap(type_, other.type_);
    dims_.swap(other.dims_);
    metaInfo_.swap(other.metaInfo_);
  }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);
  
private:
  TypeID type_;
  std::vector<int> dims_;
  MetaInfoMap metaInfo_;
};

} // namespace serialbox

#endif
