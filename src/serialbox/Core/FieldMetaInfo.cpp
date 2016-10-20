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

#include "serialbox/Core/FieldMetaInfo.h"
#include <algorithm>
#include <memory>

namespace serialbox {

FieldMetaInfo& FieldMetaInfo::operator=(const FieldMetaInfo& other) {
  type_ = other.type_;
  dims_ = other.dims_;
  metaInfo_ = std::make_shared<MetaInfoMap>(*other.metaInfo_);
  return (*this);
}

void FieldMetaInfo::swap(FieldMetaInfo& other) noexcept {
  std::swap(type_, other.type_);
  dims_.swap(other.dims_);
  metaInfo_->swap(*other.metaInfo_);
}

bool FieldMetaInfo::operator==(const FieldMetaInfo& right) const noexcept {
  if(type_ != right.type_)
    return false;

  if(dims_.size() != right.dims_.size() ||
     !std::equal(dims_.begin(), dims_.end(), right.dims_.begin()))
    return false;

  return (*metaInfo_ == *right.metaInfo_);
}

json::json FieldMetaInfo::toJSON() const {
  json::json jsonNode;
  jsonNode["type_id"] = static_cast<int>(type_);
  jsonNode["dims"] = dims_;
  jsonNode["meta_info"] = metaInfo_->toJSON();
  return jsonNode;
}

void FieldMetaInfo::fromJSON(const json::json& jsonNode) {
  dims_.clear();
  metaInfo_->clear();

  if(jsonNode.is_null() || jsonNode.empty())
    throw Exception("node is empty");

  if(!jsonNode.count("type_id"))
    throw Exception("no node 'type_id'");
  type_ = static_cast<TypeID>(int(jsonNode["type_id"]));

  if(!jsonNode.count("dims"))
    throw Exception("no node 'value'");
  for(auto it = jsonNode["dims"].begin(), end = jsonNode["dims"].end(); it != end; ++it)
    dims_.push_back(int(*it));

  if(!jsonNode.count("meta_info"))
    throw Exception("no node 'meta_info'");
  try {
    metaInfo_->fromJSON(jsonNode["meta_info"]);
  } catch(Exception& e) {
    throw Exception("in node 'meta_info': %s", e.what());
  }
}

std::string FieldMetaInfo::toString() const {
  std::string str = toJSON().dump(4);
  return str;
}

std::ostream& operator<<(std::ostream& stream, const FieldMetaInfo& f) {
  return (stream << f.toString());
}

} // namespace serialbox
