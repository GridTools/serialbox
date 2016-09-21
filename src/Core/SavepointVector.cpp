//===-- serialbox/Core/SavepointVector.h --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file provides the SavepointVector which manages the registered savepoints and their mapping
/// to the stored fields.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Logging.h"
#include "serialbox/Core/SavepointVector.h"

namespace serialbox {

bool SavepointVector::insert(const Savepoint& savepoint) noexcept {
  int idx = savepoints_.size();
  if(index_.insert(typename index_type::value_type{savepoint, idx}).second) {
    savepoints_.push_back(savepoint);
    fields_.push_back(std::vector<FieldID>());
    return true;
  }
  return false;
}

bool SavepointVector::addField(const Savepoint& savepoint, const FieldID& fieldID) noexcept {
  auto it = find(savepoint);
  if(it != end())
    return addField(it, fieldID);
  return false;
}

bool SavepointVector::addField(const iterator& savepointIterator, const FieldID& fieldID) noexcept {
  int idx = savepointIterator - savepoints_.begin();
  auto& fields = fields_[idx];

  if(internal::fieldExists(fields, fieldID.name) != -1)
    return false;

  fields.push_back(fieldID);
  return true;
}

FieldID SavepointVector::getFieldID(const Savepoint& savepoint, const std::string& field) const {
  auto it = find(savepoint);
  if(it != end())
    return getFieldID(it, field);
  throw Exception("savepoint '%' does not exist", savepoint.toString());
}

void SavepointVector::swap(SavepointVector& other) noexcept {
  index_.swap(other.index_);
  savepoints_.swap(other.savepoints_);
  fields_.swap(other.fields_);
}

bool SavepointVector::exists(const Savepoint& savepoint) const noexcept {
  return (index_.find(savepoint) != index_.end());
}

SavepointVector::iterator SavepointVector::find(const Savepoint& savepoint) noexcept {
  auto it = index_.find(savepoint);
  if(it != index_.end())
    return (savepoints_.begin() + it->second);
  return savepoints_.end();
}

SavepointVector::const_iterator SavepointVector::find(const Savepoint& savepoint) const noexcept {
  auto it = index_.find(savepoint);
  if(it != index_.end())
    return (savepoints_.begin() + it->second);
  return savepoints_.end();
}

const std::vector<FieldID>& SavepointVector::fieldsOf(const Savepoint& savepoint) const {
  auto it = index_.find(savepoint);
  if(it != index_.end())
    return fields_[it->second];
  throw Exception("savepoint '%' does not exist", savepoint.toString());
}

json::json SavepointVector::toJSON() const {
  json::json jsonNode;
  return jsonNode;
}

void SavepointVector::fromJSON(const json::json& jsonNode) {
  index_.clear();
  savepoints_.clear();
  fields_.clear();
}

std::ostream& operator<<(std::ostream& stream, const SavepointVector& s) { return stream; }

} // namespace serialbox
