//===-- serialbox/core/SavepointVector.cpp ------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/SavepointVector.h"
#include "serialbox/core/Logging.h"
#include "serialbox/core/SavepointVectorSerializer.h"

namespace serialbox {

int SavepointVector::insert(const SavepointImpl& savepoint) noexcept {
  int idx = savepoints_.size();
  if(index_.insert(typename index_type::value_type{savepoint, idx}).second) {
    savepoints_.push_back(std::make_shared<SavepointImpl>(savepoint));
    fields_.push_back(fields_per_savepoint_type());
    return idx;
  }
  return -1;
}

bool SavepointVector::addField(const SavepointImpl& savepoint, const FieldID& fieldID) noexcept {
  int idx = find(savepoint);
  if(idx != -1)
    return addField(idx, fieldID);
  return false;
}

bool SavepointVector::addField(int idx, const FieldID& fieldID) noexcept {
  return fields_[idx].insert({fieldID.name, fieldID.id}).second;
}

bool SavepointVector::hasField(const SavepointImpl& savepoint, const std::string& field) noexcept {
  int idx = find(savepoint);
  if(idx != -1)
    return hasField(idx, field);
  return false;
}

bool SavepointVector::hasField(int idx, const std::string& field) noexcept {
  return (fields_[idx].find(field) != fields_[idx].end());
}

FieldID SavepointVector::getFieldID(int idx, const std::string& field) const {
  auto it = fields_[idx].find(field);
  if(it != fields_[idx].end())
    return FieldID{it->first, it->second};
  throw Exception("field '%s' does not exists at savepoint '%s'", field, savepoints_[idx]->name());
}

FieldID SavepointVector::getFieldID(const SavepointImpl& savepoint,
                                    const std::string& field) const {
  int idx = find(savepoint);
  if(idx != -1)
    return getFieldID(idx, field);
  throw Exception("savepoint '%' does not exist", savepoint.toString());
}

void SavepointVector::swap(SavepointVector& other) noexcept {
  index_.swap(other.index_);
  savepoints_.swap(other.savepoints_);
  fields_.swap(other.fields_);
}

bool SavepointVector::exists(const SavepointImpl& savepoint) const noexcept {
  return (find(savepoint) != -1);
}

int SavepointVector::find(const SavepointImpl& savepoint) const noexcept {
  auto it = index_.find(savepoint);
  return ((it != index_.end()) ? it->second : -1);
}

const SavepointVector::fields_per_savepoint_type& SavepointVector::fieldsOf(int idx) const
    noexcept {
  return fields_[idx];
}

const SavepointVector::fields_per_savepoint_type&
SavepointVector::fieldsOf(const SavepointImpl& savepoint) const {
  auto it = index_.find(savepoint);
  if(it != index_.end())
    return fieldsOf(it->second);
  throw Exception("savepoint '%' does not exist", savepoint.toString());
}

void SavepointVector::clear() noexcept {
  savepoints_.clear();
  index_.clear();
  fields_.clear();
}

std::ostream& operator<<(std::ostream& stream, const SavepointVector& s) {
  stream << "SavepointVector = " << json::json{s}.dump(4);
  return stream;
}

} // namespace serialbox
