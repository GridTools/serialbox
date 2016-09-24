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
    fields_.push_back(FieldsPerSavepointMap());
    return true;
  }
  return false;
}

bool SavepointVector::addField(const Savepoint& savepoint, const FieldID& fieldID) noexcept {
  int idx = find(savepoint);
  if(idx != -1)
    return addField(idx, fieldID);
  return false;
}

bool SavepointVector::addField(int idx, const FieldID& fieldID) noexcept {
  return fields_[idx].insert({fieldID.name, fieldID.id}).second;
}

FieldID SavepointVector::getFieldID(int idx, const std::string& field) const {
  auto it = fields_[idx].find(field);
  if(it != fields_[idx].end())
    return FieldID{it->first, it->second};
  throw Exception("field '%s' does not exists at savepoint '%s'", field, savepoints_[idx].name());
}

FieldID SavepointVector::getFieldID(const Savepoint& savepoint, const std::string& field) const {
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

bool SavepointVector::exists(const Savepoint& savepoint) const noexcept {
  return (find(savepoint) != -1);
}

int SavepointVector::find(const Savepoint& savepoint) const noexcept {
  auto it = index_.find(savepoint);
  return ((it != index_.end()) ? it->second : -1);
}

const SavepointVector::FieldsPerSavepointMap& SavepointVector::fieldsOf(int idx) const noexcept {
 return fields_[idx];
}

const SavepointVector::FieldsPerSavepointMap&
SavepointVector::fieldsOf(const Savepoint& savepoint) const {
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

json::json SavepointVector::toJSON() const {
  json::json jsonNode;
  CHECK(savepoints_.size() == fields_.size());

  for(std::size_t i = 0; i < savepoints_.size(); ++i)
    jsonNode["savepoints"].push_back(savepoints_[i].toJSON());

  for(std::size_t i = 0; i < fields_.size(); ++i) {
    const std::string& savepoint = savepoints_[i].name();
    json::json fieldNode;

    if(fields_[i].empty())
      fieldNode[savepoint] = nullptr;

    for(auto it = fields_[i].begin(), end = fields_[i].end(); it != end; ++it)
      fieldNode[savepoint][it->first] = it->second;

    jsonNode["fields_per_savepoint"].push_back(fieldNode);
  }

  return jsonNode;
}

void SavepointVector::fromJSON(const json::json& jsonNode) {
  index_.clear();
  savepoints_.clear();
  fields_.clear();

  if(jsonNode.is_null() || jsonNode.empty())
    return;

  // Add savepoints
  if(jsonNode.count("savepoints")) {
    for(auto it = jsonNode["savepoints"].begin(), end = jsonNode["savepoints"].end(); it != end;
        ++it) {
      Savepoint sp(*it);
      insert(sp);
    }
  }

  // Eeach savepoint needs an entry in the fields array (it can be null though)
  if(jsonNode.count("fields_per_savepoint") &&
     jsonNode["fields_per_savepoint"].size() != fields_.size())
    throw Exception("inconsistent number of 'fields_per_savepoint' and 'savepoints'");

  for(std::size_t i = 0; i < fields_.size(); ++i) {
    const json::json& fieldNode = jsonNode["fields_per_savepoint"][i][savepoints_[i].name()];

    // Savepoint has no fields
    if(fieldNode.is_null() || fieldNode.empty())
      break;

    // Add fields
    for(auto it = fieldNode.begin(), end = fieldNode.end(); it != end; ++it)
      fields_[i].insert({it.key(), static_cast<unsigned int>(it.value())});
  }
}

std::ostream& operator<<(std::ostream& stream, const SavepointVector& s) {
  stream << "SavepointVector = " << s.toJSON().dump(4);
  return stream;
}

} // namespace serialbox
