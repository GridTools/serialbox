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

#ifndef SERIALBOX_CORE_SAVEPOINTVECTOR_H
#define SERIALBOX_CORE_SAVEPOINTVECTOR_H

#include "serialbox/Core/FieldID.h"
#include "serialbox/Core/Json.h"
#include "serialbox/Core/Savepoint.h"
#include <iosfwd>
#include <unordered_map>
#include <vector>

namespace serialbox {

/// \brief The SavepointVector manages the registered savepoints and their mapping to the stored
/// fields
///
/// The savepoints are ordered in the sequence they were registred.
class SavepointVector {
  using index_type = std::unordered_map<Savepoint, int>;

public:
  /// \brief Map of fields per savepoint
  using FieldsPerSavepointMap = std::unordered_map<std::string, unsigned int>;

  /// \brief A random access iterator to `Savepoint`
  using iterator = std::vector<Savepoint>::iterator;

  /// \brief A random access iterator to `const Savepoint`
  using const_iterator = std::vector<Savepoint>::const_iterator;

  /// \brief Default constructor (empty)
  SavepointVector() : index_(), savepoints_(), fields_(){};

  /// \brief Copy constructor
  SavepointVector(const SavepointVector&) = default;

  /// \brief Move constructor
  SavepointVector(SavepointVector&&) = default;

  /// \brief Construct from JSON
  explicit SavepointVector(const json::json& jsonNode) { fromJSON(jsonNode); }

  /// \brief Copy assignment
  SavepointVector& operator=(const SavepointVector&) = default;

  /// \brief Move assignment
  SavepointVector& operator=(SavepointVector&&) = default;

  /// \brief Check if savepoint exists
  /// \return True iff the savepoint exists
  bool exists(const Savepoint& savepoint) const noexcept;

  /// \brief Find savepoint
  /// \return Return index to savepoint in savepoint-vector or -1 if savepoint does not exist
  int find(const Savepoint& savepoint) const noexcept;

  /// \brief Insert savepoint in savepoint vector
  /// \return True iff the savepoint was successfully inserted
  bool insert(const Savepoint& savepoint) noexcept;

  /// \brief Add a field to the savepoint
  /// \return True iff the field was successfully addeed to the savepoint
  bool addField(const Savepoint& savepoint, const FieldID& fieldID) noexcept;

  /// \brief Add a field to the savepoint given a valid savepoint index ´idx´
  /// \return True iff the field was successfully addeed to the savepoint
  bool addField(int idx, const FieldID& fieldID) noexcept;

  /// \brief Get the FielID of field ´field´ at savepoint ´savepoint´
  ///
  /// \throw Exception  Savepoint or field at savepoint do not exist
  FieldID getFieldID(const Savepoint& savepoint, const std::string& field) const;

  /// \brief Get the FielID of field ´field´ given a valid savepoint index ´idx´
  ///
  /// \throw Exception  Savepoint or field at savepoint do not exist
  FieldID getFieldID(int idx, const std::string& field) const;

  /// \brief Access fields of savepoint
  ///
  /// \throw Exception  Savepoint does not exists
  const FieldsPerSavepointMap& fieldsOf(const Savepoint& savepoint) const;

  /// \brief Access fields of savepoint
  const FieldsPerSavepointMap& fieldsOf(int idx) const noexcept;

  /// \brief Returns a bool value indicating whether the savepoint is empty
  bool empty() const noexcept { return index_.empty(); }

  /// \brief Returns the number of savepoints in the vector
  std::size_t size() const noexcept { return savepoints_.size(); }

  /// \brief All the elements Savepoints are dropped: their destructors are called, and they
  /// are removed from the container, leaving it with a size of 0
  void clear() noexcept;

  /// \brief Swap with other
  void swap(SavepointVector& other) noexcept;

  /// \brief Returns an iterator pointing to the first savepoint in the vector
  iterator begin() noexcept { return savepoints_.begin(); }
  const_iterator begin() const noexcept { return savepoints_.begin(); }

  /// \brief Returns an iterator pointing to the past-the-end savepoint in the vector
  iterator end() noexcept { return savepoints_.end(); }
  const_iterator end() const noexcept { return savepoints_.end(); }

  /// \brief Access the savepoints
  const std::vector<Savepoint>& savepoints() const noexcept { return savepoints_; }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointVector& s);

private:
  std::unordered_map<Savepoint, int> index_;  ///< Hash-map for fast lookup
  std::vector<Savepoint> savepoints_;         ///< Vector of stored savepoints
  std::vector<FieldsPerSavepointMap> fields_; ///< Fields of each savepoint
};

} // namespace serialbox

#endif
