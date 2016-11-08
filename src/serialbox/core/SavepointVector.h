//===-- serialbox/core/SavepointVector.h --------------------------------------------*- C++ -*-===//
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

#include "serialbox/core/FieldID.h"
#include "serialbox/core/Json.h"
#include "serialbox/core/SavepointImpl.h"
#include <iosfwd>
#include <unordered_map>
#include <vector>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief The SavepointVector manages the registered savepoints and their mapping to the stored
/// fields
///
/// The savepoints are ordered in the sequence they were registred.
class SavepointVector {
  using index_type = std::unordered_map<SavepointImpl, int>;

public:
  /// \brief Vector of savepoints
  using savepoint_vector_type = std::vector<std::shared_ptr<SavepointImpl>>;

  /// \brief Map of fields per savepoint
  using fields_per_savepoint_type = std::unordered_map<std::string, unsigned int>;

  /// \brief Vector of map of fields per savepoint
  using fields_per_savepoint_vector_type = std::vector<fields_per_savepoint_type>;

  /// \brief A random access iterator to `std::shared_ptr<Savepoint>`
  using iterator = savepoint_vector_type::iterator;

  /// \brief A random access iterator to `const std::shared_ptr<Savepoint>`
  using const_iterator = savepoint_vector_type::const_iterator;

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
  ///
  /// \return True iff the savepoint exists
  bool exists(const SavepointImpl& savepoint) const noexcept;

  /// \brief Find savepoint
  ///
  /// \return Index of `savepoint` in the savepoint-vector or -1 if savepoint does not exist
  int find(const SavepointImpl& savepoint) const noexcept;

  /// \brief Insert savepoint in savepoint vector
  ///
  /// \return Index of the newly inserted savepoint or -1 if savepoint already exists
  int insert(const SavepointImpl& savepoint) noexcept;

  /// \brief Add a field to the savepoint
  ///
  /// \return True iff the field was successfully addeed to the savepoint
  bool addField(const SavepointImpl& savepoint, const FieldID& fieldID) noexcept;

  /// \brief Add a field to the savepoint given a valid savepoint index `idx`
  ///
  /// \return True iff the field was successfully addeed to the savepoint
  bool addField(int idx, const FieldID& fieldID) noexcept;

  /// \brief Check if savepoint has field `field`
  ///
  /// \return True iff the field `field` exists at savepoint
  bool hasField(const SavepointImpl& savepoint, const std::string& field) noexcept;

  /// \brief Check if savepoint has field `field` given a valid savepoint index `idx`
  ///
  /// \return True iff the field `field` exists at savepoint
  bool hasField(int idx, const std::string& field) noexcept;

  /// \brief Get the FielID of field `field` at savepoint `savepoint`
  ///
  /// \throw Exception  Savepoint or field at savepoint do not exist
  FieldID getFieldID(const SavepointImpl& savepoint, const std::string& field) const;

  /// \brief Get the FielID of field `field` given a valid savepoint index `idx`
  ///
  /// \throw Exception  Savepoint or field at savepoint do not exist
  FieldID getFieldID(int idx, const std::string& field) const;

  /// \brief Access fields of savepoint
  ///
  /// \throw Exception  Savepoint does not exists
  const fields_per_savepoint_type& fieldsOf(const SavepointImpl& savepoint) const;

  /// \brief Access fields of savepoint given a valid savepoint index `idx`
  const fields_per_savepoint_type& fieldsOf(int idx) const noexcept;

  /// \brief Returns a bool value indicating whether the savepoint vector is empty
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

  /// \brief Get savepoint
  SavepointImpl& operator[](int idx) noexcept { return *savepoints_[idx]; }
  const SavepointImpl& operator[](int idx) const noexcept { return *savepoints_[idx]; }

  /// \brief Returns a reference to the last element in the savepoint vector
  std::shared_ptr<SavepointImpl>& back() noexcept { return savepoints_.back(); }
  const std::shared_ptr<SavepointImpl>& back() const noexcept { return savepoints_.back(); }

  /// \brief Access the savepoints
  const savepoint_vector_type& savepoints() const noexcept { return savepoints_; }
  savepoint_vector_type& savepoints() noexcept { return savepoints_; }

  /// \brief Convert to JSON
  json::json toJSON() const;

  /// \brief Construct from JSON node
  ///
  /// \throw Exception  JSON node is ill-formed
  void fromJSON(const json::json& jsonNode);

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointVector& s);

private:
  index_type index_;                        ///< Hash-map for fast lookup
  savepoint_vector_type savepoints_;        ///< Vector of stored savepoints
  fields_per_savepoint_vector_type fields_; ///< Fields of each savepoint
};

/// @}

} // namespace serialbox

#endif
