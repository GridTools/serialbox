//===-- serialbox/Core/Frontend/gridtools/Savepoint.h -------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the gridtools implementation of savepoint.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_SAVEPOINT_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_SAVEPOINT_H

#include "serialbox/Core/Frontend/gridtools/Exception.h"
#include "serialbox/Core/Frontend/gridtools/MetaInfoMap.h"
#include "serialbox/Core/SavepointImpl.h"
#include <memory>

namespace serialbox {

namespace gridtools {

/// \brief Savepoint implemenation of the gridtools frontend
///
/// Savepoints are primarily identified by their `name` and further distinguished by their
/// `meta_info`. Savepoints are used within the \ref gridtools::serializer "serializer" to
/// discriminate fields at different points in time.
/// 
/// \ingroup gridtools
class savepoint {
public:
  /// \brief Construct a savepoint
  ///
  /// This method prepares the savepoint for usage and gives a name, which is the only required
  /// information for the savepoint to be usable. Meta-information can be added after the
  /// initialization has been performed.
  ///
  /// \param name    Name of the savepoint
  explicit savepoint(const std::string& name)
      : savepointImpl_(std::make_shared<SavepointImpl>(name)){};

  /// \brief Construct savepoint with `name` and `metaInfo`
  ///
  /// \param name        Name of the savepoint
  /// \param meta_info   Arguments forwarded to the constructor of meta_info_map
  savepoint(const std::string& name, meta_info_map meta_info)
      : savepointImpl_(std::make_shared<SavepointImpl>(name, *meta_info.impl())){};
  
  /// \brief Copy constructor
  ///
  /// This performs a \i shallow copy, meaning the objects share the same underlying SavepointImpl.
  /// To deep copy the object call meta_info_map::clone().
  ///
  /// \b Example
  /// \code
  ///   savepoint s1("s1", {{"key1", meta_info_value(4.0)}});
  ///   savepoint s2(s1);
  ///
  ///   s1.meta_info().clear();
  ///   assert(s2.meta_info().empty()); // s1 and s2 share the same SavepointImpl, hence the
  ///                                   // meta_info_map of s2 was cleared as well
  /// \endcode
  ///
  /// \see meta_info_map::clone()
  savepoint(const savepoint& other) = default;

  /// \brief Move constructor
  savepoint(savepoint&&) = default;

  /// \brief Copy assignment
  ///
  /// This performs a \i shallow copy, meaning the objects share the same underlying SavepointImpl.
  /// To deep copy the object call meta_info_map::clone().
  ///
  /// \b Example
  /// \code
  ///   savepoint s1("s1", {{"key1", meta_info_value(4.0)}});
  ///   savepoint s2 = s1;
  ///
  ///   s1.meta_info().clear();
  ///   assert(s2.meta_info().empty()); // s1 and s2 share the same SavepointImpl, hence the
  ///                                   // meta_info_map of s2 was cleared as well
  /// \endcode
  ///
  /// \see meta_info_map::clone()
  savepoint& operator=(const savepoint& other) = default;

  /// \brief Move assignment
  savepoint& operator=(savepoint&&) = default;

  /// \brief Clone the current savepoint object by performing a deep copy
  ///
  /// \b Example
  /// \code
  ///   savepoint s1("s1", {{"key1", meta_info_value(4.0)}});
  ///   savepoint s2 = s1.clone();
  ///
  ///   s1.meta_info().clear();
  ///   assert(!s2.meta_info().empty()); // s1 and s2 do NOT share the same SavepointImpl, hence the
  ///                                    // meta_info_map of s2 was NOT cleared
  /// \endcode
  savepoint clone() const { return savepoint(std::make_shared<SavepointImpl>(*savepointImpl_)); }

  /// \brief Construct with SavepointImpl (internal use)
  explicit savepoint(const std::shared_ptr<SavepointImpl>& savepoint_impl) {
    savepointImpl_ = savepoint_impl;
  }

  /// \brief Add a new `key = value` or `key = {value1, ..., valueN}` pair to the meta-information
  /// of the savepoint
  ///
  /// \param key    Key of the new element
  /// \param value  Object to be copied to (or moved as) the value of the new element
  ///
  /// \throw exception  Value cannot be inserted as it already exists
  template <class StringType, class ValueType>
  void add_meta_info(StringType&& key, ValueType&& value) {
    savepointImpl_->addMetaInfo(std::forward<StringType>(key), std::forward<ValueType>(value));
  }

  /// \brief Test for equality
  bool operator==(const savepoint& right) const {
    return (*savepointImpl_ == *right.savepointImpl_);
  }

  /// \brief Test for inequality
  bool operator!=(const savepoint& right) const { return (!(*this == right)); }

  /// \brief Swap with other
  void swap(savepoint& other) noexcept { savepointImpl_->swap(*other.savepointImpl_); }

  /// \brief Access name
  const std::string& name() const noexcept { return savepointImpl_->name(); }

  /// \brief Access meta-info
  meta_info_map meta_info() const noexcept { return meta_info_map(savepointImpl_->metaInfoPtr()); }

  /// \brief Returns a bool value indicating whether the savepoint is empty (i.e has no
  /// meta-information attached)
  bool empty() const noexcept { return savepointImpl_->empty(); }

  /// \brief Convert savepoint to string
  std::string to_string() const { return savepointImpl_->toString(); };

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const savepoint& s) {
    return (stream << *s.savepointImpl_);
  }

  /// \brief Get implementation pointer
  const std::shared_ptr<SavepointImpl>& impl() const { return savepointImpl_; }

private:
  std::shared_ptr<SavepointImpl> savepointImpl_;
};

} // namespace gridtools

} // namespace serialbox

#endif
