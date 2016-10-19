//===-- serialbox/Core/Frontend/gridtools/FieldMetaInfo.h ---------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the gridtools implementation of the field meta-information.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_FIELD_META_INFO_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_FIELD_META_INFO_H

#include "serialbox/Core/FieldMetaInfo.h"
#include "serialbox/Core/Frontend/gridtools/Exception.h"
#include "serialbox/Core/Frontend/gridtools/MetaInfoMap.h"
#include <memory>

namespace serialbox {

namespace gridtools {

/// \brief Meta-information of a data field
class field_meta_info {
public:
  /// \brief Default constructor
  field_meta_info() : field_meta_info_impl_(std::make_shared<FieldMetaInfo>()){};

  /// \brief Construct field meta-information
  ///
  /// Further meta-information can be added later on with field_meta_info::meta_info.
  ///
  /// \param type      Type of the field
  /// \param dims      Dimension of the field
  /// \param metaInfo  Meta-information of the field
  field_meta_info(TypeID type, const std::vector<int>& dims, const meta_info_map& meta_info)
      : field_meta_info_impl_(std::make_shared<FieldMetaInfo>(type, dims, *meta_info.impl())) {}

  /// \brief Construct field meta-information
  ///
  /// Meta-information can be added later on with field_meta_info::meta_info.
  ///
  /// \param type      Type of the field
  /// \param dims      Dimension of the field
  field_meta_info(TypeID type, const std::vector<int>& dims)
      : field_meta_info_impl_(std::make_shared<FieldMetaInfo>(type, dims)) {}

  /// \brief Construct with FieldMetaInfo (internal use)
  explicit field_meta_info(const std::shared_ptr<FieldMetaInfo>& field_meta_info_ptr) {
    field_meta_info_impl_ = field_meta_info_ptr;
  }

  /// \brief Copy constructor
  ///
  /// This performs a \i shallow copy, meaning the objects share the same underlying FieldMetaInfo.
  /// To deep copy the object call field_meta_info::clone().
  ///
  /// \b Example
  /// \code
  ///   meta_info_map m1(TypeID::Float32, std::vector<int>{10, 23, 30});
  ///   meta_info_map m2 = m1;
  ///
  ///   m1.meta_info().insert("key", true);
  ///   assert(m1 == m2); // m1 and m2 are equal as they share the same FieldMetaInfo
  /// \endcode
  ///
  /// \see field_meta_info::clone()
  field_meta_info(const field_meta_info& other) = default;

  /// \brief Move constructor
  field_meta_info(field_meta_info&&) = default;

  /// \brief Copy assignment
  ///
  /// This performs a \i shallow copy, meaning the objects share the same underlying FieldMetaInfo.
  /// To deep copy the object call field_meta_info::clone().
  ///
  /// \b Example
  /// /// \code
  ///   meta_info_map m1(TypeID::Float32, std::vector<int>{10, 23, 30});
  ///   meta_info_map m2 = m1;
  ///
  ///   m1.meta_info().insert("key", true);
  ///   assert(m1 == m2); // m1 and m2 are equal as they share the same FieldMetaInfo
  /// \endcode
  ///
  /// \see field_meta_info::clone()
  field_meta_info& operator=(const field_meta_info& other) = default;

  /// \brief Move assignment
  field_meta_info& operator=(field_meta_info&&) = default;

  /// \brief Clone the current field_meta_info object by performing a deep copy
  ///
  /// \b Example
  /// \code
  ///   meta_info_map m1(TypeID::Float32, std::vector<int>{10, 23, 30});
  ///   meta_info_map m2 = m1.clone();
  ///
  ///   m1.meta_info().insert("key", true);
  ///   assert(m1 != m2); // m1 and m2 are NOT equal as the don't share the same FieldMetaInfo
  /// \endcode
  field_meta_info clone() const {
    return field_meta_info(std::make_shared<FieldMetaInfo>(*field_meta_info_impl_));
  }

  /// \brief Swap with other
  void swap(field_meta_info& other) noexcept {
    field_meta_info_impl_->swap(*other.field_meta_info_impl_);
  };

  /// \brief Test for equality
  bool operator==(const field_meta_info& right) const noexcept {
    return (*field_meta_info_impl_ == *right.field_meta_info_impl_);
  }

  /// \brief Test for inequality
  bool operator!=(const field_meta_info& right) const noexcept { return (!(*this == right)); }

  /// \brief Access TypeID
  /// @{
  TypeID& type() noexcept { return field_meta_info_impl_->type(); }
  const TypeID& type() const noexcept { return field_meta_info_impl_->type(); }
  /// @}

  /// \brief Access dimensions
  /// @{
  std::vector<int>& dims() noexcept { return field_meta_info_impl_->dims(); }
  const std::vector<int>& dims() const noexcept { return field_meta_info_impl_->dims(); }
  /// @}

  /// \brief Access meta-info map
  meta_info_map meta_info() const noexcept {
    return meta_info_map(field_meta_info_impl_->metaInfoPtr());
  }

  /// \brief Convert to stream
  template <class StreamType>
  friend std::ostream& operator<<(StreamType&& stream, const field_meta_info& f) {
    return (stream << *f.field_meta_info_impl_);
  }

  /// \brief Get implementation pointer
  const std::shared_ptr<FieldMetaInfo>& impl() const { return field_meta_info_impl_; }

private:
  std::shared_ptr<FieldMetaInfo> field_meta_info_impl_;
};

} // namespace gridtools

} // namespace serialbox

#endif
