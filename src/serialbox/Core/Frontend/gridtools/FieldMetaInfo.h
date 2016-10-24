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
#include "serialbox/Core/Frontend/gridtools/StorageViewHelper.h"
#include "serialbox/Core/Frontend/gridtools/Type.h"
#include <memory>

namespace serialbox {

namespace gridtools {

/// \brief Meta-information of a data field
///
/// \ingroup gridtools
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
  field_meta_info(type_id type, const std::vector<int>& dims, const meta_info_map& meta_info)
      : field_meta_info_impl_(std::make_shared<FieldMetaInfo>(type, dims, *meta_info.impl())) {}

  /// \brief Construct field meta-information
  ///
  /// Meta-information can be added later on with field_meta_info::meta_info.
  ///
  /// \param type      Type of the field
  /// \param dims      Dimension of the field
  field_meta_info(type_id type, const std::vector<int>& dims)
      : field_meta_info_impl_(std::make_shared<FieldMetaInfo>(type, dims)) {}

  /// \brief Construct field meta-information with a gridtools storage
  ///
  /// \param storage  gridtools storage
  template <class StorageType,
            class = typename std::enable_if<!std::is_same<typename std::decay<StorageType>::type,
                                                          field_meta_info>::value>::type>
  field_meta_info(const StorageType& storage) {
    TypeID typeID = ToTypeID<typename StorageType::value_type>::value;
    field_meta_info_impl_ = std::make_shared<FieldMetaInfo>(typeID, internal::get_dims(storage));
  }

  /// \brief Copy constructor
  ///
  /// This performs a \i shallow copy, meaning the objects share the same underlying FieldMetaInfo.
  /// To deep copy the object call field_meta_info::clone().
  ///
  /// \b Example
  /// \code
  ///   field_meta_info f1(type_id::Float32, std::vector<int>{10, 23, 30});
  ///   field_meta_info f2(f1);
  ///
  ///   f1.meta_info().insert("key", true);
  ///   assert(f1 == f2); // f1 and f2 are equal as they share the same FieldMetaInfo
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
  ///   field_meta_info f1(type_id::Float32, std::vector<int>{10, 23, 30});
  ///   field_meta_info f2 = f1;
  ///
  ///   f1.meta_info().insert("key", true);
  ///   assert(f1 == f2); // f1 and f2 are equal as they share the same FieldMetaInfo
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
  ///   field_meta_info f1(type_id::Float32, std::vector<int>{10, 23, 30});
  ///   field_meta_info f2 = f1.clone();
  ///
  ///   f1.meta_info().insert("key", true);
  ///   assert(f1 != f2); // f1 and f2 are NOT equal as the don't share the same FieldMetaInfo
  /// \endcode
  field_meta_info clone() const {
    return field_meta_info(std::make_shared<FieldMetaInfo>(*field_meta_info_impl_));
  }

  /// \brief Construct with FieldMetaInfo (internal use)
  explicit field_meta_info(const std::shared_ptr<FieldMetaInfo>& field_meta_info_ptr) {
    field_meta_info_impl_ = field_meta_info_ptr;
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

  /// \brief Access type_id
  /// @{
  type_id& type() noexcept { return field_meta_info_impl_->type(); }
  const type_id& type() const noexcept { return field_meta_info_impl_->type(); }
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
  friend std::ostream& operator<<(std::ostream& stream, const field_meta_info& f) {
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
