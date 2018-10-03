//===-- serialbox/core/FieldMetainfoImpl.h ------------------------------------------*- C++
//-*-===//
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

#ifndef SERIALBOX_CORE_FIELDMAPMETAINFOIMPL_H
#define SERIALBOX_CORE_FIELDMAPMETAINFOIMPL_H

#include "serialbox/core/MetainfoMapImpl.h"
#include <memory>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Meta-information of a data field
class FieldMetainfoImpl {
public:
  /// \brief Default constructor
  FieldMetainfoImpl()
      : type_(TypeID::Invalid), dims_(), metaInfo_(std::make_shared<MetainfoMapImpl>()) {}

  /// \brief Construct members externally
  FieldMetainfoImpl(TypeID type, const std::vector<int>& dims, const MetainfoMapImpl& metaInfo)
      : type_(type), dims_(dims), metaInfo_(std::make_shared<MetainfoMapImpl>(metaInfo)) {}

  /// \brief Construct members externally
  FieldMetainfoImpl(TypeID type, const std::vector<int>& dims)
      : type_(type), dims_(dims), metaInfo_(std::make_shared<MetainfoMapImpl>()) {}

  /// \brief Copy constructor
  FieldMetainfoImpl(const FieldMetainfoImpl& other) { *this = other; }

  /// \brief Move constructor
  FieldMetainfoImpl(FieldMetainfoImpl&&) = default;

  /// \brief Copy assignment
  FieldMetainfoImpl& operator=(const FieldMetainfoImpl& other);

  /// \brief Move assignment
  FieldMetainfoImpl& operator=(FieldMetainfoImpl&&) = default;

  /// \brief Swap with other
  void swap(FieldMetainfoImpl& other) noexcept;

  /// \brief Test for equality
  bool operator==(const FieldMetainfoImpl& right) const noexcept;

  /// \brief Test for inequality
  bool operator!=(const FieldMetainfoImpl& right) const noexcept { return (!(*this == right)); }

  /// \brief Access TypeID
  TypeID& type() noexcept { return type_; }
  const TypeID& type() const noexcept { return type_; }

  /// \brief Access dimensions
  std::vector<int>& dims() noexcept { return dims_; }
  const std::vector<int>& dims() const noexcept { return dims_; }

  /// \brief Access meta-info map
  MetainfoMapImpl& metaInfo() noexcept { return *metaInfo_; }
  const MetainfoMapImpl& metaInfo() const noexcept { return *metaInfo_; }

  /// \brief Convert to string
  std::string toString() const;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const FieldMetainfoImpl& f);

  /// \brief Get meta-info pointer
  std::shared_ptr<MetainfoMapImpl>& metaInfoPtr() noexcept { return metaInfo_; }
  const std::shared_ptr<MetainfoMapImpl>& metaInfoPtr() const noexcept { return metaInfo_; }

private:
  TypeID type_;
  std::vector<int> dims_;
  std::shared_ptr<MetainfoMapImpl> metaInfo_;
};

/// @}

} // namespace serialbox

#endif
