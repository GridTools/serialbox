//===-- serialbox/core/frontend/stella/DataFieldInfo.cpp ----------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the DataFieldInfo implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/frontend/stella/DataFieldInfo.h"
#include "serialbox/core/FieldMetainfoImpl.h"
#include "serialbox/core/Type.h"
#include "serialbox/core/frontend/stella/Utility.h"

namespace serialbox {

namespace stella {

DataFieldInfo::DataFieldInfo() : fieldMetainfoImpl_(), metainfo_() {}

DataFieldInfo::DataFieldInfo(const boost::shared_ptr<FieldMetainfoImpl>& fieldMetainfoImpl)
    : fieldMetainfoImpl_(fieldMetainfoImpl),
      metainfo_(internal::make_shared_ptr<MetainfoMapImpl>(fieldMetainfoImpl_->metaInfoPtr())) {}

DataFieldInfo::DataFieldInfo(const DataFieldInfo& other)
    : fieldMetainfoImpl_(other.fieldMetainfoImpl_),
      metainfo_(internal::make_shared_ptr<MetainfoMapImpl>(fieldMetainfoImpl_->metaInfoPtr())) {}

DataFieldInfo& DataFieldInfo::operator=(const DataFieldInfo& other) {
  *fieldMetainfoImpl_ = *other.fieldMetainfoImpl_;
  return (*this);
}

void DataFieldInfo::Init(std::string name, std::string type, int bytesPerElement, int rank,
                         int iSize, int jSize, int kSize, int lSize, int iMinusHalo, int iPlusHalo,
                         int jMinusHalo, int jPlusHalo, int kMinusHalo, int kPlusHalo,
                         int lMinusHalo, int lPlusHalo) {

  fieldMetainfoImpl_ = boost::make_shared<FieldMetainfoImpl>();
  metainfo_.setImpl(internal::make_shared_ptr<MetainfoMapImpl>(fieldMetainfoImpl_->metaInfoPtr()));

  try {
    TypeID typeID = internal::TypeNameToTypeID(type);
    fieldMetainfoImpl_->type() = typeID;

    if(bytesPerElement != TypeUtil::sizeOf(typeID))
      throw Exception("inconsistent bytes-per-element: got '%i' but according to passed type "
                      "'%s' expected '%i'",
                      bytesPerElement, type, TypeUtil::sizeOf(typeID));

    fieldMetainfoImpl_->dims() = std::vector<int>{iSize, jSize, kSize, lSize};

    MetainfoMapImpl& metaInfo = fieldMetainfoImpl_->metaInfo();
    metaInfo.clear();
    metaInfo.insert("__name", name);
    metaInfo.insert("__elementtype", type);
    metaInfo.insert("__bytesperelement", bytesPerElement);
    metaInfo.insert("__rank", rank);
    metaInfo.insert("__isize", iSize);
    metaInfo.insert("__jsize", jSize);
    metaInfo.insert("__ksize", kSize);
    metaInfo.insert("__lsize", lSize);
    metaInfo.insert("__iminushalosize", iMinusHalo);
    metaInfo.insert("__iplushalosize", iPlusHalo);
    metaInfo.insert("__jminushalosize", jMinusHalo);
    metaInfo.insert("__jplushalosize", jPlusHalo);
    metaInfo.insert("__kminushalosize", kMinusHalo);
    metaInfo.insert("__kplushalosize", kPlusHalo);
    metaInfo.insert("__lminushalosize", lMinusHalo);
    metaInfo.insert("__lplushalosize", lPlusHalo);

  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

std::string DataFieldInfo::name() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__name")
             ? fieldMetainfoImpl_->metaInfo().as<std::string>("__name")
             : "";
}

std::string DataFieldInfo::type() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__elementtype")
             ? fieldMetainfoImpl_->metaInfo().as<std::string>("__elementtype")
             : "";
}

int DataFieldInfo::bytesPerElement() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__bytesperelement")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__bytesperelement")
             : 0;
}

int DataFieldInfo::rank() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__rank")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__rank")
             : 0;
}

int DataFieldInfo::iSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__isize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__isize")
             : 1;
}

int DataFieldInfo::jSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__jsize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__jsize")
             : 1;
}

int DataFieldInfo::kSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__ksize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__ksize")
             : 1;
}

int DataFieldInfo::lSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__lsize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__lsize")
             : 1;
}

int DataFieldInfo::iMinusHaloSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__iminushalosize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__iminushalosize")
             : 0;
}

int DataFieldInfo::iPlusHaloSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__iplushalosize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__iplushalosize")
             : 0;
}

int DataFieldInfo::jMinusHaloSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__jminushalosize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__jminushalosize")
             : 0;
}

int DataFieldInfo::jPlusHaloSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__jplushalosize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__jplushalosize")
             : 0;
}

int DataFieldInfo::kMinusHaloSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__kminushalosize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__kminushalosize")
             : 0;
}

int DataFieldInfo::kPlusHaloSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__kplushalosize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__kplushalosize")
             : 0;
}

int DataFieldInfo::lMinusHaloSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__lminushalosize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__lminushalosize")
             : 0;
}

int DataFieldInfo::lPlusHaloSize() const {
  return fieldMetainfoImpl_->metaInfo().hasKey("__lplushalosize")
             ? fieldMetainfoImpl_->metaInfo().as<int>("__lplushalosize")
             : 0;
}

bool DataFieldInfo::operator==(const DataFieldInfo& other) const {
  return (*fieldMetainfoImpl_ == *other.fieldMetainfoImpl_);
}

std::string DataFieldInfo::ToString() const {
  std::ostringstream ss;
  const auto& metaInfo = fieldMetainfoImpl_->metaInfo();

  std::string name = metaInfo.hasKey("__name") ? metaInfo.as<std::string>("__name") : "";

  const auto& dims = fieldMetainfoImpl_->dims();
  int iSize = dims.size() < 1 ? 1 : dims[0];
  int jSize = dims.size() < 2 ? 1 : dims[1];
  int kSize = dims.size() < 3 ? 1 : dims[2];
  int lSize = dims.size() < 4 ? 1 : dims[3];

  ss << name << " (" << iSize << "x" << jSize << "x" << kSize << "x" << lSize << ") "
     << metainfo_.ToString();
  return ss.str();
}

void DataFieldInfo::setImpl(const boost::shared_ptr<FieldMetainfoImpl>& fieldMetainfoImpl) {
  metainfo_.setImpl(internal::make_shared_ptr<MetainfoMapImpl>(fieldMetainfoImpl_->metaInfoPtr()));
}

boost::shared_ptr<FieldMetainfoImpl>& DataFieldInfo::getImpl() { return fieldMetainfoImpl_; }

const boost::shared_ptr<FieldMetainfoImpl>& DataFieldInfo::getImpl() const {
  return fieldMetainfoImpl_;
}

} // namespace stella

} // namespace serialbox
