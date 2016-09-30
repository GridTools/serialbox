//===-- serialbox/Core/Frontend/STELLA/DataFieldInfo.cpp ----------------------------*- C++ -*-===//
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

#include "serialbox/Core/FieldMetaInfo.h"
#include "serialbox/Core/Frontend/STELLA/DataFieldInfo.h"
#include "serialbox/Core/Frontend/STELLA/Utility.h"
#include "serialbox/Core/Type.h"

namespace serialbox {

namespace stella {

DataFieldInfo::DataFieldInfo()
    : owner_(true), fieldMetaInfoImpl_(new FieldMetaInfo),
      metainfo_(&fieldMetaInfoImpl_->metaInfo()) {}

DataFieldInfo::DataFieldInfo(FieldMetaInfo* fieldMetaInfoImpl)
    : owner_(false), fieldMetaInfoImpl_(fieldMetaInfoImpl),
      metainfo_(&fieldMetaInfoImpl_->metaInfo()) {}

DataFieldInfo::DataFieldInfo(const DataFieldInfo& other) {
  fieldMetaInfoImpl_ = new FieldMetaInfo(*other.fieldMetaInfoImpl_);
  metainfo_.setImpl(&fieldMetaInfoImpl_->metaInfo());
  owner_ = true;
}

DataFieldInfo& DataFieldInfo::operator=(const DataFieldInfo& other) {
  *fieldMetaInfoImpl_ = *other.fieldMetaInfoImpl_;
  return (*this);
}

DataFieldInfo::~DataFieldInfo() {
  if(owner_)
    delete fieldMetaInfoImpl_;
}

void DataFieldInfo::Init(std::string name, std::string type, int bytesPerElement, int rank,
                         int iSize, int jSize, int kSize, int lSize, int iMinusHalo, int iPlusHalo,
                         int jMinusHalo, int jPlusHalo, int kMinusHalo, int kPlusHalo,
                         int lMinusHalo, int lPlusHalo) {

  try {
    TypeID typeID = internal::TypeNameToTypeID(type);
    fieldMetaInfoImpl_->type() = typeID;

    if(bytesPerElement != TypeUtil::sizeOf(typeID))
      throw Exception("inconsistent bytes-per-element: got '%i' but according to passed type "
                      "'%s' expected '%i'",
                      bytesPerElement, type, TypeUtil::sizeOf(typeID));

    fieldMetaInfoImpl_->dims() = std::vector<int>{iSize, jSize, kSize, lSize};

    MetaInfoMap& metaInfo = fieldMetaInfoImpl_->metaInfo();
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
  return fieldMetaInfoImpl_->metaInfo().hasKey("__name")
             ? fieldMetaInfoImpl_->metaInfo().as<std::string>("__name")
             : "";
}

std::string DataFieldInfo::type() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__elementtype")
             ? fieldMetaInfoImpl_->metaInfo().as<std::string>("__elementtype")
             : "";
}

int DataFieldInfo::bytesPerElement() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__bytesperelement")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__bytesperelement")
             : 0;
}

int DataFieldInfo::rank() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__rank")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__rank")
             : 0;
}

int DataFieldInfo::iSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__isize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__isize")
             : 1;
}

int DataFieldInfo::jSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__jsize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__jsize")
             : 1;
}

int DataFieldInfo::kSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__ksize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__ksize")
             : 1;
}

int DataFieldInfo::lSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__lsize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__lsize")
             : 1;
}

int DataFieldInfo::iMinusHaloSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__iminushalosize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__iminushalosize")
             : 0;
}

int DataFieldInfo::iPlusHaloSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__iplushalosize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__iplushalosize")
             : 0;
}

int DataFieldInfo::jMinusHaloSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__jminushalosize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__jminushalosize")
             : 0;
}

int DataFieldInfo::jPlusHaloSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__jplushalosize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__jplushalosize")
             : 0;
}

int DataFieldInfo::kMinusHaloSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__kminushalosize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__kminushalosize")
             : 0;
}

int DataFieldInfo::kPlusHaloSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__kplushalosize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__kplushalosize")
             : 0;
}

int DataFieldInfo::lMinusHaloSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__lminushalosize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__lminushalosize")
             : 0;
}

int DataFieldInfo::lPlusHaloSize() const {
  return fieldMetaInfoImpl_->metaInfo().hasKey("__lplushalosize")
             ? fieldMetaInfoImpl_->metaInfo().as<int>("__lplushalosize")
             : 0;
}

bool DataFieldInfo::operator==(const DataFieldInfo& other) const {
  return (*fieldMetaInfoImpl_ == *other.fieldMetaInfoImpl_);
}

std::string DataFieldInfo::ToString() const {
  std::ostringstream ss;
  const auto& metaInfo = fieldMetaInfoImpl_->metaInfo();

  std::string name = metaInfo.hasKey("__name") ? metaInfo.as<std::string>("__name") : "";

  const auto& dims = fieldMetaInfoImpl_->dims();
  int iSize = dims.size() < 1 ? 1 : dims[0];
  int jSize = dims.size() < 2 ? 1 : dims[1];
  int kSize = dims.size() < 3 ? 1 : dims[2];
  int lSize = dims.size() < 4 ? 1 : dims[3];

  ss << name << " (" << iSize << "x" << jSize << "x" << kSize << "x" << lSize << ") "
     << MetainfoSet(const_cast<MetaInfoMap*>(&metaInfo)).ToString();
  return ss.str();
}

} // namespace stella

} // namespace serialbox
