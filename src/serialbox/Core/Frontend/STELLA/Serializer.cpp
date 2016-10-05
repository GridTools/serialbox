//===-- serialbox/Core/Frontend/STELLA/Serializer.cpp -------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the Serializer implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Frontend/STELLA/Serializer.h"
#include "serialbox/Core/Frontend/STELLA/Utility.h"
#include "serialbox/Core/SerializerImpl.h"
#include "serialbox/Core/Unreachable.h"
#include <boost/make_shared.hpp>
#include <cstdlib>

namespace serialbox {

namespace stella {

int Serializer::enabled_ = 0;

Serializer::Serializer() { serializerImpl_ = nullptr; }

SerializerOpenMode Serializer::mode() const {
  switch(serializerImpl_->mode()) {
  case OpenModeKind::Read:
    return SerializerOpenModeRead;
  case OpenModeKind::Write:
    return SerializerOpenModeWrite;
  case OpenModeKind::Append:
    return SerializerOpenModeAppend;
  default:
    serialbox_unreachable("invalid mode");
  }
}

std::string Serializer::directory() const { return serializerImpl_->directory().string(); }

std::string Serializer::prefix() const { return serializerImpl_->prefix(); }

void Serializer::Init(const std::string& directory, const std::string& prefix,
                      SerializerOpenMode mode) {
  if(enabled_ == 0) {
    const char* envvar = std::getenv("STELLA_SERIALIZATION_DISABLED");
    enabled_ = (envvar && std::atoi(envvar) > 0) ? -1 : 1;
  }

  // Initialize SerializerImpl
  try {
    switch(mode) {
    case SerializerOpenModeRead:
      serializerImpl_ = boost::make_shared<SerializerImpl>(OpenModeKind::Read, directory, prefix,
                                                           "BinaryArchive");
      break;
    case SerializerOpenModeWrite:
      serializerImpl_ = boost::make_shared<SerializerImpl>(OpenModeKind::Write, directory, prefix,
                                                           "BinaryArchive");
      break;
    case SerializerOpenModeAppend:
      serializerImpl_ = boost::make_shared<SerializerImpl>(OpenModeKind::Append, directory, prefix,
                                                           "BinaryArchive");
      break;
    }
  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }

  // Initialize MetainfoSet
  globalMetainfo_.setImpl(
      internal::make_shared_ptr<MetaInfoMap>(serializerImpl_->globalMetaInfoPtr()));

  // Initialize savepoint vector
  for(auto& savepoint : serializerImpl_->savepoints())
    savepoints_.emplace_back(internal::make_shared_ptr(savepoint));

  // Initialize data field infos
  for(auto it = serializerImpl_->fieldMap().begin(), end = serializerImpl_->fieldMap().end();
      it != end; ++it)
    datafieldInfos_.insert({it->first, internal::make_shared_ptr<FieldMetaInfo>(it->second)});
}

bool Serializer::RegisterField(const std::string& name, std::string type, int bytesPerElement,
                               int iSize, int jSize, int kSize, int lSize, int iMinusHalo,
                               int iPlusHalo, int jMinusHalo, int jPlusHalo, int kMinusHalo,
                               int kPlusHalo, int lMinusHalo, int lPlusHalo) {
  try {
    TypeID typeID = internal::TypeNameToTypeID(type);
    if(bytesPerElement != TypeUtil::sizeOf(typeID))
      throw Exception("inconsistent bytes-per-element: got '%i' but according to passed type "
                      "'%s' expected '%i'",
                      bytesPerElement, type, TypeUtil::sizeOf(typeID));

    int rank =
        (iSize != 1 ? 1 : 0) + (jSize != 1 ? 1 : 0) + (kSize != 1 ? 1 : 0) + (lSize != 1 ? 1 : 0);

    std::vector<int> dims{iSize, jSize, kSize, lSize};
    MetaInfoMap metaInfo;
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

    FieldMetaInfo fieldMetaInfo(typeID, dims, metaInfo);

    // Field was already registered with the same meta-data
    if(serializerImpl_->hasField(name) &&
       (serializerImpl_->getFieldMetaInfoOf(name) == fieldMetaInfo)) {
      return false;
    }

    serializerImpl_->registerField(name, fieldMetaInfo);

    // Keep the frontend data-structure up to date
    auto it = serializerImpl_->fieldMap().findField(name);
    datafieldInfos_.insert({it->first, internal::make_shared_ptr<FieldMetaInfo>(it->second)});

  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
  return true;
}

const DataFieldInfo& Serializer::FindField(const std::string& fieldname) const {
  try {
    auto it = datafieldInfos_.find(fieldname);
    if(it == datafieldInfos_.end())
      throw Exception("field %s is not registered", fieldname);
    return it->second;

  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

void Serializer::AddFieldMetainfo(const std::string& fieldname, const std::string& key,
                                  bool value) {
  try {
    if(!serializerImpl_->addFieldMetaInfo(fieldname, key, value))
      throw Exception("metainfo with key = %s exists already in field %s", key, fieldname);
  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

void Serializer::AddFieldMetainfo(const std::string& fieldname, const std::string& key, int value) {
  try {
    if(!serializerImpl_->addFieldMetaInfo(fieldname, key, value))
      throw Exception("metainfo with key = %s exists already in field %s", key, fieldname);
  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

void Serializer::AddFieldMetainfo(const std::string& fieldname, const std::string& key,
                                  float value) {
  try {
    if(!serializerImpl_->addFieldMetaInfo(fieldname, key, value))
      throw Exception("metainfo with key = %s exists already in field %s", key, fieldname);
  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

void Serializer::AddFieldMetainfo(const std::string& fieldname, const std::string& key,
                                  double value) {
  try {
    if(!serializerImpl_->addFieldMetaInfo(fieldname, key, value))
      throw Exception("metainfo with key = %s exists already in field %s", key, fieldname);
  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

void Serializer::AddFieldMetainfo(const std::string& fieldname, const std::string& key,
                                  std::string value) {
  try {
    if(!serializerImpl_->addFieldMetaInfo(fieldname, key, value))
      throw Exception("metainfo with key = %s exists already in field %s", key, fieldname);
  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

std::vector<std::string> Serializer::fieldnames() const { return serializerImpl_->fieldnames(); }

std::vector<std::string> Serializer::FieldsAtSavepoint(const Savepoint& savepoint) const {
  std::vector<std::string> fields;

  // Check if savepoint exists
  int idx = serializerImpl_->savepointVector().find(*savepoint.getImpl());
  if(idx != -1) {
    // Iterate fields per savepoint
    const auto& fieldsPerSavepointMap = serializerImpl_->savepointVector().fieldsOf(idx);
    for(auto it = fieldsPerSavepointMap.begin(), end = fieldsPerSavepointMap.end(); it != end; ++it)
      fields.push_back(it->first);
  }
  return fields;
}

static StorageView makeStorageView(const void* pData, TypeID type, const std::vector<int>& dims,
                                   std::vector<int> strides) {

  int bytesPerElement = TypeUtil::sizeOf(type);

  // The StorageView operates on strides per type (not per byte)
  for(std::size_t i = 0; i < dims.size(); ++i)
    strides[i] /= bytesPerElement;

  // Check dimensions (strides.size() == 4 is guaranteed)
  if(dims.size() > 4)
    throw Exception("the STELLA frontend does not support %i dimensional storages", dims.size());

  // Adjust strides s.t dims.size() == strides.size()
  while(strides.size() != dims.size())
    strides.pop_back();

  return StorageView(const_cast<void*>(pData), type, dims, std::vector<int>(strides));
}

void Serializer::WriteField(const std::string& fieldName, const Savepoint& savepoint,
                            const void* pData, int iStride, int jStride, int kStride, int lStride) {
  if(enabled_ < 0)
    return;

  try {
    std::size_t numSavepoints = serializerImpl_->savepointVector().size();
    const FieldMetaInfo& info = serializerImpl_->getFieldMetaInfoOf(fieldName);

    StorageView storageView =
        makeStorageView(pData, info.type(), info.dims(), {iStride, jStride, kStride, lStride});

    serializerImpl_->write(fieldName, *savepoint.getImpl(), storageView);

    // Keep the frontend data-structure up to date
    if(numSavepoints < serializerImpl_->savepointVector().size())
      savepoints_.emplace_back(
          internal::make_shared_ptr<SavepointImpl>(serializerImpl_->savepointVector().back()));

  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

void Serializer::ReadField(const std::string& fieldName, const Savepoint& savepoint, void* pData,
                           int iStride, int jStride, int kStride, int lStride,
                           bool alsoPrevious) const {
  if(enabled_ < 0)
    return;

  (void)alsoPrevious;

  try {
    const FieldMetaInfo& info = serializerImpl_->getFieldMetaInfoOf(fieldName);

    StorageView storageView =
        makeStorageView(pData, info.type(), info.dims(), {iStride, jStride, kStride, lStride});

    serializerImpl_->read(fieldName, *savepoint.getImpl(), storageView);

  } catch(Exception& e) {
    internal::throwSerializationException("Error: %s", e.what());
  }
}

std::string Serializer::ToString() const {
  std::ostringstream ss;
  ss << serializerImpl_;
  return ss.str();
}

} // namespace stella

} // namespace serialbox
