/*===-- serialbox-c/Serializer.cpp --------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains C implementation of the Serializer.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Logging.h"
#include "serialbox-c/Savepoint.h"
#include "serialbox-c/Serializer.h"
#include "serialbox-c/Utility.h"
#include "serialbox/Core/Archive/ArchiveFactory.h"
#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Logging.h"
#include "serialbox/Core/SerializerImpl.h"
#include "serialbox/Core/Slice.h"
#include "serialbox/Core/StorageView.h"

using namespace serialboxC;

namespace internal {

template <class VecType>
static std::string vecToString(VecType&& vec) {
  std::stringstream ss;
  if(!vec.empty()) {
    for(std::size_t i = 0; i < vec.size() - 1; ++i)
      ss << vec[i] << ", ";
    ss << vec.back();
  }
  return ss.str();
}

} // namespace internal

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

serialboxSerializer_t* serialboxSerializerCreate(serialboxOpenModeKind mode, const char* directory,
                                                 const char* prefix, const char* archive) {
  serialboxSerializer_t* serializer = allocate<serialboxSerializer_t>();
  try {
    switch(mode) {
    case Read:
      serializer->impl = new Serializer(serialbox::OpenModeKind::Read, directory, prefix, archive);
      break;
    case Write:
      serializer->impl = new Serializer(serialbox::OpenModeKind::Write, directory, prefix, archive);
      break;
    case Append:
      serializer->impl =
          new Serializer(serialbox::OpenModeKind::Append, directory, prefix, archive);
      break;
    }
    serializer->ownsData = 1;
  } catch(std::exception& e) {
    std::free(serializer);
    serializer = NULL;
    serialboxFatalError(e.what());
  }
  return serializer;
}

void serialboxSerializerDestroy(serialboxSerializer_t* serializer) {
  if(serializer) {
    Serializer* ser = toSerializer(serializer);
    if(serializer->ownsData)
      delete ser;
    std::free(serializer);
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

serialboxOpenModeKind serialboxSerializerGetMode(const serialboxSerializer_t* serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return static_cast<serialboxOpenModeKind>(static_cast<int>(ser->mode()));
}

const char* serialboxSerializerGetDirectory(const serialboxSerializer_t* serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return ser->directory().c_str();
}

const char* serialboxSerializerGetPrefix(const serialboxSerializer_t* serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return ser->prefix().c_str();
}

void serialboxSerializerUpdateMetaData(serialboxSerializer_t* serializer) {
  Serializer* ser = toSerializer(serializer);
  try {
    ser->updateMetaData();
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

int serialboxSerializationStatus(void) { return Serializer::serializationStatus(); }

void serialboxEnableSerialization(void) { Serializer::enableSerialization(); }

void serialboxDisableSerialization(void) { Serializer::disableSerialization(); }

/*===------------------------------------------------------------------------------------------===*\
 *     Global Meta-information
\*===------------------------------------------------------------------------------------------===*/

serialboxMetaInfo_t* serialboxSerializerGetGlobalMetaInfo(serialboxSerializer_t* serializer) {
  Serializer* ser = toSerializer(serializer);
  serialboxMetaInfo_t* metaInfo = allocate<serialboxMetaInfo_t>();
  metaInfo->impl = ser->globalMetaInfoPtr().get();
  metaInfo->ownsData = 0;
  return metaInfo;
}

/*===------------------------------------------------------------------------------------------===*\
 *     Register and Query Savepoints
\*===------------------------------------------------------------------------------------------===*/

int serialboxSerializerAddSavepoint(serialboxSerializer_t* serializer,
                                    const serialboxSavepoint_t* savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  Serializer* ser = toSerializer(serializer);
  return ser->registerSavepoint(*sp);
}

int serialboxSerializerHasSavepoint(const serialboxSerializer_t* serializer,
                                    const serialboxSavepoint_t* savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  const Serializer* ser = toConstSerializer(serializer);
  return ser->savepointVector().exists(*sp);
}

int serialboxSerializerGetNumSavepoints(const serialboxSerializer_t* serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return (int)ser->savepointVector().size();
}

serialboxSavepoint_t**
serialboxSerializerGetSavepointVector(const serialboxSerializer_t* serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  const auto& savepointVector = ser->savepointVector().savepoints();

  serialboxSavepoint_t** savepoints = allocate<serialboxSavepoint_t*>(savepointVector.size());

  if(!savepoints)
    serialboxFatalError("out of memory");

  for(std::size_t i = 0; i < savepointVector.size(); ++i) {
    serialboxSavepoint_t* savepoint = allocate<serialboxSavepoint_t>();
    savepoint->impl = savepointVector[i].get();
    savepoint->ownsData = 0;
    savepoints[i] = savepoint;
  }

  return savepoints;
}

void serialboxSerializerDestroySavepointVector(serialboxSavepoint_t** savepointVector, int len) {
  for(int i = 0; i < len; ++i)
    std::free(savepointVector[i]);
  std::free(savepointVector);
}

serialboxArrayOfString_t*
serialboxSerializerGetFieldnamesAtSavepoint(const serialboxSerializer_t* serializer,
                                            const serialboxSavepoint_t* savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  const Serializer* ser = toConstSerializer(serializer);
  serialboxArrayOfString_t* array = NULL;

  try {
    const auto& fieldnameMap = ser->savepointVector().fieldsOf(*sp);
    array = serialboxArrayOfStringCreate(fieldnameMap.size());

    int i = 0;
    for(auto it = fieldnameMap.begin(), end = fieldnameMap.end(); it != end; ++it, ++i)
      array->data[i] = allocateAndCopyString(it->first);

  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return array;
}

/*===------------------------------------------------------------------------------------------===*\
 *     Register and Query Fields
\*===------------------------------------------------------------------------------------------===*/

int serialboxSerializerAddField(serialboxSerializer_t* serializer, const char* name,
                                const serialboxFieldMetaInfo_t* fieldMetaInfo) {
  Serializer* ser = toSerializer(serializer);
  const FieldMetaInfo* info = toConstFieldMetaInfo(fieldMetaInfo);

  try {
    ser->registerField(name, *info);
  } catch(std::exception& e) {
    LOG(warning) << e.what();
    return 0;
  }
  return 1;
}

int serialboxSerializerHasField(serialboxSerializer_t* serializer, const char* field) {
  Serializer* ser = toSerializer(serializer);
  return ser->hasField(field);
}

int serialboxSerializerAddField2(serialboxSerializer_t* serializer, const char* name,
                                 serialboxTypeID type, int bytesPerElement, int iSize, int jSize,
                                 int kSize, int lSize, int iMinusHalo, int iPlusHalo,
                                 int jMinusHalo, int jPlusHalo, int kMinusHalo, int kPlusHalo,
                                 int lMinusHalo, int lPlusHalo) {
  Serializer* ser = toSerializer(serializer);

  try {
    serialbox::TypeID typeID = static_cast<serialbox::TypeID>((int)type);
    std::string typeName = serialbox::TypeUtil::toString(typeID);

    if(bytesPerElement != serialbox::TypeUtil::sizeOf(typeID))
      throw serialbox::Exception("inconsistent bytes-per-element: got '%i' but according to passed "
                                 "type '%s' expected '%i'",
                                 bytesPerElement, typeName, serialbox::TypeUtil::sizeOf(typeID));

    int rank =
        (iSize != 1 ? 1 : 0) + (jSize != 1 ? 1 : 0) + (kSize != 1 ? 1 : 0) + (lSize != 1 ? 1 : 0);

    std::vector<int> dims{iSize, jSize, kSize, lSize};
    MetaInfoMap metaInfo;
    metaInfo.insert("__name", name);
    metaInfo.insert("__elementtype", typeName);
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
    if(ser->hasField(name) && (ser->getFieldMetaInfoOf(name) == fieldMetaInfo))
      return 0;

    ser->registerField(name, fieldMetaInfo);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return 1;
}

serialboxArrayOfString_t*
serialboxSerializerGetFieldnames(const serialboxSerializer_t* serializer) {

  const Serializer* ser = toConstSerializer(serializer);
  serialboxArrayOfString_t* array = NULL;

  try {
    const auto fieldnameVector = ser->fieldnames();
    array = serialboxArrayOfStringCreate(fieldnameVector.size());

    for(std::size_t i = 0; i < fieldnameVector.size(); ++i)
      array->data[i] = allocateAndCopyString(fieldnameVector[i]);

  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }

  return array;
}

serialboxFieldMetaInfo_t*
serialboxSerializerGetFieldMetaInfo(const serialboxSerializer_t* serializer, const char* name) {
  const Serializer* ser = toConstSerializer(serializer);

  auto it = ser->fieldMap().findField(name);
  if(it != ser->fieldMap().end()) {
    serialboxFieldMetaInfo_t* info = allocate<serialboxFieldMetaInfo_t>();
    info->impl = it->second.get();
    info->ownsData = 0;
    return info;
  }
  return NULL;
}

/*===------------------------------------------------------------------------------------------===*\
 *     Writing & Reading
\*===------------------------------------------------------------------------------------------===*/

namespace internal {

serialbox::StorageView makeStorageView(Serializer* ser, const char* name, void* originPtr,
                                       const int* strides, int numStrides) {

  // Check if field exists
  auto it = ser->fieldMap().findField(name);
  if(it == ser->fieldMap().end())
    throw serialbox::Exception("field '%s' is not registerd within the Serializer", name);

  // Get necessary meta-information to construct StorageView
  const auto& dims = it->second->dims();
  std::vector<int> stridesVec(strides, strides + numStrides);

  if(dims.size() != stridesVec.size())
    throw serialbox::Exception("inconsistent number of dimensions and strides of field '%s'"
                               "\nDimensions as: [ %s ]"
                               "\nStrides    as: [ %s ]",
                               name, internal::vecToString(dims),
                               internal::vecToString(stridesVec));

  return serialbox::StorageView(originPtr, it->second->type(), dims, stridesVec);
}

} // namespace internal

void serialboxSerializerWrite(serialboxSerializer_t* serializer, const char* name,
                              const serialboxSavepoint_t* savepoint, void* originPtr,
                              const int* strides, int numStrides) {
  Serializer* ser = toSerializer(serializer);
  const Savepoint* sp = toConstSavepoint(savepoint);

  try {
    serialbox::StorageView storageView(
        internal::makeStorageView(ser, name, originPtr, strides, numStrides));
    ser->write(name, *sp, storageView);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxSerializerRead(serialboxSerializer_t* serializer, const char* name,
                             const serialboxSavepoint_t* savepoint, void* originPtr,
                             const int* strides, int numStrides) {
  Serializer* ser = toSerializer(serializer);
  const Savepoint* sp = toConstSavepoint(savepoint);

  try {
    serialbox::StorageView storageView(
        internal::makeStorageView(ser, name, originPtr, strides, numStrides));
    ser->read(name, *sp, storageView);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxSerializerReadSliced(serialboxSerializer_t* serializer, const char* name,
                                   const serialboxSavepoint_t* savepoint, void* originPtr,
                                   const int* strides, int numStrides, const int* slice) {
  Serializer* ser = toSerializer(serializer);
  const Savepoint* sp = toConstSavepoint(savepoint);

  try {
    serialbox::StorageView storageView(
        internal::makeStorageView(ser, name, originPtr, strides, numStrides));

    serialbox::Slice sliceObj((serialbox::Slice::Empty()));
    for(int i = 0; i < numStrides; ++i)
      sliceObj.sliceTriples().push_back({slice[3 * i], slice[3 * i + 1], slice[3 * i + 2]});
    storageView.setSlice(sliceObj);
    
    ser->read(name, *sp, storageView);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Stateless Serialization
\*===------------------------------------------------------------------------------------------===*/

void serialboxWriteToFile(const char* filename, void* originPtr, serialboxTypeID typeID,
                          const int* dims, int numDims, const int* strides, const char* fieldname,
                          const char* archivename) {

  try {
    std::vector<int> dimsVec(dims, dims + numDims);
    std::vector<int> stridesVec(strides, strides + numDims);
    serialbox::StorageView storageView(originPtr, (serialbox::TypeID)typeID, dimsVec, stridesVec);

    serialbox::ArchiveFactory::writeToFile(filename, storageView, archivename, fieldname);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxReadFromFile(const char* filename, void* originPtr, serialboxTypeID typeID,
                           const int* dims, int numDims, const int* strides, const char* fieldname,
                           const char* archivename) {

  try {
    std::vector<int> dimsVec(dims, dims + numDims);
    std::vector<int> stridesVec(strides, strides + numDims);
    serialbox::StorageView storageView(originPtr, (serialbox::TypeID)typeID, dimsVec, stridesVec);

    serialbox::ArchiveFactory::readFromFile(filename, storageView, archivename, fieldname);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}
