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

#include "serialbox-c/Serializer.h"
#include "serialbox-c/FieldMetainfo.h"
#include "serialbox-c/Logging.h"
#include "serialbox-c/Metainfo.h"
#include "serialbox-c/Savepoint.h"
#include "serialbox-c/Utility.h"
#include "serialbox/core/Exception.h"
#include "serialbox/core/Logging.h"
#include "serialbox/core/SerializerImpl.h"
#include "serialbox/core/Slice.h"
#include "serialbox/core/StorageView.h"
#include "serialbox/core/Unreachable.h"
#include "serialbox/core/archive/ArchiveFactory.h"

using namespace serialboxC;

namespace {

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

std::vector<int> make_dims(int iSize, int jSize, int kSize, int lSize) {
  std::vector<int> dims;
  if(iSize > 0)
    dims.push_back(iSize);
  if(jSize > 0)
    dims.push_back(jSize);
  if(kSize > 0)
    dims.push_back(kSize);
  if(lSize > 0)
    dims.push_back(lSize);
  return dims;
}
}

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

serialboxSerializer_t* serialboxSerializerCreate(int mode, const char* directory,
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
    default:
      serialbox_unreachable((boost::format("invalid mode (%i)") % mode).str().c_str());
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

int serialboxSerializerGetMode(const serialboxSerializer_t* serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return static_cast<int>(ser->mode());
}

const char* serialboxSerializerGetDirectory(const serialboxSerializer_t* serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return toCharP(ser->directory().c_str());
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

char* serialboxSerializerToString(const serialboxSerializer_t* serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  std::stringstream ss;
  ss << *ser;
  return allocateAndCopyString(ss.str());
}

/*===------------------------------------------------------------------------------------------===*\
 *     Global Meta-information
\*===------------------------------------------------------------------------------------------===*/

serialboxMetainfo_t* serialboxSerializerGetGlobalMetainfo(serialboxSerializer_t* serializer) {
  Serializer* ser = toSerializer(serializer);
  serialboxMetainfo_t* metaInfo = allocate<serialboxMetainfo_t>();
  metaInfo->impl = ser->globalMetainfoPtr().get();
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
                                const serialboxFieldMetainfo_t* fieldMetainfo) {
  Serializer* ser = toSerializer(serializer);
  const FieldMetainfo* info = toConstFieldMetainfo(fieldMetainfo);

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

int serialboxSerializerAddField2(serialboxSerializer_t* serializer, const char* name, int type,
                                 int bytesPerElement, int iSize, int jSize, int kSize, int lSize,
                                 int iMinusHalo, int iPlusHalo, int jMinusHalo, int jPlusHalo,
                                 int kMinusHalo, int kPlusHalo, int lMinusHalo, int lPlusHalo) {
  Serializer* ser = toSerializer(serializer);

  try {
    serialbox::TypeID typeID = static_cast<serialbox::TypeID>((int)type);
    std::string typeName = serialbox::TypeUtil::toString(typeID);

    if(bytesPerElement != serialbox::TypeUtil::sizeOf(typeID))
      throw serialbox::Exception("inconsistent bytes-per-element: got '%i' but according to passed "
                                 "type '%s' expected '%i'",
                                 bytesPerElement, typeName, serialbox::TypeUtil::sizeOf(typeID));

    auto dims = ::make_dims(iSize, jSize, kSize, lSize);
    int rank = dims.size();

    MetainfoMap metaInfo;
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

    FieldMetainfo fieldMetainfo(typeID, dims, metaInfo);

    // Field was already registered with the same meta-data
    if(ser->hasField(name) && (ser->getFieldMetainfoImplOf(name) == fieldMetainfo))
      return 0;

    ser->registerField(name, fieldMetainfo);
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

serialboxFieldMetainfo_t*
serialboxSerializerGetFieldMetainfo(const serialboxSerializer_t* serializer, const char* name) {
  const Serializer* ser = toConstSerializer(serializer);

  auto it = ser->fieldMap().findField(name);
  if(it != ser->fieldMap().end()) {
    serialboxFieldMetainfo_t* info = allocate<serialboxFieldMetainfo_t>();
    info->impl = it->second.get();
    info->ownsData = 0;
    return info;
  }
  return NULL;
}

void serialboxSerializerGetFieldMetainfo2(const serialboxSerializer_t* serializer, const char* name,
                                          char** storedName, char** elementType,
                                          int* bytesPerElement, int* rank, int* iSize, int* jSize,
                                          int* kSize, int* lSize, int* iMinusHalo, int* iPlusHalo,
                                          int* jMinusHalo, int* jPlusHalo, int* kMinusHalo,
                                          int* kPlusHalo, int* lMinusHalo, int* lPlusHalo) {

  try {
    serialboxFieldMetainfo_t* fieldMetainfo = serialboxSerializerGetFieldMetainfo(serializer, name);
    serialboxMetainfo_t* metainfo = serialboxFieldMetainfoGetMetainfo(fieldMetainfo);

    *storedName = serialboxMetainfoGetString(metainfo, "__name");
    *elementType = serialboxMetainfoGetString(metainfo, "__elementtype");
    *bytesPerElement = serialboxMetainfoGetInt32(metainfo, "__bytesperelement");
    *rank = serialboxMetainfoGetInt32(metainfo, "__rank");
    *iSize = serialboxMetainfoGetInt32(metainfo, "__isize");
    *jSize = serialboxMetainfoGetInt32(metainfo, "__jsize");
    *kSize = serialboxMetainfoGetInt32(metainfo, "__ksize");
    *lSize = serialboxMetainfoGetInt32(metainfo, "__lsize");
    *iMinusHalo = serialboxMetainfoGetInt32(metainfo, "__iminushalosize");
    *jMinusHalo = serialboxMetainfoGetInt32(metainfo, "__jminushalosize");
    *kMinusHalo = serialboxMetainfoGetInt32(metainfo, "__kminushalosize");
    *lMinusHalo = serialboxMetainfoGetInt32(metainfo, "__lminushalosize");
    *iPlusHalo = serialboxMetainfoGetInt32(metainfo, "__iplushalosize");
    *jPlusHalo = serialboxMetainfoGetInt32(metainfo, "__jplushalosize");
    *kPlusHalo = serialboxMetainfoGetInt32(metainfo, "__kplushalosize");
    *lPlusHalo = serialboxMetainfoGetInt32(metainfo, "__lplushalosize");
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
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
    throw serialbox::Exception("field '%s' is not registered within the Serializer", name);

  // Get necessary meta-information to construct StorageView
  const auto& dims = it->second->dims();
  std::vector<int> stridesVec(strides, strides + numStrides);

  if(dims.size() != stridesVec.size())
    throw serialbox::Exception("inconsistent number of dimensions and strides of field '%s'"
                               "\nDimensions as: [ %s ]"
                               "\nStrides    as: [ %s ]",
                               name, ::vecToString(dims), ::vecToString(stridesVec));

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

void serialboxSerializerReadAsync(serialboxSerializer_t* serializer, const char* name,
                                  const serialboxSavepoint_t* savepoint, void* originPtr,
                                  const int* strides, int numStrides) {
  Serializer* ser = toSerializer(serializer);
  const Savepoint* sp = toConstSavepoint(savepoint);

  try {
    serialbox::StorageView storageView(
        internal::makeStorageView(ser, name, originPtr, strides, numStrides));
    ser->readAsync(name, *sp, storageView);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxSerializerWaitForAll(serialboxSerializer_t* serializer) {
  Serializer* ser = toSerializer(serializer);
  try {
    ser->waitForAll();
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Stateless Serialization
\*===------------------------------------------------------------------------------------------===*/

void serialboxWriteToFile(const char* filename, void* originPtr, int typeID, const int* dims,
                          int numDims, const int* strides, const char* fieldname,
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

void serialboxReadFromFile(const char* filename, void* originPtr, int typeID, const int* dims,
                           int numDims, const int* strides, const char* fieldname,
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
