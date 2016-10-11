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
#include "serialbox/Core/Exception.h"
#include "serialbox/Core/Logging.h"
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

serialboxSerializer_t serialboxSerializerCreate(serialboxOpenModeKind mode, const char* directory,
                                                const char* prefix, const char* archive) {
  Serializer* serializer = NULL;
  try {
    switch(mode) {
    case Read:
      serializer = new Serializer(serialbox::OpenModeKind::Read, directory, prefix, archive);
      break;
    case Write:
      serializer = new Serializer(serialbox::OpenModeKind::Write, directory, prefix, archive);
      break;
    case Append:
      serializer = new Serializer(serialbox::OpenModeKind::Append, directory, prefix, archive);
      break;
    }
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return static_cast<serialboxSerializer_t>(serializer);
}

void serialboxSerializerDestroy(serialboxSerializer_t* serializerPtr) {
  if(serializerPtr) {
    Serializer* ser = toSerializer(*serializerPtr);
    delete ser;
    *serializerPtr = NULL;
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

serialboxOpenModeKind serialboxSerializerGetMode(const serialboxSerializer_t serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return static_cast<serialboxOpenModeKind>(static_cast<int>(ser->mode()));
}

const char* serialboxSerializerGetDirectory(const serialboxSerializer_t serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return ser->directory().c_str();
}

const char* serialboxSerializerGetPrefix(const serialboxSerializer_t serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return ser->prefix().c_str();
}

void serialboxSerializerUpdateMetaData(serialboxSerializer_t serializer) {
  Serializer* ser = toSerializer(serializer);
  try {
    ser->updateMetaData();
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Global Meta-information
\*===------------------------------------------------------------------------------------------===*/

serialboxMetaInfo_t serialboxSerializerGetGlobalMetaInfo(serialboxSerializer_t serializer) {
  Serializer* ser = toSerializer(serializer);
  return static_cast<serialboxMetaInfo_t>(ser->globalMetaInfoPtr().get());
}

/*===------------------------------------------------------------------------------------------===*\
 *     Register and Query Savepoints
\*===------------------------------------------------------------------------------------------===*/

int serialboxSerializerAddSavepoint(serialboxSerializer_t serializer,
                                    const serialboxSavepoint_t savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  Serializer* ser = toSerializer(serializer);
  return ser->registerSavepoint(*sp);
}

int serialboxSerializerGetNumSavepoints(const serialboxSerializer_t serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  return (int)ser->savepointVector().size();
}

serialboxSavepoint_t*
serialboxSerializerGetSavepointVector(const serialboxSerializer_t serializer) {
  const Serializer* ser = toConstSerializer(serializer);
  const auto& savepointVector = ser->savepointVector().savepoints();

  serialboxSavepoint_t* savepoints =
      (serialboxSavepoint_t*)std::malloc(sizeof(serialboxSavepoint_t) * savepointVector.size());

  if(!savepoints)
    serialboxFatalError("out of memory");

  for(std::size_t i = 0; i < savepointVector.size(); ++i)
    savepoints[i] = static_cast<serialboxSavepoint_t>(savepointVector[i].get());

  return savepoints;
}

int serialboxSerializationEnabled = 1;

void serialboxEnableSerialization(void) { serialboxSerializationEnabled = 1; }

void serialboxDisableSerialization(void) { serialboxSerializationEnabled = 0; }

/*===------------------------------------------------------------------------------------------===*\
 *     Register and Query Fields
\*===------------------------------------------------------------------------------------------===*/

int serialboxSerializerAddField(serialboxSerializer_t serializer, const char* name,
                                const serialboxFieldMetaInfo_t fieldMetaInfo) {
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

void serialboxSerializerGetFieldnames(const serialboxSerializer_t serializer, char*** fieldnames,
                                      int* len) {
  const Serializer* ser = toConstSerializer(serializer);

  const auto fieldnameVector = ser->fieldnames();

  (*len) = (int)fieldnameVector.size();
  (*fieldnames) = (char**)std::malloc(fieldnameVector.size() * sizeof(char*));

  if(!(*fieldnames))
    serialboxFatalError("out of memory");

  for(std::size_t i = 0; i < fieldnameVector.size(); ++i)
    (*fieldnames)[i] = allocateAndCopyString(fieldnameVector[i]);
}

serialboxFieldMetaInfo_t serialboxSerializerGetFieldMetaInfo(const serialboxSerializer_t serializer,
                                                             const char* name) {
  const Serializer* ser = toConstSerializer(serializer);

  auto it = ser->fieldMap().findField(name);
  if(it != ser->fieldMap().end())
    return static_cast<serialboxFieldMetaInfo_t>(it->second.get());
  return NULL;
}

void serialboxSerializerGetFieldnamesAtSavepoint(const serialboxSerializer_t serializer,
                                                 const serialboxSavepoint_t savepoint,
                                                 char*** fieldnames, int* len) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  const Serializer* ser = toConstSerializer(serializer);

  try {
    const auto& fieldnameMap = ser->savepointVector().fieldsOf(*sp);

    (*len) = (int)fieldnameMap.size();
    (*fieldnames) = (char**)std::malloc(fieldnameMap.size() * sizeof(char*));

    if(!(*fieldnames))
      serialboxFatalError("out of memory");

    int i = 0;
    for(auto it = fieldnameMap.begin(), end = fieldnameMap.end(); it != end; ++it, ++i)
      (*fieldnames)[i] = allocateAndCopyString(it->first);

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

void serialboxSerializerWrite(serialboxSerializer_t serializer, const char* name,
                              const serialboxSavepoint_t savepoint, void* originPtr,
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

void serialboxSerializerRead(serialboxSerializer_t serializer, const char* name,
                             const serialboxSavepoint_t savepoint, void* originPtr,
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
