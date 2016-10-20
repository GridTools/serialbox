/*===-- serialbox-c/FieldMetaInfo.cpp -----------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the FieldMetaInfo.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/FieldMetaInfo.h"
#include "serialbox-c/Utility.h"

using namespace serialboxC;

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

serialboxFieldMetaInfo_t* serialboxFieldMetaInfoCreate(serialboxTypeID type, const int* dimensions,
                                                       int numDimensions) {
  serialboxFieldMetaInfo_t* fieldMetaInfo = allocate<serialboxFieldMetaInfo_t>();

  std::vector<int> dims(dimensions, dimensions + numDimensions);
  try {
    fieldMetaInfo->impl = new FieldMetaInfo(serialbox::TypeID((int)type), dims);
    fieldMetaInfo->ownsData = 1;
  } catch(std::exception& e) {
    std::free(fieldMetaInfo);
    fieldMetaInfo = NULL;
    serialboxFatalError(e.what());
  }
  return fieldMetaInfo;
}

serialboxFieldMetaInfo_t*
serialboxFieldMetaInfoCreateFromFieldMetaInfo(const serialboxFieldMetaInfo_t* other) {
  const FieldMetaInfo* otherInfo = toConstFieldMetaInfo(other);
  serialboxFieldMetaInfo_t* fieldMetaInfo = allocate<serialboxFieldMetaInfo_t>();
  try {
    fieldMetaInfo->impl = new FieldMetaInfo(*otherInfo);
    fieldMetaInfo->ownsData = 1;
  } catch(std::exception& e) {
    std::free(fieldMetaInfo);
    fieldMetaInfo = NULL;
    serialboxFatalError(e.what());
  }
  return fieldMetaInfo;
}

void serialboxFieldMetaInfoDestroy(serialboxFieldMetaInfo_t* fieldMetaInfo) {
  if(fieldMetaInfo) {
    FieldMetaInfo* info = toFieldMetaInfo(fieldMetaInfo);
    if(fieldMetaInfo->ownsData)
      delete info;
    std::free(fieldMetaInfo);
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

int serialboxFieldMetaInfoEqual(const serialboxFieldMetaInfo_t* f1,
                                const serialboxFieldMetaInfo_t* f2) {
  const FieldMetaInfo* info1 = toConstFieldMetaInfo(f1);
  const FieldMetaInfo* info2 = toConstFieldMetaInfo(f2);
  return ((*info1) == (*info2));
}

char* serialboxFieldMetaInfoToString(const serialboxFieldMetaInfo_t* fieldMetaInfo) {
  const FieldMetaInfo* info = toConstFieldMetaInfo(fieldMetaInfo);
  return allocateAndCopyString(info->toString());
}

/*===------------------------------------------------------------------------------------------===*\
 *     Dimensions and TypeID
\*===------------------------------------------------------------------------------------------===*/

serialboxTypeID serialboxFieldMetaInfoGetTypeID(const serialboxFieldMetaInfo_t* fieldMetaInfo) {
  const FieldMetaInfo* info = toConstFieldMetaInfo(fieldMetaInfo);
  return serialboxTypeID((int)info->type());
}

const int* serialboxFieldMetaInfoGetDimensions(const serialboxFieldMetaInfo_t* fieldMetaInfo) {
  const FieldMetaInfo* info = toConstFieldMetaInfo(fieldMetaInfo);
  return info->dims().data();
}

int serialboxFieldMetaInfoGetNumDimensions(const serialboxFieldMetaInfo_t* fieldMetaInfo) {
  const FieldMetaInfo* info = toConstFieldMetaInfo(fieldMetaInfo);
  return (int)info->dims().size();
}

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

serialboxMetaInfo_t* serialboxFieldMetaInfoGetMetaInfo(serialboxFieldMetaInfo_t* fieldMetaInfo) {
  FieldMetaInfo* info = toFieldMetaInfo(fieldMetaInfo);
  serialboxMetaInfo_t* metaInfo = allocate<serialboxMetaInfo_t>();
  metaInfo->impl = info->metaInfoPtr().get();
  metaInfo->ownsData = 0;
  return metaInfo;
}
