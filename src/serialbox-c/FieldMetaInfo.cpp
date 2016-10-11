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

serialboxFieldMetaInfo_t serialboxFieldMetaInfoCreate(serialboxTypeID type, const int* dimensions,
                                                      int numDimensions) {

  std::vector<int> dims(dimensions, dimensions + numDimensions);
  serialboxFieldMetaInfo_t fieldMetaInfo = NULL;
  try {
    fieldMetaInfo = new FieldMetaInfo(serialbox::TypeID((int)type), dims);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return fieldMetaInfo;
}

void serialboxFieldMetaInfoDestroy(serialboxFieldMetaInfo_t* fieldMetaInfoPtr) {
  if(fieldMetaInfoPtr) {
    FieldMetaInfo* info = toFieldMetaInfo(*fieldMetaInfoPtr);
    delete info;
    *fieldMetaInfoPtr = NULL;
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

int serialboxFieldMetaInfoEqual(const serialboxFieldMetaInfo_t f1,
                                const serialboxFieldMetaInfo_t f2) {
  const FieldMetaInfo* info1 = toConstFieldMetaInfo(f1);
  const FieldMetaInfo* info2 = toConstFieldMetaInfo(f2);
  return ((*info1) == (*info2));
}

/*===------------------------------------------------------------------------------------------===*\
 *     Dimensions and TypeID
\*===------------------------------------------------------------------------------------------===*/

serialboxTypeID serialboxFieldMetaInfoGetTypeID(const serialboxFieldMetaInfo_t fieldMetaInfo) {
  const FieldMetaInfo* info = toConstFieldMetaInfo(fieldMetaInfo);
  return serialboxTypeID((int)info->type());
}

const int* serialboxFieldMetaInfoGetDimensions(const serialboxFieldMetaInfo_t fieldMetaInfo) {
  const FieldMetaInfo* info = toConstFieldMetaInfo(fieldMetaInfo);
  return info->dims().data();
}

int serialboxFieldMetaInfoGetNumDimensions(const serialboxFieldMetaInfo_t fieldMetaInfo) {
  const FieldMetaInfo* info = toConstFieldMetaInfo(fieldMetaInfo);
  return (int)info->dims().size();
}

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

serialboxMetaInfo_t serialboxFieldMetaInfoGetMetaInfo(serialboxFieldMetaInfo_t fieldMetaInfo) {
  FieldMetaInfo* info = toFieldMetaInfo(fieldMetaInfo);
  return static_cast<serialboxMetaInfo_t>(info->metaInfoPtr().get());
}
