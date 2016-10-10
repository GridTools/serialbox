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

#include "serialbox-c/ErrorHandling.h"
#include "serialbox-c/FieldMetaInfo.h"
#include "serialbox/Core/FieldMetaInfo.h"

using FieldMetaInfo = serialbox::FieldMetaInfo;

namespace internal {

static FieldMetaInfo* toFieldMetaInfo(serialboxFieldMetaInfo_t fieldMetaInfo) {
  if(!fieldMetaInfo)
    serialboxFatalError("invalid FieldMetaInfo: NULL pointer");
  return reinterpret_cast<FieldMetaInfo*>(fieldMetaInfo);
}

} // namespace internal

serialboxFieldMetaInfo_t serialboxFieldMetaInfoCreate(serialboxTypeID type, int numDimensions,
                                                      int* dimensions) {

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
    FieldMetaInfo* info = internal::toFieldMetaInfo(*fieldMetaInfoPtr);
    delete info;
    *fieldMetaInfoPtr = NULL;
  }
}

serialboxTypeID serialboxFieldMetaInfoGetTypeID(serialboxFieldMetaInfo_t fieldMetaInfo) {
  FieldMetaInfo* info = internal::toFieldMetaInfo(fieldMetaInfo);
  return serialboxTypeID((int)info->type());
}

const int* serialboxFieldMetaInfoGetDimensions(serialboxFieldMetaInfo_t fieldMetaInfo) {
  FieldMetaInfo* info = internal::toFieldMetaInfo(fieldMetaInfo);
  return info->dims().data();
}

int serialboxFieldMetaInfoGetNumDimensions(serialboxFieldMetaInfo_t fieldMetaInfo) {
  FieldMetaInfo* info = internal::toFieldMetaInfo(fieldMetaInfo);
  return (int)info->dims().size();
}

serialboxMetaInfo_t serialboxFieldMetaInfoGetMetaInfo(serialboxFieldMetaInfo_t fieldMetaInfo) {
  FieldMetaInfo* info = internal::toFieldMetaInfo(fieldMetaInfo);
  return static_cast<serialboxMetaInfo_t>(info->metaInfoPtr().get());
}
