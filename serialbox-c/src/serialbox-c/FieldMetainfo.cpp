/*===-- serialbox-c/FieldMetainfo.cpp -----------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the FieldMetainfoImpl.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/FieldMetainfo.h"
#include "serialbox-c/Utility.h"

using namespace serialboxC;

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

serialboxFieldMetainfo_t* serialboxFieldMetainfoCreate(serialboxTypeID type, const int* dimensions,
                                                       int numDimensions) {
  serialboxFieldMetainfo_t* fieldMetainfo = allocate<serialboxFieldMetainfo_t>();

  std::vector<int> dims(dimensions, dimensions + numDimensions);
  try {
    fieldMetainfo->impl = new FieldMetainfo(serialbox::TypeID((int)type), dims);
    fieldMetainfo->ownsData = 1;
  } catch(std::exception& e) {
    std::free(fieldMetainfo);
    fieldMetainfo = NULL;
    serialboxFatalError(e.what());
  }
  return fieldMetainfo;
}

serialboxFieldMetainfo_t*
serialboxFieldMetainfoCreateFromFieldMetainfo(const serialboxFieldMetainfo_t* other) {
  const FieldMetainfo* otherInfo = toConstFieldMetainfo(other);
  serialboxFieldMetainfo_t* fieldMetainfo = allocate<serialboxFieldMetainfo_t>();
  try {
    fieldMetainfo->impl = new FieldMetainfo(*otherInfo);
    fieldMetainfo->ownsData = 1;
  } catch(std::exception& e) {
    std::free(fieldMetainfo);
    fieldMetainfo = NULL;
    serialboxFatalError(e.what());
  }
  return fieldMetainfo;
}

void serialboxFieldMetainfoDestroy(serialboxFieldMetainfo_t* fieldMetainfo) {
  if(fieldMetainfo) {
    FieldMetainfo* info = toFieldMetainfo(fieldMetainfo);
    if(fieldMetainfo->ownsData)
      delete info;
    std::free(fieldMetainfo);
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

int serialboxFieldMetainfoEqual(const serialboxFieldMetainfo_t* f1,
                                const serialboxFieldMetainfo_t* f2) {
  const FieldMetainfo* info1 = toConstFieldMetainfo(f1);
  const FieldMetainfo* info2 = toConstFieldMetainfo(f2);
  return ((*info1) == (*info2));
}

char* serialboxFieldMetainfoToString(const serialboxFieldMetainfo_t* fieldMetainfo) {
  const FieldMetainfo* info = toConstFieldMetainfo(fieldMetainfo);
  return allocateAndCopyString(info->toString());
}

/*===------------------------------------------------------------------------------------------===*\
 *     Dimensions and TypeID
\*===------------------------------------------------------------------------------------------===*/

serialboxTypeID serialboxFieldMetainfoGetTypeID(const serialboxFieldMetainfo_t* fieldMetainfo) {
  const FieldMetainfo* info = toConstFieldMetainfo(fieldMetainfo);
  return serialboxTypeID((int)info->type());
}

const int* serialboxFieldMetainfoGetDimensions(const serialboxFieldMetainfo_t* fieldMetainfo) {
  const FieldMetainfo* info = toConstFieldMetainfo(fieldMetainfo);
  return info->dims().data();
}

int serialboxFieldMetainfoGetNumDimensions(const serialboxFieldMetainfo_t* fieldMetainfo) {
  const FieldMetainfo* info = toConstFieldMetainfo(fieldMetainfo);
  return (int)info->dims().size();
}

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

serialboxMetainfo_t* serialboxFieldMetainfoGetMetainfo(serialboxFieldMetainfo_t* fieldMetainfo) {
  FieldMetainfo* info = toFieldMetainfo(fieldMetainfo);
  serialboxMetainfo_t* metaInfo = allocate<serialboxMetainfo_t>();
  metaInfo->impl = info->metaInfoPtr().get();
  metaInfo->ownsData = 0;
  return metaInfo;
}
