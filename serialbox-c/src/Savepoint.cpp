/*===-- serialbox-c/Savepoint.cpp ---------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the Savepoint.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Savepoint.h"
#include "serialbox-c/Utility.h"

using namespace serialboxC;

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

serialboxSavepoint_t* serialboxSavepointCreate(const char* name) {
  serialboxSavepoint_t* savepoint = allocate<serialboxSavepoint_t>();
  try {
    savepoint->impl = new Savepoint(name);
    savepoint->ownsData = 1;
  } catch(std::exception& e) {
    std::free(savepoint);
    savepoint = NULL;
    serialboxFatalError(e.what());
  }
  return savepoint;
}

serialboxSavepoint_t* serialboxSavepointCreateFromSavepoint(const serialboxSavepoint_t* other) {
  const Savepoint* sp = toConstSavepoint(other);
  serialboxSavepoint_t* savepoint = allocate<serialboxSavepoint_t>();
  try {
    savepoint->impl = new Savepoint(*sp);
    savepoint->ownsData = 1;
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return savepoint;
}

void serialboxSavepointDestroy(serialboxSavepoint_t* savepoint) {
  if(savepoint) {
    Savepoint* sp = toSavepoint(savepoint);
    if(savepoint->ownsData)
      delete sp;
    std::free(savepoint);
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

const char* serialboxSavepointGetName(const serialboxSavepoint_t* savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  return sp->name().c_str();
}

int serialboxSavepointEqual(const serialboxSavepoint_t* s1, const serialboxSavepoint_t* s2) {
  const Savepoint* sp1 = toConstSavepoint(s1);
  const Savepoint* sp2 = toConstSavepoint(s2);
  return ((*sp1) == (*sp2));
}

char* serialboxSavepointToString(const serialboxSavepoint_t* savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  return allocateAndCopyString(sp->toString());
}

size_t serialboxSavepointHash(const serialboxSavepoint_t* savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  return std::hash<Savepoint>()(*sp);
}

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

serialboxMetainfo_t* serialboxSavepointGetMetainfo(const serialboxSavepoint_t* savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  serialboxMetainfo_t* metaInfo = allocate<serialboxMetainfo_t>();
  metaInfo->impl = sp->metaInfoPtr().get();
  metaInfo->ownsData = 0;
  return metaInfo;
}
