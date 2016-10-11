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

serialboxSavepoint_t serialboxSavepointCreate(const char* name) {
  serialboxSavepoint_t savepoint = NULL;
  try {
    savepoint = new Savepoint(name);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return savepoint;
}

serialboxSavepoint_t serialboxSavepointCreateFromSavepoint(const serialboxSavepoint_t other) {
  const Savepoint* sp = toConstSavepoint(other);
  serialboxSavepoint_t savepoint = NULL;
  try {
    savepoint = new Savepoint(*sp);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return savepoint;
}

void serialboxSavepointDestroy(serialboxSavepoint_t* savepointPtr) {
  if(savepointPtr) {
    Savepoint* sp = toSavepoint(*savepointPtr);
    delete sp;
    *savepointPtr = NULL;
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

const char* serialboxSavepointGetName(const serialboxSavepoint_t savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  return sp->name().c_str();
}

int serialboxSavepointEqual(const serialboxSavepoint_t s1, const serialboxSavepoint_t s2) {
  const Savepoint* sp1 = toConstSavepoint(s1);
  const Savepoint* sp2 = toConstSavepoint(s2);
  return ((*sp1) == (*sp2));
}

char* serialboxSavepointToString(const serialboxSavepoint_t savepoint) {
  const Savepoint* sp = toConstSavepoint(savepoint);
  std::string str(sp->toString());
  char* buffer = NULL;
  buffer = (char*)std::malloc((str.size() + 1) * sizeof(char));

  if(!buffer)
    serialboxFatalError("out of memory");

  std::memcpy(buffer, str.c_str(), str.size() + 1);

  return buffer;
}

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

serialboxMetaInfo_t serialboxSavepointGetMetaInfo(serialboxSavepoint_t savepoint) {
  Savepoint* sp = toSavepoint(savepoint);
  return static_cast<serialboxMetaInfo_t>(sp->metaInfoPtr().get());
}
