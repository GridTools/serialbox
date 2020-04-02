/*===-- serialbox-c/Archive.cpp -----------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C Interface to the ArchiveFactory.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Archive.h"
#include "serialbox-c/Utility.h"
#include "serialbox/core/archive/ArchiveFactory.h"

using namespace serialboxC;

serialboxArrayOfString_t* serialboxArchiveGetRegisteredArchives(void) {
  serialboxArrayOfString_t* array = NULL;
  try {
    const auto archiveVector = serialbox::ArchiveFactory::registeredArchives();

    array = allocate<serialboxArrayOfString_t>();
    array->len = (int)archiveVector.size();
    array->data = allocate<serialboxString_t>(array->len * sizeof(serialboxString_t));

    for(std::size_t i = 0; i < archiveVector.size(); ++i)
      array->data[i] = allocateAndCopyString(archiveVector[i]);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return array;
}

char* serialboxArchiveGetArchiveFromExtension(const char* filename) {
  char* archive = NULL;
  try {
    archive = allocateAndCopyString(serialbox::ArchiveFactory::archiveFromExtension(filename));
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return archive;
}
