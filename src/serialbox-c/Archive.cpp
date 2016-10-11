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
#include "serialbox/Core/Archive/ArchiveFactory.h"

void serialboxArchiveGetRegisteredArchives(char*** archives, int* len) {
  const auto archiveVector = serialbox::ArchiveFactory::getInstance().registeredArchives();

  (*len) = (int)archiveVector.size();
  (*archives) = (char**)std::malloc(archiveVector.size() * sizeof(char*));

  if(!(*archives))
    serialboxFatalError("out of memory");

  for(std::size_t i = 0; i < archiveVector.size(); ++i)
    (*archives)[i] = serialboxC::allocateAndCopyString(archiveVector[i]);
}
