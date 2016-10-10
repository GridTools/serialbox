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

#include "serialbox-c/ErrorHandling.h"
#include "serialbox-c/Serializer.h"
#include "serialbox/Core/Exception.h"
#include "serialbox/Core/SerializerImpl.h"
#include "serialbox/Core/Unreachable.h"

using Serializer = serialbox::SerializerImpl;

namespace internal {

static Serializer* toSerializer(serialboxSerializer_t serializer) {
  if(!serializer)
    serialboxFatalError("invalid Serializer: NULL pointer");
  return reinterpret_cast<Serializer*>(serializer);
}

} // namespace internal

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
    Serializer* ser = internal::toSerializer(*serializerPtr);
    delete ser;
    *serializerPtr = NULL;
  }
}

serialboxOpenModeKind serialboxSerializerGetMode(serialboxSerializer_t serializer) {
  Serializer* ser = internal::toSerializer(serializer);
  return static_cast<serialboxOpenModeKind>(static_cast<int>(ser->mode()));
}

const char* serialboxSerializerGetDirectory(serialboxSerializer_t serializer) {
  Serializer* ser = internal::toSerializer(serializer);
  return ser->directory().c_str();
}
