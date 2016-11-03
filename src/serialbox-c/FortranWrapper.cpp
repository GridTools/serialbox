/*===-- serialbox-c/FortranWrapper.cpp ----------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains some abstractions for the Fortran interface.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/FortranWrapper.h"
#include "serialbox-c/Serializer.h"
#include "serialbox-c/Utility.h"
#include "serialbox/Core/Exception.h"
#include "serialbox/Core/FieldMetaInfo.h"
#include "serialbox/Core/MetaInfoMap.h"
#include "serialbox/Core/SavepointImpl.h"
#include "serialbox/Core/SerializerImpl.h"

using namespace serialboxC;
using serialbox::Exception;

/*===------------------------------------------------------------------------------------------===*\
 *     Serializer
\*===------------------------------------------------------------------------------------------===*/

void serialboxFortranSerializerWrite(void* serializer, const void* savepoint,
                                    const char* name, void* originPtr,
                                    int istride, int jstride, int kstride, int lstride) {

}


void serialboxFortranSerializerRead(void* serializer, const void* savepoint,
                                    const char* name, void* originPtr,
                                    int istride, int jstride, int kstride, int lstrides) {

}

void serialboxFortranSerializerPrint(void* serializer) {
 // Debug info
}

void serialboxFortranSerializerFieldExists(void* serializer, const char* name) {

}

int serialboxFortranSerializerCheckField(void* serializer, const char* name, int type,
                                          int istride, int jstride, int kstride, int lstride) {

}

void serialboxFortranComputeStrides(void* serializer, const char* fieldname,
                                    const void* base_ptr,
                                    const void* iplus1, const void* jplus1, const void* kplus1, const void* lplus1,
                                    int* istride, int* jstride, int* kstride, int* lstride) {

}


void serialboxFortranSerializerAddMetaInfoBoolean(void* serializer, const char* key, int value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetaInfo(key, (bool)value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddMetaInfoInt32(void* serializer, const char* key, int value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetaInfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddMetaInfoFloat32(void* serializer, const char* key, float value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetaInfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddMetaInfoFloat64(void* serializer, const char* key, double value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetaInfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddMetaInfoString(void* serializer, const char* key,
                                                 const char* value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetaInfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerRegisterField(void* serializer, const char* name, int type,
                                             int bytesPerElement, int iSize, int jSize, int kSize,
                                             int lSize) {
  serialboxSerializerAddField2(static_cast<serialboxSerializer_t*>(serializer), name, type,
                               bytesPerElement, iSize, jSize, kSize, lSize, 0, 0, 0, 0, 0, 0, 0, 0);
}

/*===------------------------------------------------------------------------------------------===*\
 *     FieldMetaInfo
\*===------------------------------------------------------------------------------------------===*/

void serialboxFortranSerializerAddFieldMetaInfoBoolean(void* serializer, const char* field,
                                                       const char* key, int value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetaInfo(field, key, (bool)value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddFieldMetaInfoInt32(void* serializer, const char* field,
                                                     const char* key, int value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetaInfo(field, key, value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddFieldMetaInfoFloat32(void* serializer, const char* field,
                                                       const char* key, float value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetaInfo(field, key, value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddFieldMetaInfoFloat64(void* serializer, const char* field,
                                                       const char* key, double value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetaInfo(field, key, value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddFieldMetaInfoString(void* serializer, const char* field,
                                                      const char* key, const char* value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetaInfo(field, key, value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Savepoint
\*===------------------------------------------------------------------------------------------===*/

void serialboxFortranSavepointAddMetaInfoBoolean(void* savepoint, const char* key, int value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetaInfo(key, (bool)value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSavepointAddMetaInfoInt32(void* savepoint, const char* key, int value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetaInfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSavepointAddMetaInfoFloat32(void* savepoint, const char* key, float value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetaInfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSavepointAddMetaInfoFloat64(void* savepoint, const char* key, double value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetaInfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSavepointAddMetaInfoString(void* savepoint, const char* key,
                                                const char* value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetaInfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}
