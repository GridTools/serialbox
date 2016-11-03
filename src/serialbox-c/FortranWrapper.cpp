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
using serialbox::TypeID;

/*===------------------------------------------------------------------------------------------===*\
 *     Serializer
\*===------------------------------------------------------------------------------------------===*/

void serialboxFortranSerializerWrite(void* serializer, const void* savepoint, const char* name,
                                     void* originPtr, int istride, int jstride, int kstride,
                                     int lstride) {
  std::array<int, 4> strides{{istride, jstride, kstride, lstride}};
  serialboxSerializerWrite(static_cast<serialboxSerializer_t*>(serializer), name,
                           static_cast<const serialboxSavepoint_t*>(savepoint), originPtr,
                           strides.data(), 4);
}

void serialboxFortranSerializerRead(void* serializer, const void* savepoint, const char* name,
                                    void* originPtr, int istride, int jstride, int kstride,
                                    int lstride) {
  std::array<int, 4> strides{{istride, jstride, kstride, lstride}};
  serialboxSerializerRead(static_cast<serialboxSerializer_t*>(serializer), name,
                          static_cast<const serialboxSavepoint_t*>(savepoint), originPtr,
                          strides.data(), 4);
}

void serialboxFortranSerializerPrintDebugInfo(void* serializer) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  std::cout << ser << std::endl;
}

template <class ArrayType1, class ArrayType2>
static void checkRank(const char* name, ArrayType1&& array, ArrayType2&& arrayRef) {
  const int rank = (array[0] > 0 ? 1 : 0) + (array[1] > 0 ? 1 : 0) + (array[2] > 0 ? 1 : 0) +
                   (array[3] > 0 ? 1 : 0);

  const int refRank = (arrayRef[0] > 0 ? 1 : 0) + (arrayRef[1] > 0 ? 1 : 0) +
                      (arrayRef[2] > 0 ? 1 : 0) + (arrayRef[3] > 0 ? 1 : 0);

  if(rank != refRank)
    throw Exception("field '%s' has rank %i but field with rank %i was passed", name, refRank,
                    rank);
}

void serialboxFortranSerializerCheckField(const void* serializer, const char* name, int* type,
                                          int* isize, int* jsize, int* ksize, int* lsize) {

  const Serializer* ser = toConstSerializer(static_cast<const serialboxSerializer_t*>(serializer));

  try {
    const auto& info = ser->getFieldMetaInfoOf(name);

    if(info.dims().size() != 4)
      throw Exception("number of dimensions is %i, required are 4");

    std::array<int, 4> actualSizes{{*isize, *jsize, *ksize, *lsize}};
    const auto& refSizes = info.dims();

    // Check rank
    checkRank(name, actualSizes, refSizes);

    // Check type (be careful with converting *type as it is an arbitrary int)
    TypeID typeID = *type <= Float64 ? (TypeID)*type : TypeID::Invalid;
    if(typeID != info.type())
      throw Exception("field '%s' has type '%s' but was registrered as type '%s'", name,
                      serialbox::TypeUtil::toString(info.type()),
                      serialbox::TypeUtil::toString(typeID));

    // Reorder and check dimensions
    for(int i = 0; i < 4; ++i) {
      if(actualSizes[i] == refSizes[i])
        continue;

      if(refSizes[i] == 1) {
        for(int j = 3; j > i; --j)
          actualSizes[j] = actualSizes[j - 1];
        actualSizes[i] = 1;
        continue;
      } else
        throw Exception("dimensions of field '%s' do not match regsitered ones:"
                        "\nRegistred as: [ %i, %i, %i, %i ]"
                        "\nGiven     as: [ %i, %i, %i, %i ]",
                        name, refSizes[0], refSizes[1], refSizes[2], refSizes[3], actualSizes[0],
                        actualSizes[1], actualSizes[2], actualSizes[3]);
    }
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranComputeStrides(void* serializer, const char* fieldname, const void* basePtr,
                                    const void* iplus1, const void* jplus1, const void* kplus1,
                                    const void* lplus1, int* istride, int* jstride, int* kstride,
                                    int* lstride) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));

  try {
    const auto& info = ser->getFieldMetaInfoOf(fieldname);

    std::array<long, 4> strides{
        {reinterpret_cast<const char*>(iplus1) - reinterpret_cast<const char*>(basePtr),
         reinterpret_cast<const char*>(jplus1) - reinterpret_cast<const char*>(basePtr),
         reinterpret_cast<const char*>(kplus1) - reinterpret_cast<const char*>(basePtr),
         reinterpret_cast<const char*>(lplus1) - reinterpret_cast<const char*>(basePtr)}};

    if(info.dims().size() != 4)
      throw Exception("number of dimensions is %i, required are 4");

    const auto& dims = info.dims();

    // Check rank
    checkRank(fieldname, strides, dims);

    // Reorder strides
    for(int i = 0; i < 4; ++i) {
      if(dims[i] <= 1) {

        // Shift strides to the left and set the current one to 0
        for(int j = 3; j > i; --j)
          strides[j] = strides[j - 1];

        strides[i] = 0;
      }
    }

    // Convert to unit-strides
    const int bytesPerElement = serialbox::TypeUtil::sizeOf(info.type());
    *istride = strides[0] / bytesPerElement;
    *jstride = strides[1] / bytesPerElement;
    *kstride = strides[2] / bytesPerElement;
    *lstride = strides[3] / bytesPerElement;

  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
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
