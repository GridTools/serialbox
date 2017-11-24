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
#include "serialbox/core/Exception.h"
#include "serialbox/core/FieldMetainfoImpl.h"
#include "serialbox/core/MetainfoMapImpl.h"
#include "serialbox/core/SavepointImpl.h"
#include "serialbox/core/SerializerImpl.h"

using namespace serialboxC;
using serialbox::Exception;
using serialbox::TypeID;

namespace {
void make_4D(std::vector<int>& v) {
  if(v.size() > 4)
    throw Exception(
        "The FortranWrapper supports up to 4 dimensions (field with %i dimensions was passed).",
        v.size());
  else
    v.resize(4, 0);
}

std::vector<int> make_strides(int istride, int jstride, int kstride, int lstride) {
  std::vector<int> strides;
  if(istride >= 0)
    strides.push_back(istride);
  if(jstride >= 0)
    strides.push_back(jstride);
  if(kstride >= 0)
    strides.push_back(kstride);
  if(lstride >= 0)
    strides.push_back(lstride);
  return strides;
}
}

/*===------------------------------------------------------------------------------------------===*\
 *     Serializer
\*===------------------------------------------------------------------------------------------===*/

void serialboxFortranSerializerWrite(void* serializer, const void* savepoint, const char* name,
                                     void* originPtr, int istride, int jstride, int kstride,
                                     int lstride) {
  auto strides = ::make_strides(istride, jstride, kstride, lstride);
  serialboxSerializerWrite(static_cast<serialboxSerializer_t*>(serializer), name,
                           static_cast<const serialboxSavepoint_t*>(savepoint), originPtr,
                           strides.data(), strides.size());
}

void serialboxFortranSerializerRead(void* serializer, const void* savepoint, const char* name,
                                    void* originPtr, int istride, int jstride, int kstride,
                                    int lstride) {
  auto strides = ::make_strides(istride, jstride, kstride, lstride);
  serialboxSerializerRead(static_cast<serialboxSerializer_t*>(serializer), name,
                          static_cast<const serialboxSavepoint_t*>(savepoint), originPtr,
                          strides.data(), strides.size());
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

  bool scalar = rank == 0 && refRank == 1 && arrayRef[0] == 1;

  if(rank != refRank && !scalar)
    throw Exception("field '%s' has rank %i but field with rank %i was passed", name, refRank,
                    rank);
}

void serialboxFortranSerializerCheckField(const void* serializer, const char* name, int* type,
                                          int* isize, int* jsize, int* ksize, int* lsize) {

  const Serializer* ser = toConstSerializer(static_cast<const serialboxSerializer_t*>(serializer));

  try {
    const auto& info = ser->getFieldMetainfoImplOf(name);

    std::array<int, 4> actualSizes{{*isize, *jsize, *ksize, *lsize}};
    auto refSizes = info.dims();
    ::make_4D(refSizes);

    // Check rank
    checkRank(name, actualSizes, refSizes);

    // Check type (be careful with converting *type as it is an arbitrary int)
    TypeID typeID = *type <= Float64 ? (TypeID)*type : TypeID::Invalid;
    if(typeID != info.type())
      throw Exception("field '%s' has type '%s' but was registered as type '%s'", name,
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
        throw Exception("dimensions of field '%s' do not match registered ones:"
                        "\nRegistered as: [ %i, %i, %i, %i ]"
                        "\nGiven      as: [ %i, %i, %i, %i ]",
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
    const auto& info = ser->getFieldMetainfoImplOf(fieldname);

    std::array<long, 4> strides{
        {reinterpret_cast<const char*>(iplus1) - reinterpret_cast<const char*>(basePtr),
         reinterpret_cast<const char*>(jplus1) - reinterpret_cast<const char*>(basePtr),
         reinterpret_cast<const char*>(kplus1) - reinterpret_cast<const char*>(basePtr),
         reinterpret_cast<const char*>(lplus1) - reinterpret_cast<const char*>(basePtr)}};

    // Reorder strides
    for(int i = 2; i >= 0; --i)
      if(strides[i] == 0)
        strides[i] = strides[i + 1];

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

void serialboxFortranSerializerGetFieldDimensions(const void* serializer, const char* name,
                                                  int* isize, int* jsize, int* ksize, int* lsize) {

  const Serializer* ser = toConstSerializer(static_cast<const serialboxSerializer_t*>(serializer));

  auto dims = ser->getFieldMetainfoImplOf(name).dims();

  ::make_4D(dims);

  *isize = dims[0];
  *jsize = dims[1];
  *ksize = dims[2];
  *lsize = dims[3];
}

void serialboxFortranSerializerGetFieldHalos(const void* serializer, const char* name,
                                             int* iMinusHalo, int* iPlusHalo, int* jMinusHalo,
                                             int* jPlusHalo, int* kMinusHalo, int* kPlusHalo,
                                             int* lMinusHalo, int* lPlusHalo) {

  char *notUsedHere_storedName, *notUsedHere_elementType;
  int notUsedHere_bytesPerElement, notUsedHere_rank;
  int notUsedHere_iSize, notUsedHere_jSize, notUsedHere_kSize, notUsedHere_lSize;

  serialboxSerializerGetFieldMetainfo2(
      static_cast<const serialboxSerializer_t*>(serializer), name, &notUsedHere_storedName,
      &notUsedHere_elementType, &notUsedHere_bytesPerElement, &notUsedHere_rank, &notUsedHere_iSize,
      &notUsedHere_jSize, &notUsedHere_kSize, &notUsedHere_lSize, iMinusHalo, iPlusHalo, jMinusHalo,
      jPlusHalo, kMinusHalo, kPlusHalo, lMinusHalo, lPlusHalo);
}

void serialboxFortranSerializerAddMetainfoBoolean(void* serializer, const char* key, int value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetainfo(key, (bool)value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddMetainfoInt32(void* serializer, const char* key, int value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetainfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddMetainfoFloat32(void* serializer, const char* key, float value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetainfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddMetainfoFloat64(void* serializer, const char* key, double value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetainfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddMetainfoString(void* serializer, const char* key,
                                                 const char* value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    ser->addGlobalMetainfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerRegisterField(void* serializer, const char* name, int type,
                                             int bytesPerElement, int iSize, int jSize, int kSize,
                                             int lSize, int iMinusHalo, int iPlusHalo,
                                             int jMinusHalo, int jPlusHalo, int kMinusHalo,
                                             int kPlusHalo, int lMinusHalo, int lPlusHalo) {
  serialboxSerializerAddField2(static_cast<serialboxSerializer_t*>(serializer), name, type,
                               bytesPerElement, iSize, jSize, kSize, lSize, iMinusHalo, iPlusHalo,
                               jMinusHalo, jPlusHalo, kMinusHalo, kPlusHalo, lMinusHalo, lPlusHalo);
}

/*===------------------------------------------------------------------------------------------===*\
 *     FieldMetainfoImpl
\*===------------------------------------------------------------------------------------------===*/

void serialboxFortranSerializerAddFieldMetainfoBoolean(void* serializer, const char* field,
                                                       const char* key, int value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetainfoImpl(field, key, (bool)value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddFieldMetainfoInt32(void* serializer, const char* field,
                                                     const char* key, int value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetainfoImpl(field, key, value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddFieldMetainfoFloat32(void* serializer, const char* field,
                                                       const char* key, float value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetainfoImpl(field, key, value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddFieldMetainfoFloat64(void* serializer, const char* field,
                                                       const char* key, double value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetainfoImpl(field, key, value))
      throw Exception(
          "cannot add element with key '%s' to field meta-info of '%s': element already exists",
          key, field);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSerializerAddFieldMetainfoString(void* serializer, const char* field,
                                                      const char* key, const char* value) {
  Serializer* ser = toSerializer(static_cast<serialboxSerializer_t*>(serializer));
  try {
    if(!ser->addFieldMetainfoImpl(field, key, value))
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

void serialboxFortranSavepointAddMetainfoBoolean(void* savepoint, const char* key, int value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetainfo(key, (bool)value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSavepointAddMetainfoInt32(void* savepoint, const char* key, int value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetainfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSavepointAddMetainfoFloat32(void* savepoint, const char* key, float value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetainfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSavepointAddMetainfoFloat64(void* savepoint, const char* key, double value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetainfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}

void serialboxFortranSavepointAddMetainfoString(void* savepoint, const char* key,
                                                const char* value) {
  Savepoint* sp = toSavepoint(static_cast<serialboxSavepoint_t*>(savepoint));
  try {
    sp->addMetainfo(key, value);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}
