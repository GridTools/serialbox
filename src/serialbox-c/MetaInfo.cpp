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

#include "serialbox-c/MetaInfo.h"
#include "serialbox-c/Utility.h"
#include <cstdlib>

using namespace serialboxC;

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

serialboxMetaInfo_t* serialboxMetaInfoCreate(void) {
  serialboxMetaInfo_t* metaInfo = allocate<serialboxMetaInfo_t>();
  try {
    metaInfo->impl = new MetaInfoMap;
    metaInfo->ownsData = 1;
  } catch(std::exception& e) {
    std::free(metaInfo);
    metaInfo = NULL;
    serialboxFatalError(e.what());
  }
  return metaInfo;
}

void serialboxMetaInfoDestroy(serialboxMetaInfo_t* metaInfo) {
  if(metaInfo) {
    MetaInfoMap* map = toMetaInfoMap(metaInfo);
    if(metaInfo->ownsData)
      delete map;
    std::free(metaInfo);
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

int serialboxMetaInfoGetSize(const serialboxMetaInfo_t* metaInfo) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  return (int)map->size();
}

int serialboxMetaInfoIsEmpty(const serialboxMetaInfo_t* metaInfo) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  return (int)map->empty();
}

void serialboxMetaInfoClear(serialboxMetaInfo_t* metaInfo) {
  MetaInfoMap* map = toMetaInfoMap(metaInfo);
  map->clear();
}

int serialboxMetaInfoHasKey(const serialboxMetaInfo_t* metaInfo, const char* key) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  return (int)map->hasKey(key);
}

char* serialboxMetaInfoToString(const serialboxMetaInfo_t* metaInfo) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  std::stringstream ss;
  ss << *map;
  return allocateAndCopyString(ss.str());
}

void serialboxMetaInfoGetKeys(const serialboxMetaInfo_t* metaInfo, char*** keys, int* len) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);

  const auto keyVector = map->keys();

  (*len) = (int)keyVector.size();
  (*keys) = (char**)std::malloc(keyVector.size() * sizeof(char*));

  if(!(*keys))
    serialboxFatalError("out of memory");

  for(std::size_t i = 0; i < keyVector.size(); ++i)
    (*keys)[i] = allocateAndCopyString(keyVector[i]);
}

void serialboxMetaInfoGetTypes(const serialboxMetaInfo_t* metaInfo, serialboxTypeID** types,
                               int* len) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);

  const auto typesVector = map->types();

  (*len) = (int)typesVector.size();
  (*types) = (serialboxTypeID*)std::malloc(typesVector.size() * sizeof(serialboxTypeID));

  if(!(*types))
    serialboxFatalError("out of memory");

  for(std::size_t i = 0; i < typesVector.size(); ++i)
    (*types)[i] = static_cast<serialboxTypeID>((int)typesVector[i]);
}

/*===------------------------------------------------------------------------------------------===*\
 *     Add meta-information
\*===------------------------------------------------------------------------------------------===*/

#define SERIALBOX_METAINFO_ADD_IMPL(name, serialboxType, CXXType)                                  \
  int serialboxMetaInfoAdd##name(serialboxMetaInfo_t* metaInfo, const char* key,                   \
                                 serialboxType value) {                                            \
    MetaInfoMap* map = toMetaInfoMap(metaInfo);                                                    \
    return static_cast<int>(map->insert(key, CXXType(value)));                                     \
  }                                                                                                \
                                                                                                   \
  int serialboxMetaInfoAddArrayOf##name(serialboxMetaInfo_t* metaInfo, const char* key,            \
                                        serialboxType* array, int len) {                           \
    MetaInfoMap* map = toMetaInfoMap(metaInfo);                                                    \
    serialbox::Array<CXXType> vec(array, array + len);                                             \
    return static_cast<int>(map->insert(key, vec));                                                \
  }

SERIALBOX_METAINFO_ADD_IMPL(Boolean, serialboxBoolean_t, bool);
SERIALBOX_METAINFO_ADD_IMPL(Int32, serialboxInt32_t, int);
SERIALBOX_METAINFO_ADD_IMPL(Int64, serialboxInt64_t, std::int64_t);
SERIALBOX_METAINFO_ADD_IMPL(Float32, serialboxFloat32_t, float);
SERIALBOX_METAINFO_ADD_IMPL(Float64, serialboxFloat64_t, double);
SERIALBOX_METAINFO_ADD_IMPL(String, serialboxString_t, std::string);

#undef SERIALBOX_METAINFO_ADD_IMPL

/*===------------------------------------------------------------------------------------------===*\
 *     Query meta-information
\*===------------------------------------------------------------------------------------------===*/

#define SERIALBOX_METAINFO_GET_IMPL(name, serialboxType, CXXType)                                  \
  serialboxType serialboxMetaInfoGet##name(const serialboxMetaInfo_t* metaInfo, const char* key) { \
    const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);                                         \
    serialboxType value = 0;                                                                       \
    try {                                                                                          \
      value = map->as<CXXType>(key);                                                               \
    } catch(std::exception & e) {                                                                  \
      serialboxFatalError(e.what());                                                               \
    }                                                                                              \
    return value;                                                                                  \
  }

SERIALBOX_METAINFO_GET_IMPL(Boolean, serialboxBoolean_t, bool);
SERIALBOX_METAINFO_GET_IMPL(Int32, serialboxInt32_t, int);
SERIALBOX_METAINFO_GET_IMPL(Int64, serialboxInt64_t, std::int64_t);
SERIALBOX_METAINFO_GET_IMPL(Float32, serialboxFloat32_t, float);
SERIALBOX_METAINFO_GET_IMPL(Float64, serialboxFloat64_t, double);

#undef SERIALBOX_METAINFO_GET_IMPL

serialboxString_t serialboxMetaInfoGetString(const serialboxMetaInfo_t* metaInfo, const char* key) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  char* value = NULL;
  try {
    // Deep copy the string
    auto valueStr = map->as<std::string>(key);
    value = allocateAndCopyString(valueStr);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return value;
}

#define SERIALBOX_METAINFO_GET_ARRAY_IMPL(name, serialboxType, serialboxArrayType, CXXType)        \
  void serialboxMetaInfoGetArrayOf##name(const serialboxMetaInfo_t* metaInfo, const char* key,     \
                                         serialboxArrayType* array, int* len) {                    \
    const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);                                         \
    try {                                                                                          \
      auto value = map->as<serialbox::Array<CXXType>>(key);                                        \
      *len = (int)value.size();                                                                    \
      *array = (serialboxArrayType)std::malloc(sizeof(serialboxType) * value.size());              \
                                                                                                   \
      if(!(*array))                                                                                \
        serialboxFatalError("out of memory");                                                      \
                                                                                                   \
      for(std::size_t i = 0; i < value.size(); ++i)                                                \
        (*array)[i] = value[i];                                                                    \
    } catch(std::exception & e) {                                                                  \
      serialboxFatalError(e.what());                                                               \
    }                                                                                              \
  }

SERIALBOX_METAINFO_GET_ARRAY_IMPL(Boolean, serialboxBoolean_t, serialboxArrayOfBoolean_t, bool);
SERIALBOX_METAINFO_GET_ARRAY_IMPL(Int32, serialboxInt32_t, serialboxArrayOfInt32_t, int);
SERIALBOX_METAINFO_GET_ARRAY_IMPL(Int64, serialboxInt64_t, serialboxArrayOfInt64_t, std::int64_t);
SERIALBOX_METAINFO_GET_ARRAY_IMPL(Float32, serialboxFloat32_t, serialboxArrayOfFloat32_t, float);
SERIALBOX_METAINFO_GET_ARRAY_IMPL(Float64, serialboxFloat64_t, serialboxArrayOfFloat64_t, double);

void serialboxMetaInfoGetArrayOfString(const serialboxMetaInfo_t* metaInfo, const char* key,
                                       serialboxArrayOfString_t* array, int* len) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  try {
    auto value = map->as<serialbox::Array<std::string>>(key);
    *len = (int)value.size();
    *array = (serialboxArrayOfString_t)std::malloc(sizeof(serialboxString_t) * value.size());

    if(!(*array))
      serialboxFatalError("out of memory");

    for(std::size_t i = 0; i < value.size(); ++i)
      (*array)[i] = allocateAndCopyString(value[i]);

  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}
