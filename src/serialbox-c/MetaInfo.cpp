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

#include "serialbox-c/ErrorHandling.h"
#include "serialbox-c/MetaInfo.h"
#include "serialbox/Core/MetaInfoMap.h"
#include <cstdlib>

using MetaInfoMap = serialbox::MetaInfoMap;

namespace internal {

static MetaInfoMap* toMetaInfoMap(serialboxMetaInfo_t metaInfo) {
  if(!metaInfo)
    serialboxFatalError("invalid MetaInfo: NULL pointer");
  return reinterpret_cast<MetaInfoMap*>(metaInfo);
}

} // namespace internal

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

serialboxMetaInfo_t serialboxMetaInfoCreate() {
  MetaInfoMap* map = NULL;
  try {
    map = new MetaInfoMap;
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }

  return static_cast<serialboxMetaInfo_t>(map);
}

void serialboxMetaInfoDestroy(serialboxMetaInfo_t* metaInfoPtr) {
  if(metaInfoPtr) {
    MetaInfoMap* map = internal::toMetaInfoMap(*metaInfoPtr);
    delete map;
    *metaInfoPtr = NULL;
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

int serialboxMetaInfoGetSize(serialboxMetaInfo_t metaInfo) {
  MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);
  return (int)map->size();
}

int serialboxMetaInfoIsEmpty(serialboxMetaInfo_t metaInfo) {
  MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);
  return (int)map->empty();
}

void serialboxMetaInfoClear(serialboxMetaInfo_t metaInfo) {
  MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);
  map->clear();
}

int serialboxMetaInfoHasKey(serialboxMetaInfo_t metaInfo, const char* key) {
  MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);
  return (int)map->hasKey(key);
}

const char* serialboxMetaInfoToString(serialboxMetaInfo_t metaInfo) {
  MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);
  std::stringstream ss;
  ss << *map;
  std::string str(ss.str());
  char* buffer = NULL;
  try {
    buffer = new char[str.size() + 1];
    std::memcpy(buffer, str.c_str(), str.size() + 1);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return buffer;
}

/*===------------------------------------------------------------------------------------------===*\
 *     Add meta-information
\*===------------------------------------------------------------------------------------------===*/

#define SERIALBOX_METAINFO_ADD_IMPL(name, serialboxType, CXXType)                                  \
  int serialboxMetaInfoAdd##name(serialboxMetaInfo_t metaInfo, const char* key,                    \
                                 serialboxType value) {                                            \
    MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);                                          \
    return static_cast<int>(map->insert(key, CXXType(value)));                                     \
  }                                                                                                \
                                                                                                   \
  int serialboxMetaInfoAddArrayOf##name(serialboxMetaInfo_t metaInfo, const char* key,             \
                                        serialboxType* array, int len) {                           \
    MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);                                          \
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
  serialboxType serialboxMetaInfoGet##name(serialboxMetaInfo_t metaInfo, const char* key) {        \
    MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);                                          \
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

serialboxString_t serialboxMetaInfoGetString(serialboxMetaInfo_t metaInfo, const char* key) {
  MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);
  char* value = NULL;
  try {
    auto valueStr = map->as<std::string>(key);
    value = new char[valueStr.size() + 1];
    std::memcpy(value, valueStr.c_str(), valueStr.size() + 1);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return value;
}

void serialboxMetaInfoGetArrayOfBoolean(serialboxMetaInfo_t metaInfo, const char* key,
                                        serialboxArrayOfBoolean_t* array, int* len) {
  MetaInfoMap* map = internal::toMetaInfoMap(metaInfo);
  try {
    auto value = map->as<serialbox::Array<bool>>(key);
    *len = (int)value.size();
    *array = (serialboxArrayOfBoolean_t)std::malloc(sizeof(serialboxBoolean_t) * value.size());
    for(std::size_t i = 0; i < value.size(); ++i)
      (*array)[i] = value[i];
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
}
