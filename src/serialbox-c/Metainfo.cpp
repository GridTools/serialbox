/*===-- serialbox-c/Metainfo.cpp ----------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the Metainfo.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Metainfo.h"
#include "serialbox-c/Utility.h"
#include <cstdlib>

using namespace serialboxC;

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

serialboxMetainfo_t* serialboxMetainfoCreate(void) {
  serialboxMetainfo_t* metaInfo = allocate<serialboxMetainfo_t>();
  try {
    metaInfo->impl = new MetainfoMap;
    metaInfo->ownsData = 1;
  } catch(std::exception& e) {
    std::free(metaInfo);
    metaInfo = NULL;
    serialboxFatalError(e.what());
  }
  return metaInfo;
}

serialboxMetainfo_t* serialboxMetainfoCreateFromMetainfo(const serialboxMetainfo_t* other) {
  const MetainfoMap* otherMap = toConstMetainfoMap(other);
  serialboxMetainfo_t* metaInfo = allocate<serialboxMetainfo_t>();
  try {
    metaInfo->impl = new MetainfoMap(*otherMap);
    metaInfo->ownsData = 1;
  } catch(std::exception& e) {
    std::free(metaInfo);
    metaInfo = NULL;
    serialboxFatalError(e.what());
  }
  return metaInfo;
}

void serialboxMetainfoDestroy(serialboxMetainfo_t* metaInfo) {
  if(metaInfo) {
    MetainfoMap* map = toMetainfoMap(metaInfo);
    if(metaInfo->ownsData)
      delete map;
    std::free(metaInfo);
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

int serialboxMetainfoGetSize(const serialboxMetainfo_t* metaInfo) {
  const MetainfoMap* map = toConstMetainfoMap(metaInfo);
  return (int)map->size();
}

int serialboxMetainfoIsEmpty(const serialboxMetainfo_t* metaInfo) {
  const MetainfoMap* map = toConstMetainfoMap(metaInfo);
  return (int)map->empty();
}

void serialboxMetainfoClear(serialboxMetainfo_t* metaInfo) {
  MetainfoMap* map = toMetainfoMap(metaInfo);
  map->clear();
}

int serialboxMetainfoEqual(const serialboxMetainfo_t* m1, const serialboxMetainfo_t* m2) {
  const MetainfoMap* map1 = toConstMetainfoMap(m1);
  const MetainfoMap* map2 = toConstMetainfoMap(m2);
  return (*map1 == *map2);
}

int serialboxMetainfoHasKey(const serialboxMetainfo_t* metaInfo, const char* key) {
  const MetainfoMap* map = toConstMetainfoMap(metaInfo);
  return (int)map->hasKey(key);
}

serialboxTypeID serialboxMetainfoGetTypeIDOfKey(const serialboxMetainfo_t* metaInfo,
                                                const char* key) {
  const MetainfoMap* map = toConstMetainfoMap(metaInfo);
  serialboxTypeID id = Invalid;
  try {
    id = (serialboxTypeID)map->at(key).type();
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return id;
}

char* serialboxMetainfoToString(const serialboxMetainfo_t* metaInfo) {
  const MetainfoMap* map = toConstMetainfoMap(metaInfo);
  std::stringstream ss;
  ss << *map;
  return allocateAndCopyString(ss.str());
}

serialboxMetainfoElementInfo_t*
serialboxMetainfoCreateElementInfo(const serialboxMetainfo_t* metaInfo) {
  const MetainfoMap* map = toConstMetainfoMap(metaInfo);
  serialboxMetainfoElementInfo_t* elements = allocate<serialboxMetainfoElementInfo_t>();

  // keys
  const auto keyVector = map->keys();
  elements->keys = (char**)std::malloc(keyVector.size() * sizeof(char*));
  for(std::size_t i = 0; i < keyVector.size(); ++i)
    elements->keys[i] = allocateAndCopyString(keyVector[i]);

  // types
  const auto typeVector = map->types();
  elements->types = (int*)std::malloc(typeVector.size() * sizeof(int));
  for(std::size_t i = 0; i < typeVector.size(); ++i)
    elements->types[i] = (int)typeVector[i];

  // len
  elements->len = (int)map->size();

  return elements;
}

void serialboxMetainfoDestroyElementInfo(serialboxMetainfoElementInfo_t* elementInfo) {
  if(elementInfo) {
    // keys
    for(int i = 0; i < elementInfo->len; ++i)
      std::free(elementInfo->keys[i]);
    std::free(elementInfo->keys);

    // types
    std::free(elementInfo->types);

    // element-info
    std::free(elementInfo);
  }
}

SERIALBOX_API int serialboxMetainfoDeleteKey(serialboxMetainfo_t* metaInfo, const char* key) {
  MetainfoMap* map = toMetainfoMap(metaInfo);
  return map->erase(key);
}

/*===------------------------------------------------------------------------------------------===*\
 *     Add meta-information
\*===------------------------------------------------------------------------------------------===*/

#define SERIALBOX_METAINFO_ADD_IMPL(name, serialboxPrimitveType, serialboxArrayType, CXXType)      \
  int serialboxMetainfoAdd##name(serialboxMetainfo_t* metaInfo, const char* key,                   \
                                 serialboxPrimitveType value) {                                    \
    MetainfoMap* map = toMetainfoMap(metaInfo);                                                    \
    return static_cast<int>(map->insert(key, CXXType(value)));                                     \
  }                                                                                                \
                                                                                                   \
  int serialboxMetainfoAddArrayOf##name(serialboxMetainfo_t* metaInfo, const char* key,            \
                                        const serialboxArrayType* array) {                         \
    MetainfoMap* map = toMetainfoMap(metaInfo);                                                    \
    serialbox::Array<CXXType> vec(array->data, array->data + array->len);                          \
    return static_cast<int>(map->insert(key, vec));                                                \
  }

SERIALBOX_METAINFO_ADD_IMPL(Boolean, serialboxBoolean_t, serialboxArrayOfBoolean_t, bool);
SERIALBOX_METAINFO_ADD_IMPL(Int32, serialboxInt32_t, serialboxArrayOfInt32_t, int);
SERIALBOX_METAINFO_ADD_IMPL(Int64, serialboxInt64_t, serialboxArrayOfInt64_t, std::int64_t);
SERIALBOX_METAINFO_ADD_IMPL(Float32, serialboxFloat32_t, serialboxArrayOfFloat32_t, float);
SERIALBOX_METAINFO_ADD_IMPL(Float64, serialboxFloat64_t, serialboxArrayOfFloat64_t, double);

#undef SERIALBOX_METAINFO_ADD_IMPL

int serialboxMetainfoAddString(serialboxMetainfo_t* metaInfo, const char* key, const char* value) {
  MetainfoMap* map = toMetainfoMap(metaInfo);
  return static_cast<int>(map->insert(key, std::string(value)));
}

int serialboxMetainfoAddArrayOfString(serialboxMetainfo_t* metaInfo, const char* key,
                                      const serialboxArrayOfString_t* array) {
  MetainfoMap* map = toMetainfoMap(metaInfo);
  serialbox::Array<std::string> vec(array->len);
  for(std::size_t i = 0; i < vec.size(); ++i)
    vec[i] = array->data[i];
  return static_cast<int>(map->insert(key, vec));
}

/*===------------------------------------------------------------------------------------------===*\
 *     Query meta-information
\*===------------------------------------------------------------------------------------------===*/

#define SERIALBOX_METAINFO_GET_IMPL(name, serialboxType, CXXType)                                  \
  serialboxType serialboxMetainfoGet##name(const serialboxMetainfo_t* metaInfo, const char* key) { \
    const MetainfoMap* map = toConstMetainfoMap(metaInfo);                                         \
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

serialboxString_t serialboxMetainfoGetString(const serialboxMetainfo_t* metaInfo, const char* key) {
  const MetainfoMap* map = toConstMetainfoMap(metaInfo);
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

#define SERIALBOX_METAINFO_GET_ARRAY_IMPL(name, serialboxPrimitveType, serialboxArrayType,         \
                                          CXXType)                                                 \
  serialboxArrayType* serialboxMetainfoGetArrayOf##name(const serialboxMetainfo_t* metaInfo,       \
                                                        const char* key) {                         \
    const MetainfoMap* map = toConstMetainfoMap(metaInfo);                                         \
    serialboxArrayType* array = NULL;                                                              \
    try {                                                                                          \
      auto value = map->as<serialbox::Array<CXXType>>(key);                                        \
      array = allocate<serialboxArrayType>();                                                      \
      array->len = (int)value.size();                                                              \
      array->data = allocate<serialboxPrimitveType>(array->len * sizeof(serialboxPrimitveType));   \
                                                                                                   \
      for(std::size_t i = 0; i < value.size(); ++i)                                                \
        array->data[i] = value[i];                                                                 \
    } catch(std::exception & e) {                                                                  \
      serialboxFatalError(e.what());                                                               \
    }                                                                                              \
    return array;                                                                                  \
  }

SERIALBOX_METAINFO_GET_ARRAY_IMPL(Boolean, serialboxBoolean_t, serialboxArrayOfBoolean_t, bool);
SERIALBOX_METAINFO_GET_ARRAY_IMPL(Int32, serialboxInt32_t, serialboxArrayOfInt32_t, int);
SERIALBOX_METAINFO_GET_ARRAY_IMPL(Int64, serialboxInt64_t, serialboxArrayOfInt64_t, std::int64_t);
SERIALBOX_METAINFO_GET_ARRAY_IMPL(Float32, serialboxFloat32_t, serialboxArrayOfFloat32_t, float);
SERIALBOX_METAINFO_GET_ARRAY_IMPL(Float64, serialboxFloat64_t, serialboxArrayOfFloat64_t, double);

serialboxArrayOfString_t* serialboxMetainfoGetArrayOfString(const serialboxMetainfo_t* metaInfo,
                                                            const char* key) {
  const MetainfoMap* map = toConstMetainfoMap(metaInfo);
  serialboxArrayOfString_t* array = NULL;
  try {
    auto value = map->as<serialbox::Array<std::string>>(key);
    array = allocate<serialboxArrayOfString_t>();
    array->len = (int)value.size();
    array->data = allocate<serialboxString_t>(array->len * sizeof(serialboxString_t));

    for(std::size_t i = 0; i < value.size(); ++i)
      array->data[i] = allocateAndCopyString(value[i]);
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return array;
}

#undef SERIALBOX_METAINFO_GET_ARRAY_IMPL
