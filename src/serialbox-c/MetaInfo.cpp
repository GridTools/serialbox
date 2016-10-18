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

serialboxMetaInfo_t* serialboxMetaInfoCreateFromMetaInfo(const serialboxMetaInfo_t* other) {
  const MetaInfoMap* otherMap = toConstMetaInfoMap(other);
  serialboxMetaInfo_t* metaInfo = allocate<serialboxMetaInfo_t>();
  try {
    metaInfo->impl = new MetaInfoMap(*otherMap);
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

int serialboxMetaInfoEqual(const serialboxMetaInfo_t* m1, const serialboxMetaInfo_t* m2) {
  const MetaInfoMap* map1 = toConstMetaInfoMap(m1);
  const MetaInfoMap* map2 = toConstMetaInfoMap(m2);
  return (*map1 == *map2);
}

int serialboxMetaInfoHasKey(const serialboxMetaInfo_t* metaInfo, const char* key) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  return (int)map->hasKey(key);
}

serialboxTypeID serialboxMetaInfoGetTypeIDOfKey(const serialboxMetaInfo_t* metaInfo,
                                                const char* key) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  serialboxTypeID id = Invalid;
  try {
    id = (serialboxTypeID)map->at(key).type();
  } catch(std::exception& e) {
    serialboxFatalError(e.what());
  }
  return id;
}

char* serialboxMetaInfoToString(const serialboxMetaInfo_t* metaInfo) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  std::stringstream ss;
  ss << *map;
  return allocateAndCopyString(ss.str());
}

serialboxMetaInfoElementInfo_t*
serialboxMetaInfoCreateElementInfo(const serialboxMetaInfo_t* metaInfo) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
  serialboxMetaInfoElementInfo_t* elements = allocate<serialboxMetaInfoElementInfo_t>();

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

void serialboxMetaInfoDestroyElementInfo(serialboxMetaInfoElementInfo_t* elementInfo) {
  if(elementInfo) {
    // keys
    for(int i = 0; i < elementInfo->len; ++i)
      std::free(elementInfo->keys[i]);
    std::free(elementInfo->keys);

    // types
    std::free(elementInfo->types);

    // element-ifno
    std::free(elementInfo);
  }
}

/*===------------------------------------------------------------------------------------------===*\
 *     Add meta-information
\*===------------------------------------------------------------------------------------------===*/

#define SERIALBOX_METAINFO_ADD_IMPL(name, serialboxPrimitveType, serialboxArrayType, CXXType)      \
  int serialboxMetaInfoAdd##name(serialboxMetaInfo_t* metaInfo, const char* key,                   \
                                 serialboxPrimitveType value) {                                    \
    MetaInfoMap* map = toMetaInfoMap(metaInfo);                                                    \
    return static_cast<int>(map->insert(key, CXXType(value)));                                     \
  }                                                                                                \
                                                                                                   \
  int serialboxMetaInfoAddArrayOf##name(serialboxMetaInfo_t* metaInfo, const char* key,            \
                                        const serialboxArrayType* array) {                         \
    MetaInfoMap* map = toMetaInfoMap(metaInfo);                                                    \
    serialbox::Array<CXXType> vec(array->data, array->data + array->len);                          \
    return static_cast<int>(map->insert(key, vec));                                                \
  }

SERIALBOX_METAINFO_ADD_IMPL(Boolean, serialboxBoolean_t, serialboxArrayOfBoolean_t, bool);
SERIALBOX_METAINFO_ADD_IMPL(Int32, serialboxInt32_t, serialboxArrayOfInt32_t, int);
SERIALBOX_METAINFO_ADD_IMPL(Int64, serialboxInt64_t, serialboxArrayOfInt64_t, std::int64_t);
SERIALBOX_METAINFO_ADD_IMPL(Float32, serialboxFloat32_t, serialboxArrayOfFloat32_t, float);
SERIALBOX_METAINFO_ADD_IMPL(Float64, serialboxFloat64_t, serialboxArrayOfFloat64_t, double);

#undef SERIALBOX_METAINFO_ADD_IMPL

int serialboxMetaInfoAddString(serialboxMetaInfo_t* metaInfo, const char* key, const char* value) {
  MetaInfoMap* map = toMetaInfoMap(metaInfo);
  return static_cast<int>(map->insert(key, std::string(value)));
}

int serialboxMetaInfoAddArrayOfString(serialboxMetaInfo_t* metaInfo, const char* key,
                                     const serialboxArrayOfString_t* array) {
  MetaInfoMap* map = toMetaInfoMap(metaInfo);
  serialbox::Array<std::string> vec(array->len);
  for(std::size_t i = 0; i < vec.size(); ++i)
    vec[i] = array->data[i];
  return static_cast<int>(map->insert(key, vec));
}

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

#define SERIALBOX_METAINFO_GET_ARRAY_IMPL(name, serialboxPrimitveType, serialboxArrayType,         \
                                          CXXType)                                                 \
  serialboxArrayType* serialboxMetaInfoGetArrayOf##name(const serialboxMetaInfo_t* metaInfo,       \
                                                        const char* key) {                         \
    const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);                                         \
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

serialboxArrayOfString_t* serialboxMetaInfoGetArrayOfString(const serialboxMetaInfo_t* metaInfo,
                                                            const char* key) {
  const MetaInfoMap* map = toConstMetaInfoMap(metaInfo);
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
