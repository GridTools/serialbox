/*==-- serialbox-c/Utility.h -------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 **
 * \file
 * Provides helper functions for the C-Interface implementation.
 *
 * This file contains C++ and should therefore not be included in exposed C headers!
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_UTILITY_H
#define SERIALBOX_C_UTILITY_H

#ifdef __cplusplus

#include "serialbox-c/ErrorHandling.h"
#include "serialbox-c/Type.h"
#include "serialbox/Core/FieldMetaInfo.h"
#include "serialbox/Core/MetaInfoMap.h"
#include "serialbox/Core/SavepointImpl.h"
#include "serialbox/Core/SerializerImpl.h"
#include <cstdlib>
#include <cstring>

namespace serialboxC {

using Serializer = serialbox::SerializerImpl;
using FieldMetaInfo = serialbox::FieldMetaInfo;
using Savepoint = serialbox::SavepointImpl;
using MetaInfoMap = serialbox::MetaInfoMap;

/// \brief Convert `serialboxSerializer_t` to `Serializer`
/// @{
inline Serializer* toSerializer(serialboxSerializer_t* serializer) {
  if(!serializer->impl)
    serialboxFatalError("uninitialized Serializer");
  return reinterpret_cast<Serializer*>(serializer->impl);
}

inline const Serializer* toConstSerializer(const serialboxSerializer_t* serializer) {
  if(!serializer->impl)
    serialboxFatalError("uninitialized Serializer");
  return reinterpret_cast<const Serializer*>(serializer->impl);
}
/// @}

/// \brief Convert `serialboxSavepoint_t` to `Savepoint`
/// @{
inline Savepoint* toSavepoint(serialboxSavepoint_t* savepoint) {
  if(!savepoint->impl)
    serialboxFatalError("uninitialized Savepoint");
  return reinterpret_cast<Savepoint*>(savepoint->impl);
}

inline const Savepoint* toConstSavepoint(const serialboxSavepoint_t* savepoint) {
  if(!savepoint->impl)
    serialboxFatalError("uninitialized Savepoint");
  return reinterpret_cast<const Savepoint*>(savepoint->impl);
}
/// @}

/// \brief Convert `serialboxFieldMetaInfo_t` to `FieldMetaInfo`
/// @{
inline FieldMetaInfo* toFieldMetaInfo(serialboxFieldMetaInfo_t* fieldMetaInfo) {
  if(!fieldMetaInfo->impl)
    serialboxFatalError("uninitialized FieldMetaInfo");
  return reinterpret_cast<FieldMetaInfo*>(fieldMetaInfo->impl);
}

inline const FieldMetaInfo* toConstFieldMetaInfo(const serialboxFieldMetaInfo_t* fieldMetaInfo) {
  if(!fieldMetaInfo->impl)
    serialboxFatalError("uninitialized FieldMetaInfo");
  return reinterpret_cast<const FieldMetaInfo*>(fieldMetaInfo->impl);
}
/// @}

/// \brief Convert `serialboxMetaInfo_t` to `MetaInfoMap`
/// @{
inline MetaInfoMap* toMetaInfoMap(serialboxMetaInfo_t* metaInfo) {
  if(!metaInfo->impl)
    serialboxFatalError("uninitialized MetaInfo");
  return reinterpret_cast<MetaInfoMap*>(metaInfo->impl);
}

inline const MetaInfoMap* toConstMetaInfoMap(const serialboxMetaInfo_t* metaInfo) {
  if(!metaInfo->impl)
    serialboxFatalError("uninitialized MetaInfo");
  return reinterpret_cast<const MetaInfoMap*>(metaInfo->impl);
}
/// @}

/// \brief Copy string into `char*` buffer
template <class StringType>
inline char* allocateAndCopyString(StringType&& str) {
  char* buffer = NULL;
  buffer = (char*)std::malloc((str.size() + 1) * sizeof(char));
  if(!buffer)
    serialboxFatalError("out of memory");

  std::memcpy(buffer, str.c_str(), str.size() + 1);
  return buffer;
}

/// \brief Allocate memory for type `T` via malloc
template <class T>
T* allocate() noexcept {
  T* data = (T*)std::malloc(sizeof(T));
  if(!data)
    serialboxFatalError("out of memory");
  std::memset(data, 0, sizeof(T));
  return data;
}

/// \brief Allocate memory for array of size `n` of type `T` via malloc
template <class T>
T* allocate(std::size_t n) noexcept {
  T* data = (T*)std::malloc(n * sizeof(T));
  if(!data)
    serialboxFatalError("out of memory");
  std::memset(data, 0, sizeof(T));
  return data;
}

} // namespace serialboxC

#endif

#endif
