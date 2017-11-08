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
#include "serialbox/core/FieldMetainfoImpl.h"
#include "serialbox/core/MetainfoMapImpl.h"
#include "serialbox/core/SavepointImpl.h"
#include "serialbox/core/SerializerImpl.h"
#include <cstdlib>
#include <cstring>

namespace serialboxC {

using Serializer = serialbox::SerializerImpl;
using FieldMetainfo = serialbox::FieldMetainfoImpl;
using Savepoint = serialbox::SavepointImpl;
using MetainfoMap = serialbox::MetainfoMapImpl;

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

/// \brief Convert `serialboxFieldMetainfo_t` to `FieldMetainfoImpl`
/// @{
inline FieldMetainfo* toFieldMetainfo(serialboxFieldMetainfo_t* fieldMetainfo) {
  if(!fieldMetainfo->impl)
    serialboxFatalError("uninitialized FieldMetainfo");
  return reinterpret_cast<FieldMetainfo*>(fieldMetainfo->impl);
}

inline const FieldMetainfo* toConstFieldMetainfo(const serialboxFieldMetainfo_t* fieldMetainfo) {
  if(!fieldMetainfo->impl)
    serialboxFatalError("uninitialized FieldMetainfo");
  return reinterpret_cast<const FieldMetainfo*>(fieldMetainfo->impl);
}
/// @}

/// \brief Convert `serialboxMetainfo_t` to `MetainfoMapImpl`
/// @{
inline MetainfoMap* toMetainfoMap(serialboxMetainfo_t* metaInfo) {
  if(!metaInfo->impl)
    serialboxFatalError("uninitialized Metainfo");
  return reinterpret_cast<MetainfoMap*>(metaInfo->impl);
}

inline const MetainfoMap* toConstMetainfoMap(const serialboxMetainfo_t* metaInfo) {
  if(!metaInfo->impl)
    serialboxFatalError("uninitialized Metainfo");
  return reinterpret_cast<const MetainfoMap*>(metaInfo->impl);
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
  std::memset(data, 0, sizeof(T) * n);
  return data;
}

#ifdef SERIALBOX_COMPILER_MSVC

/// \brief Convert wchar_t* to char*
///
/// This is generally unsafe but atleast it might work on Windows. To properly support this, one
/// would need to switch all char* to wchar_t* whenever we deal with paths.
inline const char* toCharP(const wchar_t* wc) {
  const std::size_t wcSize = std::wcslen(wc) + 1;
  char* c = new char[wcSize];
  wcstombs(c, wc, wcSize);
  return c;
}

#else

inline const char* toCharP(const char* c) { return c; }

#endif

} // namespace serialboxC

#endif

#endif
