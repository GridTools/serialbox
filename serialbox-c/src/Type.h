/*===-- serialbox-c/Type.h ----------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the type definitions of the C Interface of Serialbox.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_TYPE_H
#define SERIALBOX_C_TYPE_H

#include "serialbox-c/Api.h"
#include "serialbox/core/Config.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup serialboxC
 * @{
 *
 * \defgroup type Type definitions
 * @{
 */

/*===------------------------------------------------------------------------------------------===*\
 *     Primitive Types
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Boolean type of serialbox
 */
SERIALBOX_API typedef int serialboxBoolean_t;

/**
 * \brief 32-bit integral type of serialbox
 */
SERIALBOX_API typedef int32_t serialboxInt32_t;

/**
 * \brief 64-bit integral type of serialbox
 */
SERIALBOX_API typedef int64_t serialboxInt64_t;

/**
 * \brief 32-bit floating point type of serialbox (float)
 */
SERIALBOX_API typedef float serialboxFloat32_t;

/**
 * \brief 64-bit floating point type of serialbox (double)
 */
SERIALBOX_API typedef double serialboxFloat64_t;

/**
 * \brief String type of serialbox
 */
SERIALBOX_API typedef char* serialboxString_t;

/*===------------------------------------------------------------------------------------------===*\
 *     Serialbox Types
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Refrence to a Serializer
 */
SERIALBOX_API typedef struct {
  void* impl;
  int ownsData;
} serialboxSerializer_t;

/**
 * \brief Refrence to a Savepoint
 */
SERIALBOX_API typedef struct {
  void* impl;
  int ownsData;
} serialboxSavepoint_t;

/**
 * \brief Refrence to a Metainfo
 */
SERIALBOX_API typedef struct {
  void* impl;
  int ownsData;
} serialboxMetainfo_t;

/**
 * \brief Refrence to a FieldMetainfoImpl
 */
SERIALBOX_API typedef struct {
  void* impl;
  int ownsData;
} serialboxFieldMetainfo_t;

/*===------------------------------------------------------------------------------------------===*\
 *     Enumtypes
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Policy for opening files in the Serializer and Archive
 */
enum serialboxOpenModeKind { Read = 0, Write, Append };

/**
 * \brief Type-id of types recognized by serialbox
 */
enum serialboxTypeID {
  Invalid = 0,

  /* Primitive Types */
  Boolean,
  Int32,
  Int64,
  Float32,
  Float64,
  String,

  /* Array Types */
  Array = 0x10,
  ArrayOfBoolean = Array | Boolean,
  ArrayOfInt32 = Array | Int32,
  ArrayOfInt64 = Array | Int64,
  ArrayOfFloat32 = Array | Float32,
  ArrayOfFloat64 = Array | Float64,
  ArrayOfString = Array | String
};

/** @} @} */

#ifdef __cplusplus
}
#endif

#endif
