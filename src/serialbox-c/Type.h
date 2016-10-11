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

#include "serialbox/Core/Config.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===------------------------------------------------------------------------------------------===*\
 *     Primitive Types
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Boolean type of serialbox
 */
typedef int serialboxBoolean_t;

/**
 * \brief 32-bit integral type of serialbox
 */
typedef int32_t serialboxInt32_t;

/**
 * \brief 64-bit integral type of serialbox
 */
typedef int64_t serialboxInt64_t;

/**
 * \brief 32-bit floating point type of serialbox
 */
typedef float serialboxFloat32_t;

/**
 * \brief 64-bit floating point type of serialbox
 */
typedef double serialboxFloat64_t;

/**
 * \brief String type of serialbox
 */
typedef const char* serialboxString_t;

/*===------------------------------------------------------------------------------------------===*\
 *     Array Types
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Boolean type of serialbox
 */
typedef serialboxBoolean_t* serialboxArrayOfBoolean_t;

/**
 * \brief 32-bit integral type of serialbox
 */
typedef serialboxInt32_t* serialboxArrayOfInt32_t;

/**
 * \brief 64-bit integral type of serialbox
 */
typedef serialboxInt64_t* serialboxArrayOfInt64_t;

/**
 * \brief 32-bit floating point type of serialbox
 */
typedef serialboxFloat32_t* serialboxArrayOfFloat32_t;

/**
 * \brief 64-bit floating point type of serialbox
 */
typedef serialboxFloat64_t* serialboxArrayOfFloat64_t;

/**
 * \brief String type of serialbox
 */
typedef serialboxString_t* serialboxArrayOfString_t;

/*===------------------------------------------------------------------------------------------===*\
 *     Serialbox Types
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Refrence to a Serializer
 */
typedef void* serialboxSerializer_t;

/**
 * \brief Refrence to a Savepoint
 */
typedef void* serialboxSavepoint_t;

/**
 * \brief Refrence to a MetaInfo
 */
typedef void* serialboxMetaInfo_t;

/**
 * \brief Refrence to a FieldMetaInfo
 */
typedef void* serialboxFieldMetaInfo_t;

/*===------------------------------------------------------------------------------------------===*\
 *     Enumtypes
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Policy for opening files in the Serializer and Archive
 */
enum serialboxOpenModeKind { Read = 0, Write, Append };

/**
 * \brief
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

#ifdef __cplusplus
}
#endif

#endif
