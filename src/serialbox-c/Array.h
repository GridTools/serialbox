/*===-- serialbox-c/Array.h ---------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the array type definitions of the C Interface of Serialbox.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Api.h"
#include "serialbox-c/Type.h"

#ifndef SERIALBOX_C_ARRAY_H
#define SERIALBOX_C_ARRAY_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup serialboxC
 * @{
 *
 * \defgroup array Array definitions
 * @{
 */

/*===------------------------------------------------------------------------------------------===*\
 *     Array Types
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Array of booleans
 */
SERIALBOX_API typedef struct {
  serialboxBoolean_t* data;
  int len;
} serialboxArrayOfBoolean_t;

/**
 * \brief Array of 32-bit integers
 */
SERIALBOX_API typedef struct {
  serialboxInt32_t* data;
  int len;
} serialboxArrayOfInt32_t;

/**
 * \brief Array of 64-bit integers
 */
SERIALBOX_API typedef struct {
  serialboxInt64_t* data;
  int len;
} serialboxArrayOfInt64_t;

/**
 * \brief Array of 32-bit floating point numbers (float)
 */
SERIALBOX_API typedef struct {
  serialboxFloat32_t* data;
  int len;
} serialboxArrayOfFloat32_t;

/**
 * \brief Array of 64-bit floating point numbers (double)
 */
SERIALBOX_API typedef struct {
  serialboxFloat64_t* data;
  int len;
} serialboxArrayOfFloat64_t;

/**
 * \brief Array of strings
 */
SERIALBOX_API typedef struct {
  serialboxString_t* data;
  int len;
} serialboxArrayOfString_t;

/**
 * \brief Allocate array of type `T` of size `len`
 *
 * \param len   Length of the allocated array
 * \return newly allocated array of length `len`
 * @{
 */
SERIALBOX_API serialboxArrayOfBoolean_t* serialboxArrayOfBooleanCreate(int len);
SERIALBOX_API serialboxArrayOfInt32_t* serialboxArrayOfInt32Create(int len);
SERIALBOX_API serialboxArrayOfInt64_t* serialboxArrayOfInt64Create(int len);
SERIALBOX_API serialboxArrayOfFloat32_t* serialboxArrayOfFloat32Create(int len);
SERIALBOX_API serialboxArrayOfFloat64_t* serialboxArrayOfFloat64Create(int len);
SERIALBOX_API serialboxArrayOfString_t* serialboxArrayOfStringCreate(int len);
/** @} */

/**
 * \brief Deallocate array
 *
 * \param array   Array to dellaocate
 * @{
 */
SERIALBOX_API void serialboxArrayOfBooleanDestroy(serialboxArrayOfBoolean_t* array);
SERIALBOX_API void serialboxArrayOfInt32Destroy(serialboxArrayOfInt32_t* array);
SERIALBOX_API void serialboxArrayOfInt64Destroy(serialboxArrayOfInt64_t* array);
SERIALBOX_API void serialboxArrayOfFloat32Destroy(serialboxArrayOfFloat32_t* array);
SERIALBOX_API void serialboxArrayOfFloat64Destroy(serialboxArrayOfFloat64_t* array);
SERIALBOX_API void serialboxArrayOfStringDestroy(serialboxArrayOfString_t* array);
/** @} */

/** @} @} */

#ifdef __cplusplus
}
#endif

#endif
