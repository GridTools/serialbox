/*===-- serialbox-c/Metainfo.h ------------------------------------------------------*- C++ -*-===*\
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

#ifndef SERIALBOX_C_METAINFO_H
#define SERIALBOX_C_METAINFO_H

#include "serialbox-c/Api.h"
#include "serialbox-c/Array.h"
#include "serialbox-c/Type.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup serialboxC
 * @{
 *
 * \defgroup metainfo Meta-info methods
 * @{
 */

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Construct an empty meta-information
 *
 * \return refrence pointer to the newly constructed meta-information or NULL if an error occurred
 */
SERIALBOX_API serialboxMetainfo_t* serialboxMetainfoCreate(void);

/**
 * \brief Copy construct the meta-information
 *
 * \param other   Metainfo to copy from
 * \return refrence pointer to the newly constructed meta-information or NULL if an error occurred
 */
SERIALBOX_API serialboxMetainfo_t*
serialboxMetainfoCreateFromMetainfo(const serialboxMetainfo_t* other);

/**
 * \brief Destroy the meta-information and deallocate all memory
 *
 * \param metaInfo  Pointer to meta-information to use
 */
SERIALBOX_API void serialboxMetainfoDestroy(serialboxMetainfo_t* metaInfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get number of elemenets in the meta-information
 *
 * \param metaInfo  Meta-information to use
 * \return Number of elemenets in the meta-information
 */
SERIALBOX_API int serialboxMetainfoGetSize(const serialboxMetainfo_t* metaInfo);
/**
 * \brief Check if meta information is empty
 *
 * \param metaInfo  Meta-information to use
 * \return 1 if empty, 0 otherwise
 */
SERIALBOX_API int serialboxMetainfoIsEmpty(const serialboxMetainfo_t* metaInfo);

/**
 * \brief All the elements in the Metainfo are dropped: their destructors are called, and they
 * are removed from the container, leaving it with a size of 0
 *
 * \param metaInfo  Meta-information to use
 */
SERIALBOX_API void serialboxMetainfoClear(serialboxMetainfo_t* metaInfo);

/**
 * \brief Check if Metainfo `m1` is equal to Metainfo `m2`
 *
 * \param m1  First Metainfo to use
 * \param m2  Second Metainfo to use
 * \return 1 if `m1 == m2`, 0 otherwise
 */
SERIALBOX_API int serialboxMetainfoEqual(const serialboxMetainfo_t* m1,
                                         const serialboxMetainfo_t* m2);

/**
 * \brief Check if and element with key `key` exists
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key to be searched for
 * \return 1 if elements exists, 0 otherwise
 */
SERIALBOX_API int serialboxMetainfoHasKey(const serialboxMetainfo_t* metaInfo, const char* key);

/**
 * \brief Get TypeID of element with key `key`
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key used for lookup
 * \return TypeID of the element
 */
SERIALBOX_API enum serialboxTypeID
serialboxMetainfoGetTypeIDOfKey(const serialboxMetainfo_t* metaInfo, const char* key);

/**
 * \brief Convert to string
 *
 * The function will allocate a sufficiently large `char` buffer (using malloc()) which needs
 * be freed by the user using free().
 *
 * \param metaInfo  Meta-information to use
 * \return C-string representation of the meta-information
 */
SERIALBOX_API char* serialboxMetainfoToString(const serialboxMetainfo_t* metaInfo);

/**
 * \brief Data-structure to query the info of the elements (keys and corresponding types) in the
 * meta-information
 *
 * \param keys      Array of null-terminated C-strings of the key of each element
 * \param types     Array of TypeIDs of each element
 * \param len       Number of elements in the meta-information
 */
SERIALBOX_API typedef struct {
  char** keys;
  int* types;
  int len;
} serialboxMetainfoElementInfo_t;

/**
 * \brief Allocate and intialize the element-info
 *
 * To free the data-structure use `serialboxMetainfoDestroyElementInfo`.
 *
 * \param metaInfo  Meta-information to use
 * \return allocated and initialized `serialboxMetainfoElementInfo_t`
 */
SERIALBOX_API serialboxMetainfoElementInfo_t*
serialboxMetainfoCreateElementInfo(const serialboxMetainfo_t* metaInfo);

/**
 * \brief Destroy the element-information and deallocate all memory
 *
 * \param elementInfo  Element-information to deallcate
 */
SERIALBOX_API void serialboxMetainfoDestroyElementInfo(serialboxMetainfoElementInfo_t* elementInfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Add meta-information
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Add a new element in the form key=value pair to the meta-information
 *
 * The element is inserted only if its key is not equivalent to the key of any other
 * element already in the meta-information struct (i.e keys must be unique).
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key of the new element
 * \param value     Object to be copied to the value of the new element
 * \return 0 if the element was inserted successfully, 1 otherwise
 * @{
 */
SERIALBOX_API int serialboxMetainfoAddBoolean(serialboxMetainfo_t* metaInfo, const char* key,
                                              serialboxBoolean_t value);
SERIALBOX_API int serialboxMetainfoAddInt32(serialboxMetainfo_t* metaInfo, const char* key,
                                            serialboxInt32_t value);
SERIALBOX_API int serialboxMetainfoAddInt64(serialboxMetainfo_t* metaInfo, const char* key,
                                            serialboxInt64_t value);
SERIALBOX_API int serialboxMetainfoAddFloat32(serialboxMetainfo_t* metaInfo, const char* key,
                                              serialboxFloat32_t value);
SERIALBOX_API int serialboxMetainfoAddFloat64(serialboxMetainfo_t* metaInfo, const char* key,
                                              serialboxFloat64_t value);
SERIALBOX_API int serialboxMetainfoAddString(serialboxMetainfo_t* metaInfo, const char* key,
                                             const char* value);
/** @} */

/**
 * \brief Add a new element in the form key=[array] to the meta-information
 *
 * The element is inserted only if its key is not equivalent to the key of any other
 * element already in the meta-information struct (i.e keys must be unique).
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key of the new element
 * \param array     Array to be copied to the value of the new element
 * \return 0 if the element was inserted successfully, 1 otherwise
 * @{
 */
SERIALBOX_API int serialboxMetainfoAddArrayOfBoolean(serialboxMetainfo_t* metaInfo, const char* key,
                                                     const serialboxArrayOfBoolean_t* array);
SERIALBOX_API int serialboxMetainfoAddArrayOfInt32(serialboxMetainfo_t* metaInfo, const char* key,
                                                   const serialboxArrayOfInt32_t* array);
SERIALBOX_API int serialboxMetainfoAddArrayOfInt64(serialboxMetainfo_t* metaInfo, const char* key,
                                                   const serialboxArrayOfInt64_t* array);
SERIALBOX_API int serialboxMetainfoAddArrayOfFloat32(serialboxMetainfo_t* metaInfo, const char* key,
                                                     const serialboxArrayOfFloat32_t* array);
SERIALBOX_API int serialboxMetainfoAddArrayOfFloat64(serialboxMetainfo_t* metaInfo, const char* key,
                                                     const serialboxArrayOfFloat64_t* array);
SERIALBOX_API int serialboxMetainfoAddArrayOfString(serialboxMetainfo_t* metaInfo, const char* key,
                                                    const serialboxArrayOfString_t* array);
/** @} */

/*===------------------------------------------------------------------------------------------===*\
 *     Query meta-information
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Convert value of element with key `key` to type `T` (where `T` is given by the suffix of
 * thefunction
 *
 * If the type `T` is different than the internally stored type, the function does its best to
 * convert the value to `T` in a meaningful way.
 *
 * The string version will allocate a sufficiently large `char` buffer (using malloc()) which needs
 * be freed by the user using free().
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key of the element
 * \return copy of the value of the element as type `T`
 *
 * \exception FatalError   Key `key` does not exist, conversion results in truncation of the value
 * @{
 */
SERIALBOX_API serialboxBoolean_t serialboxMetainfoGetBoolean(const serialboxMetainfo_t* metaInfo,
                                                             const char* key);
SERIALBOX_API serialboxInt32_t serialboxMetainfoGetInt32(const serialboxMetainfo_t* metaInfo,
                                                         const char* key);
SERIALBOX_API serialboxInt64_t serialboxMetainfoGetInt64(const serialboxMetainfo_t* metaInfo,
                                                         const char* key);
SERIALBOX_API serialboxFloat32_t serialboxMetainfoGetFloat32(const serialboxMetainfo_t* metaInfo,
                                                             const char* key);
SERIALBOX_API serialboxFloat64_t serialboxMetainfoGetFloat64(const serialboxMetainfo_t* metaInfo,
                                                             const char* key);
SERIALBOX_API serialboxString_t serialboxMetainfoGetString(const serialboxMetainfo_t* metaInfo,
                                                           const char* key);
/** @} */

/**
 * \brief Convert value of element with key `key` to a newly allocated array array of type `T`
 *
 * If the type `T` of the elements is different than the internally stored type, the function does
 * its best to convert the individual elements to `T` in a meaningful way.
 *
 * The allocated array should be freed using the corresponding `serialboxArrayOfXDestroy` methods.
 *
 * \param metaInfo   Meta-information to use
 * \param key        Key of the element
 * \return pointer to the newly allocated array
 *
 * \exception FatalError   Key `key` does not exist, conversion results in truncation of the value
 *                         or conversions from primitive to array type
 * @{
 */
SERIALBOX_API serialboxArrayOfBoolean_t*
serialboxMetainfoGetArrayOfBoolean(const serialboxMetainfo_t* metaInfo, const char* key);

SERIALBOX_API serialboxArrayOfInt32_t*
serialboxMetainfoGetArrayOfInt32(const serialboxMetainfo_t* metaInfo, const char* key);

SERIALBOX_API serialboxArrayOfInt64_t*
serialboxMetainfoGetArrayOfInt64(const serialboxMetainfo_t* metaInfo, const char* key);

SERIALBOX_API serialboxArrayOfFloat32_t*
serialboxMetainfoGetArrayOfFloat32(const serialboxMetainfo_t* metaInfo, const char* key);

SERIALBOX_API serialboxArrayOfFloat64_t*
serialboxMetainfoGetArrayOfFloat64(const serialboxMetainfo_t* metaInfo, const char* key);

SERIALBOX_API serialboxArrayOfString_t*
serialboxMetainfoGetArrayOfString(const serialboxMetainfo_t* metaInfo, const char* key);
/** @} */

/** @} @} */

#ifdef __cplusplus
}
#endif

#endif
