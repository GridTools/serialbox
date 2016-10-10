/*===-- serialbox-c/MetaInfo.h ------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the MetaInfo.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_METAINFO_H
#define SERIALBOX_C_METAINFO_H

#include "serialbox-c/Type.h"

#ifdef __cplusplus
extern "C" {
#endif

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Construct an empty meta-information
 */
serialboxMetaInfo_t serialboxMetaInfoCreate();

/**
 * \brief Destroy the meta-information and deallocate all memory
 *
 * \param metaInfoPtr  Pointer to Meta-information to use
 */
void serialboxMetaInfoDestroy(serialboxMetaInfo_t* metaInfoPtr);

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get number of elemenets in the meta-information
 *
 * \param metaInfo  Meta-information to use
 * \return Number of elemenets in the meta-information
 */
int serialboxMetaInfoGetSize(serialboxMetaInfo_t metaInfo);

/**
 * \brief Check if meta information is empty
 *
 * \param metaInfo  Meta-information to use
 * \return 1 if empty, 0 otherwise
 */
int serialboxMetaInfoIsEmpty(serialboxMetaInfo_t metaInfo);

/**
 * \brief All the elements in the MetaInfo are dropped: their destructors are called, and they
 * are removed from the container, leaving it with a size of 0
 *
 * \param metaInfo  Meta-information to use
 */
void serialboxMetaInfoClear(serialboxMetaInfo_t metaInfo);

/**
 * \brief Check if and element with key ´key´ exists
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key to be searched for
 * \return 1 if elements exists, 0 otherwise
 */
int serialboxMetaInfoHasKey(serialboxMetaInfo_t metaInfo, const char* key);

/**
 * \brief Convert to string
 *
 * \param metaInfo  Meta-information to use
 * \return String representation of the meta-information
 */
const char* serialboxMetaInfoToString(serialboxMetaInfo_t metaInfo);

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
int serialboxMetaInfoAddBoolean(serialboxMetaInfo_t metaInfo, const char* key,
                                serialboxBoolean_t value);
int serialboxMetaInfoAddInt32(serialboxMetaInfo_t metaInfo, const char* key,
                              serialboxInt32_t value);
int serialboxMetaInfoAddInt64(serialboxMetaInfo_t metaInfo, const char* key,
                              serialboxInt64_t value);
int serialboxMetaInfoAddFloat32(serialboxMetaInfo_t metaInfo, const char* key,
                                serialboxFloat32_t value);
int serialboxMetaInfoAddFloat64(serialboxMetaInfo_t metaInfo, const char* key,
                                serialboxFloat64_t value);
int serialboxMetaInfoAddString(serialboxMetaInfo_t metaInfo, const char* key,
                               serialboxString_t value);
/** @} */

/**
 * \brief Add a new element in the form key=[array] to the meta-information
 *
 * The element is inserted only if its key is not equivalent to the key of any other
 * element already in the meta-information struct (i.e keys must be unique).
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key of the new element
 * \param array     Array of length ´len´ to be copied to the value of the new element
 * \param len       Length of the array
 * \return 0 if the element was inserted successfully, 1 otherwise
 * @{
 */
int serialboxMetaInfoAddArrayOfBoolean(serialboxMetaInfo_t metaInfo, const char* key,
                                       serialboxArrayOfBoolean_t value, int len);
int serialboxMetaInfoAddArrayOfInt32(serialboxMetaInfo_t metaInfo, const char* key,
                                     serialboxArrayOfInt32_t value, int len);
int serialboxMetaInfoAddArrayOfInt64(serialboxMetaInfo_t metaInfo, const char* key,
                                     serialboxArrayOfInt64_t value, int len);
int serialboxMetaInfoAddArrayOfFloat32(serialboxMetaInfo_t metaInfo, const char* key,
                                       serialboxArrayOfFloat32_t value, int len);
int serialboxMetaInfoAddArrayOfFloat64(serialboxMetaInfo_t metaInfo, const char* key,
                                       serialboxArrayOfFloat64_t value, int len);
int serialboxMetaInfoAddArrayOfString(serialboxMetaInfo_t metaInfo, const char* key,
                                      serialboxArrayOfString_t value, int len);
/** @} */

/*===------------------------------------------------------------------------------------------===*\
 *     Query meta-information
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Convert value of element with key ´key´ to type ´T´ (where ´T´ is given by the suffix of
 * thefunction
 *
 * If the type ´T´ is different than the internally stored type, the function does its best to
 * convert the value to ´T´ in a meaningful way.
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key of the new element
 * \return Copy of the value of the element as type ´T´
 *
 * \exception FatalError   Key ´key´ does not exist, conversion results in truncation of the value
 * @{
 */
serialboxBoolean_t serialboxMetaInfoGetBoolean(serialboxMetaInfo_t metaInfo, const char* key);
serialboxInt32_t serialboxMetaInfoGetInt32(serialboxMetaInfo_t metaInfo, const char* key);
serialboxInt64_t serialboxMetaInfoGetInt64(serialboxMetaInfo_t metaInfo, const char* key);
serialboxFloat32_t serialboxMetaInfoGetFloat32(serialboxMetaInfo_t metaInfo, const char* key);
serialboxFloat64_t serialboxMetaInfoGetFloat64(serialboxMetaInfo_t metaInfo, const char* key);
serialboxString_t serialboxMetaInfoGetString(serialboxMetaInfo_t metaInfo, const char* key);
/** @} */

/**
 * \brief Convert value of element with key ´key´ to a newly allocated array of type ´T´ of 
 * length ´len´
 *
 * If the type ´T´ of the elements is different than the internally stored type, the function does 
 * its best to convert the individual elements to ´T´ in a meaningful way.
 *
 * The array is allocated with malloc() and needs to be freed by the user using free().
 * 
 * \param[in] metaInfo  Meta-information to use
 * \param[in] key       Key of the new element
 * \param[out] array     Pointer to the array (will be allocated)
 * \param[out] len       Length of the allocated array
 *
 * \exception FatalError   Key ´key´ does not exist, conversion results in truncation of the value
 *                         or conversions from primitive to array type
 * @{
 */
void serialboxMetaInfoGetArrayOfBoolean(serialboxMetaInfo_t metaInfo, const char* key,
                                        serialboxArrayOfBoolean_t* array, int* len);
void serialboxMetaInfoGetArrayOfInt32(serialboxMetaInfo_t metaInfo, const char* key,
                                      serialboxArrayOfInt32_t* array, int* len);
void serialboxMetaInfoGetArrayOfInt64(serialboxMetaInfo_t metaInfo, const char* key,
                                      serialboxArrayOfInt64_t* array, int* len);
void serialboxMetaInfoGetArrayOfFloat32(serialboxMetaInfo_t metaInfo, const char* key,
                                        serialboxArrayOfFloat32_t* array, int* len);
void serialboxMetaInfoGetArrayOfFloat64(serialboxMetaInfo_t metaInfo, const char* key,
                                        serialboxArrayOfFloat64_t* array, int* len);
void serialboxMetaInfoGetArrayOfString(serialboxMetaInfo_t metaInfo, const char* key,
                                       serialboxArrayOfString_t* array, int* len);
/** @} */

#ifdef __cplusplus
}
#endif

#endif
