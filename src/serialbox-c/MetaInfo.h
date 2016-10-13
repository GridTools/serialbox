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
 *
 * \return refrence pointer to the newly constructed meta-information or NULL if an error occurred
 */
serialboxMetaInfo_t* serialboxMetaInfoCreate(void);

/**
 * \brief Destroy the meta-information and deallocate all memory
 *
 * \param metaInfo  Pointer to meta-information to use
 */
void serialboxMetaInfoDestroy(serialboxMetaInfo_t* metaInfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get number of elemenets in the meta-information
 *
 * \param metaInfo  Meta-information to use
 * \return Number of elemenets in the meta-information
 */
int serialboxMetaInfoGetSize(const serialboxMetaInfo_t* metaInfo);
/**
 * \brief Check if meta information is empty
 *
 * \param metaInfo  Meta-information to use
 * \return 1 if empty, 0 otherwise
 */
int serialboxMetaInfoIsEmpty(const serialboxMetaInfo_t* metaInfo);

/**
 * \brief All the elements in the MetaInfo are dropped: their destructors are called, and they
 * are removed from the container, leaving it with a size of 0
 *
 * \param metaInfo  Meta-information to use
 */
void serialboxMetaInfoClear(serialboxMetaInfo_t* metaInfo);

/**
 * \brief Check if and element with key ´key´ exists
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key to be searched for
 * \return 1 if elements exists, 0 otherwise
 */
int serialboxMetaInfoHasKey(const serialboxMetaInfo_t* metaInfo, const char* key);

/**
 * \brief Convert to string
 *
 * The function will allocate a sufficiently large ´char´ buffer (using malloc()) which needs
 * be freed by the user using free().
 *
 * \param metaInfo  Meta-information to use
 * \return C-string representation of the meta-information
 */
char* serialboxMetaInfoToString(const serialboxMetaInfo_t* metaInfo);

/**
 * \brief Get an array of C-strings of all available keys in the meta-information
 *
 * The function will allocate a sufficiently large array of ´char*´. Each element (as well as the
 * array itself) needs to be freed by the user using free().
 *
 * To get the corresponding ´serialboxTypeID´ for each element use ´serialboxMetaInfoTypes´.
 *
 * \param[in]  metaInfo  Meta-information to use
 * \param[out] keys      Array of length ´len´ of C-strings of all keys in the meta-information
 * \param[out] len       Length of the array
 */
void serialboxMetaInfoGetKeys(const serialboxMetaInfo_t* metaInfo, char*** keys, int* len);

/**
 * \brief Get an array of ´serialboxTypeID´ of all available elements in the meta-information
 *
 * The function will allocate a sufficiently large array of serialboxTypeID*´ buffer which needs
 * to be freed by the user using free().
 *
 * To get the corresponding keys for each element use ´serialboxMetaInfoKeys´.
 *
 * \param[in]  metaInfo  Meta-information to use
 * \param[out] types     Array of length ´len´ of ´serialboxTypeID´s of all elements in the
 *                       meta-information
 * \param[out] len       Length of the array
 *
 * \return array of ´serialboxTypeID´s of all elements in the meta-information
 */
void serialboxMetaInfoGetTypes(const serialboxMetaInfo_t* metaInfo, serialboxTypeID** types,
                               int* len);

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
int serialboxMetaInfoAddBoolean(serialboxMetaInfo_t* metaInfo, const char* key,
                                serialboxBoolean_t value);
int serialboxMetaInfoAddInt32(serialboxMetaInfo_t* metaInfo, const char* key,
                              serialboxInt32_t value);
int serialboxMetaInfoAddInt64(serialboxMetaInfo_t* metaInfo, const char* key,
                              serialboxInt64_t value);
int serialboxMetaInfoAddFloat32(serialboxMetaInfo_t* metaInfo, const char* key,
                                serialboxFloat32_t value);
int serialboxMetaInfoAddFloat64(serialboxMetaInfo_t* metaInfo, const char* key,
                                serialboxFloat64_t value);
int serialboxMetaInfoAddString(serialboxMetaInfo_t* metaInfo, const char* key,
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
int serialboxMetaInfoAddArrayOfBoolean(serialboxMetaInfo_t* metaInfo, const char* key,
                                       serialboxArrayOfBoolean_t value, int len);
int serialboxMetaInfoAddArrayOfInt32(serialboxMetaInfo_t* metaInfo, const char* key,
                                     serialboxArrayOfInt32_t value, int len);
int serialboxMetaInfoAddArrayOfInt64(serialboxMetaInfo_t* metaInfo, const char* key,
                                     serialboxArrayOfInt64_t value, int len);
int serialboxMetaInfoAddArrayOfFloat32(serialboxMetaInfo_t* metaInfo, const char* key,
                                       serialboxArrayOfFloat32_t value, int len);
int serialboxMetaInfoAddArrayOfFloat64(serialboxMetaInfo_t* metaInfo, const char* key,
                                       serialboxArrayOfFloat64_t value, int len);
int serialboxMetaInfoAddArrayOfString(serialboxMetaInfo_t* metaInfo, const char* key,
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
 * The string version will allocate a sufficiently large ´char´ buffer (using malloc()) which needs
 * be freed by the user using free().
 *
 * \param metaInfo  Meta-information to use
 * \param key       Key of the new element
 * \return Copy of the value of the element as type ´T´
 *
 * \exception FatalError   Key ´key´ does not exist, conversion results in truncation of the value
 * @{
 */
serialboxBoolean_t serialboxMetaInfoGetBoolean(const serialboxMetaInfo_t* metaInfo,
                                               const char* key);
serialboxInt32_t serialboxMetaInfoGetInt32(const serialboxMetaInfo_t* metaInfo, const char* key);
serialboxInt64_t serialboxMetaInfoGetInt64(const serialboxMetaInfo_t* metaInfo, const char* key);
serialboxFloat32_t serialboxMetaInfoGetFloat32(const serialboxMetaInfo_t* metaInfo,
                                               const char* key);
serialboxFloat64_t serialboxMetaInfoGetFloat64(const serialboxMetaInfo_t* metaInfo,
                                               const char* key);
serialboxString_t serialboxMetaInfoGetString(const serialboxMetaInfo_t* metaInfo, const char* key);
/** @} */

/**
 * \brief Convert value of element with key ´key´ to a newly allocated array of type ´T´ of
 * length ´len´
 *
 * If the type ´T´ of the elements is different than the internally stored type, the function does
 * its best to convert the individual elements to ´T´ in a meaningful way.
 *
 * The array is allocated with malloc() and needs to be freed by the user using free(). The string
 * version will additionally allocate a sufficiently large ´char´ buffer for each entry in the array
 * which needs to be freed as well.
 *
 * \param[in] metaInfo   Meta-information to use
 * \param[in] key        Key of the new element
 * \param[out] array     Pointer to the array (will be allocated)
 * \param[out] len       Length of the allocated array
 *
 * \exception FatalError   Key ´key´ does not exist, conversion results in truncation of the value
 *                         or conversions from primitive to array type
 * @{
 */
void serialboxMetaInfoGetArrayOfBoolean(const serialboxMetaInfo_t* metaInfo, const char* key,
                                        serialboxArrayOfBoolean_t* array, int* len);
void serialboxMetaInfoGetArrayOfInt32(const serialboxMetaInfo_t* metaInfo, const char* key,
                                      serialboxArrayOfInt32_t* array, int* len);
void serialboxMetaInfoGetArrayOfInt64(const serialboxMetaInfo_t* metaInfo, const char* key,
                                      serialboxArrayOfInt64_t* array, int* len);
void serialboxMetaInfoGetArrayOfFloat32(const serialboxMetaInfo_t* metaInfo, const char* key,
                                        serialboxArrayOfFloat32_t* array, int* len);
void serialboxMetaInfoGetArrayOfFloat64(const serialboxMetaInfo_t* metaInfo, const char* key,
                                        serialboxArrayOfFloat64_t* array, int* len);
void serialboxMetaInfoGetArrayOfString(const serialboxMetaInfo_t* metaInfo, const char* key,
                                       serialboxArrayOfString_t* array, int* len);
/** @} */

#ifdef __cplusplus
}
#endif

#endif
