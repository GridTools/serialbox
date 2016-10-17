/*===-- serialbox-c/FieldMetaInfo.h -------------------------------------------------*- C++ -*-===*\
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

#ifndef SERIALBOX_C_FIELDMETAINFO_H
#define SERIALBOX_C_FIELDMETAINFO_H

#include "serialbox-c/Type.h"

#ifdef __cplusplus
extern "C" {
#endif

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Construct a meta-information for a field
 *
 * \param type            TypeID of the described field
 * \param dimensions      Array of dimensions
 * \param numDimensions   Number of dimensions
 * \return refrence to the newly constructed FieldMetaInfo or NULL if an error occurred
 */
serialboxFieldMetaInfo_t* serialboxFieldMetaInfoCreate(serialboxTypeID type, const int* dimensions,
                                                       int numDimensions);

/**
 * \brief Destroy the field meta-information and deallocate all memory
 *
 * \param fieldMetaInfo  Field meta-information to use
 */
void serialboxFieldMetaInfoDestroy(serialboxFieldMetaInfo_t* fieldMetaInfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Check if FieldMetaInfo `f1` is equal to FieldMetaInfo `f2`
 *
 * \param f1  First FieldMetaInfo to use
 * \param f2  Second FieldMetaInfo to use
 * \return 1 if `f1 == f2`, 0 otherwise
 */
int serialboxFieldMetaInfoEqual(const serialboxFieldMetaInfo_t* f1,
                                const serialboxFieldMetaInfo_t* f2);

/*===------------------------------------------------------------------------------------------===*\
 *     Dimensions and TypeID
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get type-id
 *
 * \param fieldMetaInfo  Field meta-information to use
 * \return type-id the field
 */
serialboxTypeID serialboxFieldMetaInfoGetTypeID(const serialboxFieldMetaInfo_t* fieldMetaInfo);

/**
 * \brief Get dimensions
 *
 *
 * \param fieldMetaInfo  Field meta-information to use
 * \return dimensions of the field as an array of `int`s of size
 * `serialboxFieldMetaInfoGetNumDimensions`
 */
const int* serialboxFieldMetaInfoGetDimensions(const serialboxFieldMetaInfo_t* fieldMetaInfo);

/**
 * \brief Get number of dimensions
 *
 * \param fieldMetaInfo  Field meta-information to use
 * \return number of dimensions of the field
 */
int serialboxFieldMetaInfoGetNumDimensions(const serialboxFieldMetaInfo_t* fieldMetaInfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Allocate a new `serialboxMetaInfo_t` which maps to the meta-information of the Field
 * meta-information
 *
 * \param fieldMetaInfo  Field meta-information to use
 * \return meta-information of the field
 */
serialboxMetaInfo_t* serialboxFieldMetaInfoGetMetaInfo(serialboxFieldMetaInfo_t* fieldMetaInfo);

#ifdef __cplusplus
}
#endif

#endif
