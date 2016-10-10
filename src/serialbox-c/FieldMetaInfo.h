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
 * \param numDimensions   Number of dimensions
 * \param dimensions      Array of dimensions
 */
serialboxFieldMetaInfo_t serialboxFieldMetaInfoCreate(serialboxTypeID type, int numDimensions,
                                                      int* dimensions);

/**
 * \brief Destroy the field meta-information and deallocate all memory
 *
 * \param fieldMetaInfoPtr  Pointer to Field meta-information to use
 */
void serialboxFieldMetaInfoDestroy(serialboxFieldMetaInfo_t* fieldMetaInfoPtr);

/*===------------------------------------------------------------------------------------------===*\
 *     Dimensions and TypeID
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get type-id
 *
 * \param fieldMetaInfo  Field meta-information to use
 * \return type-id the field
 */
serialboxTypeID serialboxFieldMetaInfoGetTypeID(serialboxFieldMetaInfo_t fieldMetaInfo);

/**
 * \brief Get dimensions
 *
 * \param fieldMetaInfo  Field meta-information to use
 * \return dimensions of the field
 */
const int* serialboxFieldMetaInfoGetDimensions(serialboxFieldMetaInfo_t fieldMetaInfo);

/**
 * \brief Get number of dimensions
 *
 * \param fieldMetaInfo  Field meta-information to use
 * \return number of dimensions of the field
 */
int serialboxFieldMetaInfoGetNumDimensions(serialboxFieldMetaInfo_t fieldMetaInfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get meta-information
 *
 * \param fieldMetaInfo  Field meta-information to use
 * \return type-id the field
 */
serialboxMetaInfo_t serialboxFieldMetaInfoGetMetaInfo(serialboxFieldMetaInfo_t fieldMetaInfo);

#ifdef __cplusplus
}
#endif

#endif
