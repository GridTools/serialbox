/*===-- serialbox-c/FieldMetainfo.h -------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the FieldMetainfoImpl.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_FIELDMETAINFO_H
#define SERIALBOX_C_FIELDMETAINFO_H

#include "serialbox-c/Type.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup serialboxC
 * @{
 *
 * \defgroup fieldmetainfo Field meta-info methods
 * @{
 */

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Construct a meta-information for a field
 *
 * \param type            TypeID of the described field
 * \param dimensions      Array of dimensions
 * \param numDimensions   Number of dimensions
 * \return pointer to the newly constructed FieldMetainfoImpl or NULL if an error occurred
 */
serialboxFieldMetainfo_t* serialboxFieldMetainfoCreate(enum serialboxTypeID type,
                                                       const int* dimensions, int numDimensions);

/**
 * \brief Copy construct the FieldMetainfoImpl
 *
 * \param other   FieldMetainfoImpl to copy from
 * \return pointer to the newly constructed FieldMetainfoImpl or NULL if an error occurred
 */
serialboxFieldMetainfo_t*
serialboxFieldMetainfoCreateFromFieldMetainfo(const serialboxFieldMetainfo_t* other);

/**
 * \brief Destroy the field meta-information and deallocate all memory
 *
 * \param fieldMetainfo  Field meta-information to use
 */
void serialboxFieldMetainfoDestroy(serialboxFieldMetainfo_t* fieldMetainfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Check if FieldMetainfoImpl `f1` is equal to FieldMetainfoImpl `f2`
 *
 * \param f1  First FieldMetainfoImpl to use
 * \param f2  Second FieldMetainfoImpl to use
 * \return 1 if `f1 == f2`, 0 otherwise
 */
int serialboxFieldMetainfoEqual(const serialboxFieldMetainfo_t* f1,
                                const serialboxFieldMetainfo_t* f2);

/**
 * \brief Convert to string
 *
 * The function will allocate a sufficiently large `char` buffer (using malloc()) which needs
 * be freed by the user using free().
 *
 * \param fieldMetainfo  Field meta-information to use
 * \return String representation of the FieldMetainfoImpl
 */
char* serialboxFieldMetainfoToString(const serialboxFieldMetainfo_t* fieldMetainfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Dimensions and TypeID
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get type-id
 *
 * \param fieldMetainfo  Field meta-information to use
 * \return type-id the field
 */
enum serialboxTypeID serialboxFieldMetainfoGetTypeID(const serialboxFieldMetainfo_t* fieldMetainfo);

/**
 * \brief Get dimensions
 *
 *
 * \param fieldMetainfo  Field meta-information to use
 * \return dimensions of the field as an array of `int`s of size
 * `serialboxFieldMetainfoGetNumDimensions`
 */
const int* serialboxFieldMetainfoGetDimensions(const serialboxFieldMetainfo_t* fieldMetainfo);

/**
 * \brief Get number of dimensions
 *
 * \param fieldMetainfo  Field meta-information to use
 * \return number of dimensions of the field
 */
int serialboxFieldMetainfoGetNumDimensions(const serialboxFieldMetainfo_t* fieldMetainfo);

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Allocate a new `serialboxMetainfo_t` which maps to the meta-information of the Field
 * meta-information
 *
 * \param fieldMetainfo  Field meta-information to use
 * \return meta-information of the field
 */
serialboxMetainfo_t* serialboxFieldMetainfoGetMetainfo(serialboxFieldMetainfo_t* fieldMetainfo);

/** @} @} */

#ifdef __cplusplus
}
#endif

#endif
