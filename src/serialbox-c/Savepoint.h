/*===-- serialbox-c/Savepoint.h -----------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the Savepoint.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_SAVEPOINT_H
#define SERIALBOX_C_SAVEPOINT_H

#include "serialbox-c/Api.h"
#include "serialbox-c/Type.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup serialboxC
 * @{
 *
 * \defgroup savepoint Savepoint methods
 * @{
 */

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Construct a Savepoint
 *
 * \param name   Name of the Savepoint
 * \return pointer to the newly constructed Savepoint or NULL if an error occurred
 */
SERIALBOX_API serialboxSavepoint_t* serialboxSavepointCreate(const char* name);

/**
 * \brief Copy construct the Savepoint
 *
 * \param other   Savepoint to copy from
 * \return pointer to the newly constructed Savepoint or NULL if an error occurred
 */
SERIALBOX_API serialboxSavepoint_t* 
serialboxSavepointCreateFromSavepoint(const serialboxSavepoint_t* other);

/**
 * \brief Destroy the savepoint and deallocate all memory
 *
 * \param savepoint  Savepoint to use
 */
SERIALBOX_API void serialboxSavepointDestroy(serialboxSavepoint_t* savepoint);

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get the name of the Savepoint
 *
 * \param savepoint  Savepoint to use
 * \return name of the Savepoint
 */
SERIALBOX_API const char* serialboxSavepointGetName(const serialboxSavepoint_t* savepoint);

/**
 * \brief Check if Savepoint `s1` is equal to savepoint `s2`
 *
 * \param s1  First Savepoint to use
 * \param s2  Second Savepoint to use
 * \return 1 if `s1 == s2`, 0 otherwise
 */
SERIALBOX_API int 
serialboxSavepointEqual(const serialboxSavepoint_t* s1, const serialboxSavepoint_t* s2);

/**
 * \brief Convert to string
 *
 * The function will allocate a sufficiently large `char` buffer (using malloc()) which needs
 * be freed by the user using free().
 *
 * \param savepoint  Savepoint to use
 * \return String representation of the Savepoint
 */
SERIALBOX_API char* serialboxSavepointToString(const serialboxSavepoint_t* savepoint);

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Allocate a new `serialboxMetainfo_t` which maps to the meta-information of the Savepoint
 *
 * \param savepoint  Savepoint to use
 * \return meta-information of the Savepoint
 */
SERIALBOX_API serialboxMetainfo_t* serialboxSavepointGetMetainfo(serialboxSavepoint_t* savepoint);

/** @} @} */

#ifdef __cplusplus
}
#endif

#endif
