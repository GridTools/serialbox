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

#include "serialbox-c/Type.h"

#ifdef __cplusplus
extern "C" {
#endif

/*===------------------------------------------------------------------------------------------===*\
 *     Construction & Destruction
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Construct an empty Savepoint
 *
 * \param name   Name of the Savepoint
 * \return refrence to the newly constructed Savepoint or NULL if an error occurred
 */
serialboxSavepoint_t* serialboxSavepointCreate(const char* name);

/**
 * \brief Copy construct the Savepoint
 *
 * \param other   Savepoint to copy from
 * \return Refrence to the newly constructed Serializer or NULL if an error occured
 */
serialboxSavepoint_t* serialboxSavepointCreateFromSavepoint(const serialboxSavepoint_t* other);

/**
 * \brief Destroy the savepoint and deallocate all memory
 *
 * \param savepoint  Savepoint to use
 */
void serialboxSavepointDestroy(serialboxSavepoint_t* savepoint);

/*===------------------------------------------------------------------------------------------===*\
 *     Utility
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Get the name of the savepoint
 *
 * \param savepoint  Savepoint to use
 * \return name of the savepoint
 */
const char* serialboxSavepointGetName(const serialboxSavepoint_t* savepoint);

/**
 * \brief Check if savepoint ´s1´ is equal to savepoint ´s2´
 *
 * \param s1  First savepoint to use
 * \param s2  Second savepoint to use
 * \return 1 if ´s1 == s2´, 0 otherwise
 */
int serialboxSavepointEqual(const serialboxSavepoint_t* s1, const serialboxSavepoint_t* s2);

/**
 * \brief Convert to string
 *
 * The function will allocate a sufficiently large ´char´ buffer (using malloc()) which needs
 * be freed by the user using free().
 *
 * \param savepoint  Savepoint to use
 * \return String representation of the Savepoint
 */
char* serialboxSavepointToString(const serialboxSavepoint_t* savepoint);

/*===------------------------------------------------------------------------------------------===*\
 *     Meta-information
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Allocate a new ´serialboxMetaInfo_t´ which maps to the meta-information of the Savepoint
 *
 * \param savepoint  Savepoint to use
 * \return meta-information of the savepoint
 */
serialboxMetaInfo_t* serialboxSavepointGetMetaInfo(serialboxSavepoint_t* savepoint);

#ifdef __cplusplus
}
#endif

#endif
