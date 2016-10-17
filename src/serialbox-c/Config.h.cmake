/*===-- serialbox-c/Config.h ---------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! ${SERIALBOX_CONFIG_FILE_DISCLAIMER}
 *!
 *! This file contains platform specific definitions of the Serialbox C Interface.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox/Core/Config.h"

#ifndef SERIALBOX_C_CONFIG_H
#define SERIALBOX_C_CONFIG_H

/* SERIALBOX C compiler string */
#define SERIALBOX_C_COMPILER_STRING "${SERIALBOX_C_COMPILER}"

/* SERIALBOX C++ compiler string */
#define SERIALBOX_CXX_COMPILER_STRING "${SERIALBOX_CXX_COMPILER}"

/* SERIALBOX C compiler flags */
#define SERIALBOX_C_FLAGS "${SERIALBOX_C_FLAGS}"

/* SERIALBOX C++ compiler flags */
#define SERIALBOX_CXX_FLAGS "${SERIALBOX_CXX_FLAGS}"

/**
 * \brief Get null-terminated C-string of configuration options used during compilation represented
 * as a list of key=value pairs sperated by ";"
 */
char* serialboxConfigOptions(void);

#endif

