/*===-- serialbox-c/ConfigOptions.h -------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file exposes the platform and compiler specific options.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Api.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \brief Get null-terminated C-string of configuration options used during compilation represented
 * as a list of key=value pairs sperated by ";"
 */
SERIALBOX_API char* serialboxConfigOptions(void);

#ifdef __cplusplus
}
#endif