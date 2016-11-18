/*===-- serialbox-c/Logging.h -------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the Logger.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_LOGGER_H
#define SERIALBOX_C_LOGGER_H

#include "serialbox-c/Api.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup serialboxC
 * @{
 *
 * \defgroup logging Logging methods
 * @{
 */

/**
 * \brief Enable logging
 *
 * By default, the logging is disabled. If `SERIALBOX_DISABLE_LOGGING` is defined, the function
 * does nothing.
 */
SERIALBOX_API void serialboxLoggingEnable(void);

/**
 * \brief Disable logging
 *
 * By default, the logging is disabled. If `SERIALBOX_DISABLE_LOGGING` is defined, the function
 * does nothing.
 */
SERIALBOX_API void serialboxLoggingDisable(void);

/**
 * \brief Check if logging is enabled
 *
 * \return 1 if logging is enabled, 0 otherwise
 */
SERIALBOX_API int serialboxLoggingIsEnabled(void);

/** @} @} */

#ifdef __cplusplus
}
#endif

#endif
