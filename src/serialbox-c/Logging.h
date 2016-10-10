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

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \brief Enable logging
 *
 * By default, the logging is disabled. If ´SERIALBOX_DISABLE_LOGGING´ is defined, the function
 * does nothing.
 */
void serialboxLoggingEnable(void);

/**
 * \brief Disable logging
 *
 * By default, the logging is disabled. If ´SERIALBOX_DISABLE_LOGGING´ is defined, the function
 * does nothing.
 */
void serialboxLoggingDisable(void);

/**
 * \brief Check if logging is enabled
 *
 * \return 1 if logging is enabled, 0 otherwise
 */
int serialboxLoggingIsEnabled(void);

#ifdef __cplusplus
}
#endif

#endif
