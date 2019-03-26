/*===-- serialbox-c/ErrorHandling.h -------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the error handling of the C Interface of Serialbox.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Api.h"

#ifndef SERIALBOX_C_ERRORHANDLING_H
#define SERIALBOX_C_ERRORHANDLING_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup serialboxC
 * @{
 *
 * \defgroup error Error-handling methods
 * @{
 */

/**
 * \brief Report a fatal error
 *
 * This will function will invoke the installed FatalErrorHandler.
 *
 * \see serialboxInstallFatalErrorHandler
 */
SERIALBOX_API void serialboxFatalError(const char* reason);

/**
 * \brief Error handler callback
 */
SERIALBOX_API typedef void (*serialboxFatalErrorHandler_t)(const char* reason);

/**
 * \brief Install a fatal error handler
 *
 * By default, if Serialbox detects a fatal error it will emit the last error string to stderr and
 * call exit(1).
 *
 * This may not be appropriate in some contexts. For example, the Python module might want to
 * translate the errror into an exception. This function allows you to install a callback that will
 * be invoked after a fatal error occurred.
 */
SERIALBOX_API void serialboxInstallFatalErrorHandler(serialboxFatalErrorHandler_t handler);

/**
 * \brief Reset the fatal error handler.
 *
 * This resets Serialbox's fatal error handling behavior to the default.
 */
SERIALBOX_API void serialboxResetFatalErrorHandler(void);

/*===------------------------------------------------------------------------------------------===*\
 *     Default Error Handler
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Emit the last error string to stderr and call exit(1)
 */
void serialboxDefaultFatalErrorHandler(const char* reason);

/*===------------------------------------------------------------------------------------------===*\
 *     State Error Handler
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Store the the current state of the error which can be queried via
 * \ref serialboxStateErrorHandlerGetLastError.
 */
SERIALBOX_API void serialboxStateErrorHandler(const char* reason);

/**
 * \brief Check the current error state
 *
 * This function requires to set the ErrorHandler to  \ref serialboxStateErrorHandler. To obtain the
 * associated error message, use \ref serialboxStateErrorHandlerGetErrorMessage.
 *
 * \return 1 if there was an error, 0 otherwise
 */
SERIALBOX_API int serialboxStateErrorHandlerHasError(void);

/**
 * \brief Query the current error state
 *
 * This function requires to set the ErrorHandler to \ref serialboxStateErrorHandler.
 *
 * \return newly allocated `char*` with the current error message
 */
SERIALBOX_API char* serialboxStateErrorHandlerGetErrorMessage(void);

/**
 * \brief Reset the current error state
 */
SERIALBOX_API void serialboxStateErrorHandlerResetState(void);

/** @} @} */

#ifdef __cplusplus
}
#endif

#endif
