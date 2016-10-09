/*===-- serialbox-c/Logging.cpp -----------------------------------------------------*- C++ -*-===*\
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

#include "serialbox-c/Logging.h"
#include "serialbox/Core/Logging.h"

void serialboxLoggingEnable(void) { serialbox::Logging::enable(); }

void serialboxLoggingDisable(void) { serialbox::Logging::disable(); }

int serialboxLoggingIsEnabled(void) { return serialbox::Logging::isEnabled() ? 1 : 0; }
