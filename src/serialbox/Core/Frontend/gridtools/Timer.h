//===-- serialbox/Core/Frontend/gridtools/Timer.h -----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains a high resolution timer.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_TIMER_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_TIMER_H

#include "serialbox/Core/Timer.h"

namespace serialbox {

namespace gridtools {

/// \brief High resolution timer
///
/// \see
///   Timer
///
/// \ingroup gridtools
using timer = serialbox::Timer;

} // namespace gridtools

} // namespace serialbox

#endif
