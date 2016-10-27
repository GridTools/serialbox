//===-- serialbox/Core/Frontend/gridtools/Slice.h ---------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the gridtools typedefs of the slice definition.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_SLICE_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_SLICE_H

#include "serialbox/Core/Slice.h"

namespace serialbox {

namespace gridtools {

/// \brief Specification of the slice indices which is used for partial loading of serialized data
///
/// The syntax follows closely the slicing syntax used in Python, the equivalent of
/// `[start1:stop1:step1, ... ,startN:stopN:stepN]` is
/// `Slice(start1, stop1, step1) ... (startN, stopN, stepN)` with one notable \b exception: The
/// full dimension `[:]` is represented as `Slice(0, -1)` this
/// means `[:-1]` corresponds to `Slice(0, -2)`.
///
/// Consider the follwoing examples:
///
/// Python              | C++
/// ------              | -----
/// `[5]`               | `Slice(5, 6)`
/// `[:]`               | `Slice(0, -1, 1)` or `Slice()`
/// `[0:3, 0:3]`        | `Slice(0, 3)(0, 3)`
/// `[1:10:2]`          | `Slice(1, 10, 2)`
/// `[:, 1:5:2, 1:-2]`  | `Slice()(1, 5, 2)(1, -3)`
///
/// \ingroup gridtools
using slice = serialbox::Slice;

} // namespace gridtools

} // namespace serialbox

#endif
