//===-- serialbox/Core/Frontend/gridtools/exception.hpp -----------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the gridtools implementation of the serialbox exception.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_EXCEPTION_HPP
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_EXCEPTION_HPP

#include "serialbox/Core/Exception.h"

namespace serialbox {

namespace gridtools {

/// \brief Simple exception class which stores a human-readable error description
///
/// Creates a ´std::runtime_error´ with ´std::runtime_error::what()´ set to the formatted
/// string.
/// 
/// \see Exception
using exception = Exception;

} // namespace gridtools

} // namespace serialbox

#endif
