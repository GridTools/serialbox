//===-- Unittest/GridTools.h --------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file includes all the necessary gridtools headers.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UNITTEST_GRIDTOOLS_H
#define SERIALBOX_UNITTEST_GRIDTOOLS_H

#include "serialbox/Support/Compiler.h"

#ifdef SERIALBOX_HAS_GRIDTOOLS

#define STRUCTURED_GRIDS
#define SUPPRESS_MESSAGES

#include "gridtools.hpp"
#include "storage/storage-facility.hpp"

#endif

#endif
