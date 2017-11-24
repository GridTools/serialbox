//===-- serialbox/core/Filesystem.h --------------------------------------------------*- C++
//-*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Select filesystem library.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FILESYSTEM_H
#define SERIALBOX_CORE_FILESYSTEM_H

#ifdef SERIALBOX_USE_EXPERIMENTAL_FILESYSTEM
#include <experimental/filesystem>
#define SB_FILESYSTEM std::experimental::filesystem
#else
#include <boost/filesystem.hpp>
#define SB_FILESYSTEM boost::filesystem
#endif

#endif
