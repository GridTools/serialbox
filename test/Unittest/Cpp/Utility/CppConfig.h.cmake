//===--Unittest/Cpp/Utility/CppConfig.h ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
// ${SERIALBOX_CONFIG_FILE_DISCLAIMER}
//
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UNITTEST_CPP_UTILITY_CPPCONFIG_H
#define SERIALBOX_UNITTEST_CPP_UTILITY_CPPCONFIG_H

/* Define if GridTools Unittests are built */
#cmakedefine SERIALBOX_HAS_GRIDTOOLS ${SERIALBOX_HAS_GRIDTOOLS}

/* Define if STELLA Unittests are built */
#cmakedefine SERIALBOX_HAS_STELLA ${SERIALBOX_HAS_STELLA}

/* Define if Fortran Unittests are built */
#cmakedefine SERIALBOX_HAS_FORTRAN ${SERIALBOX_HAS_FORTRAN}

#endif
