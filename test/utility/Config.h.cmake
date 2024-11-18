//===--Utility/CppConfig.h ----------------------------------------------------------*- C++ -*-===//
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

/* Define if compatibility Unittests with old serialbox are built */
#cmakedefine SERIALBOX_HAS_SERIALBOX_OLD ${SERIALBOX_HAS_SERIALBOX_OLD}

/* Define if C Unittests are built */
#cmakedefine SERIALBOX_HAS_C ${SERIALBOX_HAS_C}

/* Define if Fortran Unittests are built */
#cmakedefine SERIALBOX_HAS_FORTRAN ${SERIALBOX_HAS_FORTRAN}

#endif
