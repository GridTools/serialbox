//===-- serialbox/Core/Config.h -----------------------------------------------------*- C++ -*-===//
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
// This generated file is for internal use. Do not include it from other headers, use Compiler.h
// instead.
//
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_CONFIG_H
#define SERIALBOX_CORE_CONFIG_H

/* Define if this is Unixish platform */
#cmakedefine SERIALBOX_ON_UNIX ${SERIALBOX_ON_UNIX}

/* Define if this is Win32ish platform */
#cmakedefine SERIALBOX_ON_WIN32 ${SERIALBOX_ON_WIN32}

/* Major version of SERIALBOX */
#define SERIALBOX_VERSION_MAJOR ${SERIALBOX_VERSION_MAJOR}

/* Minor version of SERIALBOX */
#define SERIALBOX_VERSION_MINOR ${SERIALBOX_VERSION_MINOR}

/* Patch version of SERIALBOX */
#define SERIALBOX_VERSION_PATCH ${SERIALBOX_VERSION_PATCH}

/* SERIALBOX version string */
#define SERIALBOX_VERSION_STRING "${SERIALBOX_VERSION_MAJOR}.${SERIALBOX_VERSION_MINOR}.${SERIALBOX_VERSION_PATCH}"

/* Boost version used for compilation */
#define SERIALBOX_BOOST_VERSION ${SERIALBOX_BOOST_VERSION} 

/* Define if OpenSSL is available */
#cmakedefine SERIALBOX_HAS_OPENSSL ${SERIALBOX_HAS_OPENSSL}

#endif
