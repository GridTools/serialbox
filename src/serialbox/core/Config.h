/*===-- serialbox/Core/Config.h -----------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! WARNING! All changes made in this file will be lost!
 *!
 *! This generated file contains platform specific definitions.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_CORE_CONFIG_H
#define SERIALBOX_CORE_CONFIG_H
/* Define if this is Unixish platform */
#define SERIALBOX_ON_UNIX 1

/* Define if this is Win32ish platform */
/* #undef SERIALBOX_ON_WIN32 */

/* Major version of SERIALBOX */
#define SERIALBOX_VERSION_MAJOR 2

/* Minor version of SERIALBOX */
#define SERIALBOX_VERSION_MINOR 6

/* Patch version of SERIALBOX */
#define SERIALBOX_VERSION_PATCH 0

/* SERIALBOX version string */
#define SERIALBOX_VERSION_STRING "2.6.0"

/* Boost version used during compilation */
#define SERIALBOX_BOOST_VERSION 107100 

/* Define if OpenSSL is available */
/* #undef SERIALBOX_HAS_OPENSSL */

/* Define if NetCDF is available */
/* #undef SERIALBOX_HAS_NETCDF */

/* SERIALBOX was compiled with logging support */
#define SERIALBOX_HAS_LOGGING 1

/* SERIALBOX was compiled with the following filesystem */
#define SERIALBOX_USE_STD_EXPERIMENTAL_FILESYSTEM

#endif

