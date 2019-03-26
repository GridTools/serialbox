/*===-- serialbox-c/Api.h -----------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! API specific definitions.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_API_H
#define SERIALBOX_C_API_H

#if defined(_MSC_VER)

#ifdef SERIALBOX_DLL
#define SERIALBOX_API __declspec(dllexport)
#else
#define SERIALBOX_API
#endif

#else
#define SERIALBOX_API
#endif

#endif
