//===-- serialbox/Core/Compiler.h ---------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file defines several macros, based on the current compiler. This allows use of
/// compiler-specific features in a way that remains portable.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_COMPILER_H
#define SERIALBOX_CORE_COMPILER_H

#include "serialbox/Core/Config.h"

#ifndef __has_feature
#define __has_feature(x) 0
#endif

#ifndef __has_extension
#define __has_extension(x) 0
#endif

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#if defined(__clang__)
#define SERIALBOX_COMPILER_CLANG 1
#endif

#if defined(__ICC) || defined(__INTEL_COMPILER)
#define SERIALBOX_COMPILER_INTEL 1
#endif

#if defined(__GNUC__) || defined(__GNUG__)
#define SERIALBOX_COMPILER_GNU 1
#endif

#if defined(_MSC_VER)
#define SERIALBOX_COMPILER_MSVC 1
#endif

/// \macro SERIALBOX_GNUC_PREREQ
/// \brief Extend the default __GNUC_PREREQ even if glibc's features.h isn't
/// available
#ifndef SERIALBOX_GNUC_PREREQ
#if defined(__GNUC__) && defined(__GNUC_MINOR__) && defined(__GNUC_PATCHLEVEL__)
#define SERIALBOX_GNUC_PREREQ(maj, min, patch)                                                     \
  ((__GNUC__ << 20) + (__GNUC_MINOR__ << 10) + __GNUC_PATCHLEVEL__ >=                              \
   ((maj) << 20) + ((min) << 10) + (patch))
#elif defined(__GNUC__) && defined(__GNUC_MINOR__)
#define SERIALBOX_GNUC_PREREQ(maj, min, patch)                                                     \
  ((__GNUC__ << 20) + (__GNUC_MINOR__ << 10) >= ((maj) << 20) + ((min) << 10))
#else
#define SERIALBOX_GNUC_PREREQ(maj, min, patch) 0
#endif
#endif

/// \macro SERIALBOX_BUILTIN_UNREACHABLE
/// \brief Indicate unreachable state
///
/// On compilers which support it, expands to an expression which states that it is undefined
/// behaviour for the compiler to reach this point. Otherwise is not defined.
#if __has_builtin(__builtin_unreachable) || SERIALBOX_GNUC_PREREQ(4, 5, 0)
#define SERIALBOX_BUILTIN_UNREACHABLE __builtin_unreachable()
#elif defined(_MSC_VER)
#define SERIALBOX_BUILTIN_UNREACHABLE __assume(false)
#endif

/// \macro SERIALBOX_ATTRIBUTE_ALWAYS_INLINE
/// \brief Mark a method as "always inline" for performance reasons
#if __has_attribute(always_inline) || SERIALBOX_GNUC_PREREQ(4, 0, 0)
#define SERIALBOX_ATTRIBUTE_ALWAYS_INLINE __attribute__((always_inline))
#elif defined(_MSC_VER)
#define SERIALBOX_ATTRIBUTE_ALWAYS_INLINE __forceinline
#else
#define SERIALBOX_ATTRIBUTE_ALWAYS_INLINE
#endif

/// \macro SERIALBOX_ATTRIBUTE_NORETURN
/// \brief Mark a method as "no return"
#ifdef __GNUC__
#define SERIALBOX_ATTRIBUTE_NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
#define SERIALBOX_ATTRIBUTE_NORETURN __declspec(noreturn)
#else
#define SERIALBOX_ATTRIBUTE_NORETURN
#endif

/// \macro SERIALBOX_BUILTIN_LIKELY
/// \brief Mark this expression as being likely evaluated to "true"
#if __has_builtin(__builtin_expect) || SERIALBOX_GNUC_PREREQ(4, 5, 0)
#define SERIALBOX_BUILTIN_LIKELY(x) __builtin_expect(!!(x), 1)
#else
#define SERIALBOX_BUILTIN_LIKELY(x) (x)
#endif

/// \macro SERIALBOX_BUILTIN_UNLIKELY
/// \brief Mark this expression as being likely evaluated to "false"
#if __has_builtin(__builtin_expect) || SERIALBOX_GNUC_PREREQ(4, 5, 0)
#define SERIALBOX_BUILTIN_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define SERIALBOX_BUILTIN_UNLIKELY(x) (x)
#endif

#endif
