//===-- serialbox/Core/Frontend/gridtools/MetaInfoValue.h ---------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the gridtools implementation of the meta-information value.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_GRIDTOOLS_META_INFO_VALUE_H
#define SERIALBOX_CORE_FRONTEND_GRIDTOOLS_META_INFO_VALUE_H

#include "serialbox/Core/MetaInfoValue.h"

namespace serialbox {

namespace gridtools {

/// \brief Represent an immutable meta information value as a type-id and type-erased data
/// 
/// \see MetaInfoValue
using meta_info_value = MetaInfoValue;

} // namespace gridtools

} // namespace serialbox

#endif
