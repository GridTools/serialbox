//===-- serialbox/Core/Frontend/STELLA/DataFieldInfo.h ------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the DataFieldInfo implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_DATAFIELDINFO_H
#define SERIALBOX_CORE_FRONTEND_STELLA_DATAFIELDINFO_H

#include "serialbox/Core/Frontend/STELLA/ForwardDecl.h"

namespace serialbox {

namespace stella {

/// \brief Implementation of the STELLA DataFieldInfo
class DataFieldInfo {
public:
  /// \brief Construct with FieldMetaInfo (lifetime of FieldMetaInfo has to be managed externally)
  DataFieldInfo(FieldMetaInfo* fieldMetaInfoImpl);

private:
  FieldMetaInfo* fieldMetaInfoImpl_;
};

} // namespace stella

} // namespace serialbox

#endif
