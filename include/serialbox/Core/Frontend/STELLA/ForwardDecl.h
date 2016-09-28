//===-- serialbox/Core/Frontend/STELLA/ForwardDecl.h --------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains forward declaration of the Core infrastructure.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_FORWARDDECL_H
#define SERIALBOX_CORE_FRONTEND_STELLA_FORWARDDECL_H

namespace serialbox {

class FieldMap;
class FieldMetaInfo;
class MetaInfoMap;
class MetaInfoValue;
class SerializerImpl;
class SavepointImpl;

namespace stella {}

} // namespace serialbox

/// \namespace ser
/// \brief Namespace of the STELLA frontend
namespace ser = serialbox::stella;

#endif
