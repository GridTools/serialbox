//===-- Core/SerializerImpl.h -------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the shared implementation of all Serializers.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/SerializerImpl.h"
#include <type_traits>

namespace serialbox {

SerializerImpl::SerializerImpl(OpenModeKind mode, std::vector<SavepointImpl>& savepoints,
                               FieldMap& fieldMap, MetaInfoMap& globalMetaInfo,
                               std::unique_ptr<Archive>& archive)
    : mode_(mode), savepoints_(std::move(savepoints)), fieldMap_(std::move(fieldMap)),
      globalMetaInfo_(std::move(globalMetaInfo)), archive_(std::move(archive)) {}

} // namespace serialbox
