//===-- serialbox/Core/Frontend/STELLA/Serializer.h ---------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the Serializer implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_SERIALIZER_H
#define SERIALBOX_CORE_FRONTEND_STELLA_SERIALIZER_H

#include "serialbox/Core/Frontend/STELLA/ForwardDecl.h"
#include "serialbox/Core/Frontend/STELLA/Savepoint.h"
#include "serialbox/Core/Frontend/STELLA/MetainfoSet.h"
#include "serialbox/Core/Frontend/STELLA/DataFieldInfo.h"
#include <vector>

namespace serialbox {

namespace stella {

/// \brief Implementation of the STELLA Serializer
class Serializer {
public:
private:
  std::vector<Savepoint> savepoints_;
  SerializerImpl* serializerImpl_;
};

} // namespace stella

} // namespace serialbox

#endif
