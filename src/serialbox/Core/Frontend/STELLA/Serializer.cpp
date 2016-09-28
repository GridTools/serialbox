//===-- serialbox/Core/Frontend/STELLA/Serializer.cpp -------------------------------*- C++ -*-===//
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

#include "serialbox/Core/Frontend/STELLA/Serializer.h"
#include "serialbox/Core/SerializerImpl.h"
#include <cstdlib>

namespace serialbox {

namespace stella {

int Serializer::enabled_ = 0;

Serializer::Serializer() { serializerImpl_ = nullptr; }

SerializerOpenMode Serializer::mode() const {
  switch(serializerImpl_->mode()) {
  case OpenModeKind::Read:
    return SerializerOpenModeRead;
  case OpenModeKind::Write:
    return SerializerOpenModeWrite;
  case OpenModeKind::Append:
    return SerializerOpenModeAppend;
  }
}

std::string Serializer::directory() const { return serializerImpl_->directory().string(); }

std::string Serializer::prefix() const { return serializerImpl_->prefix(); }

MetainfoSet Serializer::globalMetainfo() const {
  return MetainfoSet(&serializerImpl_->globalMetaInfo());
}

void Serializer::Init(const std::string& directory, const std::string& prefix,
                      SerializerOpenMode mode) {
  if(enabled_ == 0) {
    const char* envvar = std::getenv("STELLA_SERIALIZATION_DISABLED");
    enabled_ = (envvar && std::atoi(envvar) > 0) ? -1 : 1;
  }

  //
  // Remove all files with prefix if OpenMode is Write
  //
}

} // namespace stella

} // namespace serialbox
