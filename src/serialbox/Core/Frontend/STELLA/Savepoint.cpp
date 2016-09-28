//===-- serialbox/Core/Frontend/STELLA/Savepoint.cpp --------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the Savepoint implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Frontend/STELLA/Savepoint.h"
#include "serialbox/Core/SavepointImpl.h"

namespace serialbox {

namespace stella {

Savepoint::Savepoint(SavepointImpl* savepointImpl) { savepointImpl_ = savepointImpl; }

Savepoint& Savepoint::operator=(const Savepoint& other) {
  *savepointImpl_ = *other.savepointImpl_;
  return (*this);
}

std::string Savepoint::name() const { return savepointImpl_->name(); }

MetainfoSet Savepoint::metainfo() const { return MetainfoSet(&savepointImpl_->metaInfo()); }

bool Savepoint::operator==(const Savepoint& other) const {
  return (*savepointImpl_ == *other.savepointImpl_);
}

bool Savepoint::operator!=(const Savepoint& other) const {
  return (*savepointImpl_ != *other.savepointImpl_);
}

std::string Savepoint::ToString() const { return savepointImpl_->toString(); }

std::ostream& operator<<(std::ostream& out, const Savepoint& sp) { return (out << sp.ToString()); }

} // namespace stella

} // namespace serialbox
