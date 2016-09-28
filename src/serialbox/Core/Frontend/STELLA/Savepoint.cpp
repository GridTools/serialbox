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
#include "serialbox/Core/Frontend/STELLA/Utility.h"
#include "serialbox/Core/SavepointImpl.h"
#include <ostream>

namespace serialbox {

namespace stella {

namespace internal {

template <class KeyType, class ValueType>
void insertHelper(SavepointImpl* savepointImpl, KeyType&& key, ValueType&& value) {
  try {
    savepointImpl->addMetaInfo(key, value);
  } catch(Exception& e) {
    internal::throwSerializationException("Error: metainfo with key = %s exists already", key);
  }
}

} // namespace internal

Savepoint::~Savepoint() {
  if(owner_)
    delete savepointImpl_;
}

Savepoint::Savepoint(const std::string& name)
    : owner_(true), savepointImpl_(new SavepointImpl(name)) {}

Savepoint::Savepoint(SavepointImpl* savepointImpl) : owner_(false), savepointImpl_(savepointImpl){};

Savepoint::Savepoint(const Savepoint& other) {
  savepointImpl_ = new SavepointImpl(other.name());
  owner_ = true;
  *savepointImpl_ = *other.savepointImpl_;  
}

Savepoint& Savepoint::operator=(const Savepoint& other) {
  *savepointImpl_ = *other.savepointImpl_;
  return (*this);
}

void Savepoint::AddMetainfo(const std::string& key, const bool& value) {
  internal::insertHelper(savepointImpl_, key, value);
}

void Savepoint::AddMetainfo(const std::string& key, const int& value) {
  internal::insertHelper(savepointImpl_, key, value);
}

void Savepoint::AddMetainfo(const std::string& key, const float& value) {
  internal::insertHelper(savepointImpl_, key, value);
}

void Savepoint::AddMetainfo(const std::string& key, const double& value) {
  internal::insertHelper(savepointImpl_, key, value);
}

void Savepoint::AddMetainfo(const std::string& key, const std::string& value) {
  internal::insertHelper(savepointImpl_, key, value);
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
