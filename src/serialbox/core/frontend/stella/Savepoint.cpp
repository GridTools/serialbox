//===-- serialbox/core/frontend/stella/Savepoint.cpp --------------------------------*- C++ -*-===//
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

#include "serialbox/core/frontend/stella/Savepoint.h"
#include "serialbox/core/frontend/stella/Utility.h"
#include "serialbox/core/SavepointImpl.h"
#include <ostream>

namespace serialbox {

namespace stella {

namespace internal {

template <class KeyType, class ValueType>
void insertHelper(boost::shared_ptr<SavepointImpl>& savepointImpl, KeyType&& key,
                  ValueType&& value) {
  try {
    savepointImpl->addMetainfo(key, value);
  } catch(Exception& e) {
    internal::throwSerializationException("Error: metainfo with key = %s exists already", key);
  }
}

} // namespace internal

Savepoint::Savepoint() : savepointImpl_(), metainfo_() {}

void Savepoint::Init(const std::string& name) {
  savepointImpl_ = boost::make_shared<SavepointImpl>(name);
  metainfo_.setImpl(internal::make_shared_ptr<MetainfoMapImpl>(savepointImpl_->metaInfoPtr()));
}

Savepoint::Savepoint(const boost::shared_ptr<SavepointImpl>& savepointImpl)
    : savepointImpl_(savepointImpl),
      metainfo_(internal::make_shared_ptr<MetainfoMapImpl>(savepointImpl_->metaInfoPtr())){};

Savepoint::Savepoint(const Savepoint& other) {
  savepointImpl_ = other.savepointImpl_;
  metainfo_.setImpl(internal::make_shared_ptr<MetainfoMapImpl>(savepointImpl_->metaInfoPtr()));
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

bool Savepoint::operator==(const Savepoint& other) const {
  return (*savepointImpl_ == *other.savepointImpl_);
}

bool Savepoint::operator!=(const Savepoint& other) const {
  return (*savepointImpl_ != *other.savepointImpl_);
}

const std::string& Savepoint::name() const { return savepointImpl_->name(); }

std::string Savepoint::ToString() const { return savepointImpl_->toString(); }

std::ostream& operator<<(std::ostream& out, const Savepoint& sp) { return (out << sp.ToString()); }

void Savepoint::setImpl(const boost::shared_ptr<SavepointImpl>& savepointImpl) {
  savepointImpl_ = savepointImpl;
  metainfo_.setImpl(internal::make_shared_ptr<MetainfoMapImpl>(savepointImpl_->metaInfoPtr()));
}

boost::shared_ptr<SavepointImpl>& Savepoint::getImpl() { return savepointImpl_; }

const boost::shared_ptr<SavepointImpl>& Savepoint::getImpl() const { return savepointImpl_; }

} // namespace stella

} // namespace serialbox
