//===-- serialbox/core/archive/MockArchive.h ----------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the Mock Archive.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/core/archive/MockArchive.h"
#include "serialbox/core/Unreachable.h"
#include <chrono>

namespace serialbox {

namespace {

template <class T>
void fillRandom(StorageView& storageView);

template <>
void fillRandom<double>(StorageView& storageView) {
  std::minstd_rand generator(std::chrono::high_resolution_clock::now().time_since_epoch().count());
  std::uniform_real_distribution<double> dist(-1.0, 1.0);
  for(auto it = storageView.begin(), end = storageView.end(); it != end; ++it) {
    it.as<double>() = dist(generator);
  }
}

template <>
void fillRandom<float>(StorageView& storageView) {
  std::minstd_rand generator(std::chrono::high_resolution_clock::now().time_since_epoch().count());
  std::uniform_real_distribution<float> dist(-1.0f, 1.0f);
  for(auto it = storageView.begin(), end = storageView.end(); it != end; ++it)
    it.as<float>() = dist(generator);
}

template <>
void fillRandom<int>(StorageView& storageView) {
  std::minstd_rand generator(std::chrono::high_resolution_clock::now().time_since_epoch().count());
  std::uniform_int_distribution<int> dist(0, 100);
  for(auto it = storageView.begin(), end = storageView.end(); it != end; ++it)
    it.as<int>() = dist(generator);
}

template <>
void fillRandom<std::int64_t>(StorageView& storageView) {
  std::minstd_rand generator(std::chrono::high_resolution_clock::now().time_since_epoch().count());
  std::uniform_int_distribution<std::int64_t> dist(0, 100);
  for(auto it = storageView.begin(), end = storageView.end(); it != end; ++it)
    it.as<std::int64_t>() = dist(generator);
}

template <>
void fillRandom<bool>(StorageView& storageView) {
  std::minstd_rand generator(std::chrono::high_resolution_clock::now().time_since_epoch().count());
  std::uniform_int_distribution<int> dist(0, 1);
  for(auto it = storageView.begin(), end = storageView.end(); it != end; ++it)
    it.as<bool>() = (bool)dist(generator);
}

} // anonymous namespace

const std::string MockArchive::Name = "Mock";

MockArchive::MockArchive(OpenModeKind mode)
    : mode_(mode), directory_(""), prefix_(""), metaDataFile_("") {}

FieldID MockArchive::write(const StorageView& storageView, const std::string& fieldID,
                           const std::shared_ptr<FieldMetainfoImpl> info) throw(Exception) {
  throw Exception("MockArchive does not support writing");
  return FieldID{fieldID, 0};
}

void MockArchive::read(StorageView& storageView, const FieldID& fieldID,
                       std::shared_ptr<FieldMetainfoImpl> info) const throw(Exception) {
  switch(storageView.type()) {
  case TypeID::Boolean:
    fillRandom<bool>(storageView);
    break;
  case TypeID::Int32:
    fillRandom<int>(storageView);
    break;
  case TypeID::Int64:
    fillRandom<std::int64_t>(storageView);
    break;
  case TypeID::Float32:
    fillRandom<float>(storageView);
    break;
  case TypeID::Float64:
    fillRandom<double>(storageView);
    break;
  default:
    serialbox_unreachable("invalid TypeID");
  }
}

std::ostream& MockArchive::toStream(std::ostream& stream) const {
  return (stream << "MockArchive");
}

} // namespace serialbox
