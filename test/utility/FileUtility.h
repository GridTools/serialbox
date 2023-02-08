//===-- utility/FileUtility.h -------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements File and Directory classes with RAII support for unittesting.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UTILITY_FILEUTILITY_H
#define SERIALBOX_UTILITY_FILEUTILITY_H

#include "serialbox/core/Config.h"

#include "utility/UnittestEnvironment.h"
#include <filesystem>
#include <fstream>
#include <system_error>

namespace serialbox {

namespace unittest {

namespace internal {

template <class DerivedType>
class FileBase {
public:
  using value_type = std::filesystem::path::value_type;
  using string_type = std::filesystem::path::string_type;

  FileBase(FileBase const&) = delete;
  FileBase(const std::filesystem::path& path) : path_(path) { create(); }
  FileBase(std::filesystem::path& path) : path_(path) { create(); }
  FileBase(const string_type& path) : path_(path) { create(); }
  FileBase(string_type& path) : path_(path) { create(); }
  FileBase(const value_type* path) : path_(path) { create(); }
  FileBase(value_type* path) : path_(path) { create(); }

  const std::filesystem::path& path() const noexcept { return path_; }
  std::filesystem::path& path() noexcept { return path_; }

private:
  void create() { static_cast<DerivedType&>(*this).createImpl(path_); }

protected:
  std::filesystem::path path_;
};

} // namespace internal

/// \brief File with RAII cleanup
class File : public internal::FileBase<File> {
public:
  using Base = internal::FileBase<File>;

  // Constructors
  template <class T>
  File(T&& path) : Base(path) {}

  // Destruction
  ~File() {
    if(UnittestEnvironment::getInstance().cleanup()) {
      std::error_code ec;
      std::filesystem::remove(path_, ec);
    }
  }

  void createImpl(const std::filesystem::path& path) {
    std::fstream fs(path.string());
    fs.close();
  }
};

/// \brief Directory with RAII cleanup
class Directory : public internal::FileBase<Directory> {
public:
  using Base = internal::FileBase<Directory>;

  // Constructors
  template <class T>
  Directory(T&& path) : Base(path) {}

  // Destruction
  ~Directory() {
    if(UnittestEnvironment::getInstance().cleanup()) {
      std::error_code ec;
      std::filesystem::remove_all(path_, ec);
    }
  }

  void createImpl(const std::filesystem::path& path) { std::filesystem::create_directories(path); }
};

} // namespace unittest

} // namespace serialbox

#endif
