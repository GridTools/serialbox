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

#include <boost/core/noncopyable.hpp>
#include "utility/UnittestEnvironment.h"
#include "serialbox/core/Filesystem.h"
#include <fstream>
#include <system_error>

namespace serialbox {

namespace unittest {

namespace internal {

template <class DerivedType>
class FileBase : private boost::noncopyable {
public:
  using value_type = SB_FILESYSTEM::path::value_type;
  using string_type = SB_FILESYSTEM::path::string_type;

  FileBase(const SB_FILESYSTEM::path& path) : path_(path) { create(); }
  FileBase(SB_FILESYSTEM::path& path) : path_(path) { create(); }
  FileBase(const string_type& path) : path_(path) { create(); }
  FileBase(string_type& path) : path_(path) { create(); }
  FileBase(const value_type* path) : path_(path) { create(); }
  FileBase(value_type* path) : path_(path) { create(); }

  const SB_FILESYSTEM::path& path() const noexcept { return path_; }
  SB_FILESYSTEM::path& path() noexcept { return path_; }

private:
  void create() { static_cast<DerivedType&>(*this).createImpl(path_); }

protected:
  SB_FILESYSTEM::path path_;
};

} // namespace internal

/// \brief File with RAII cleanup
class File : public internal::FileBase<File> {
public:
  using Base = internal::FileBase<File>;

  // Constructors
  template <class T>
  File(T&& path)
      : Base(path) {}

  // Destruction
  ~File() {
    if(UnittestEnvironment::getInstance().cleanup()) {
#ifdef SERIALBOX_USE_EXPERIMENTAL_FILESYSTEM
      std::error_code ec;
#else
      boost::system::error_code ec;
#endif
      SB_FILESYSTEM::remove(path_, ec);
    }
  }

  void createImpl(const SB_FILESYSTEM::path& path) {
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
  Directory(T&& path)
      : Base(path) {}

  // Destruction
  ~Directory() {
    if(UnittestEnvironment::getInstance().cleanup()) {
#ifdef SERIALBOX_USE_EXPERIMENTAL_FILESYSTEM
      std::error_code ec;
#else
      boost::system::error_code ec;
#endif
      SB_FILESYSTEM::remove_all(path_, ec);
    }
  }

  void createImpl(const SB_FILESYSTEM::path& path) { SB_FILESYSTEM::create_directories(path); }
};

} // namespace unittest

} // namespace serialbox

#endif
