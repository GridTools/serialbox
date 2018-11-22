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

#include "serialbox/core/Filesystem.h"
#include "utility/UnittestEnvironment.h"
#include <boost/core/noncopyable.hpp>
#include <fstream>
#include <system_error>

namespace serialbox {

namespace unittest {

namespace internal {

template <class DerivedType>
class FileBase : private boost::noncopyable {
public:
  using value_type = filesystem::path::value_type;
  using string_type = filesystem::path::string_type;

  FileBase(const filesystem::path& path) : path_(path) { create(); }
  FileBase(filesystem::path& path) : path_(path) { create(); }
  FileBase(const string_type& path) : path_(path) { create(); }
  FileBase(string_type& path) : path_(path) { create(); }
  FileBase(const value_type* path) : path_(path) { create(); }
  FileBase(value_type* path) : path_(path) { create(); }

  const filesystem::path& path() const noexcept { return path_; }
  filesystem::path& path() noexcept { return path_; }

private:
  void create() { static_cast<DerivedType&>(*this).createImpl(path_); }

protected:
  filesystem::path path_;
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
#ifdef SERIALBOX_USE_STD_EXPERIMENTAL_FILESYSTEM
      std::error_code ec;
#else
      boost::system::error_code ec;
#endif
      filesystem::remove(path_, ec);
    }
  }

  void createImpl(const filesystem::path& path) {
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
#ifdef SERIALBOX_USE_STD_EXPERIMENTAL_FILESYSTEM
      std::error_code ec;
#else
      boost::system::error_code ec;
#endif
      serialbox::remove_all(path_, ec);
    }
  }

  void createImpl(const filesystem::path& path) { filesystem::create_directories(path); }
};

} // namespace unittest

} // namespace serialbox

#endif
