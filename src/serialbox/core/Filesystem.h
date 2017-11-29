//===-- serialbox/core/Filesystem.h --------------------------------------------------*- C++
//-*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Select filesystem library.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FILESYSTEM_H
#define SERIALBOX_CORE_FILESYSTEM_H

#ifdef SERIALBOX_USE_EXPERIMENTAL_FILESYSTEM
#include <experimental/filesystem>
namespace filesystem = std::experimental::filesystem;

// work-around for problem in some versions of the stdc++fs
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=71313
namespace serialbox {
inline std::uintmax_t remove_all(const filesystem::path& p, std::error_code& ec) noexcept {
  auto fs = filesystem::symlink_status(p, ec);
  uintmax_t count = 0;
  if(ec.value() == 0 && fs.type() == filesystem::file_type::directory)
    for(filesystem::directory_iterator d(p, ec), end; ec.value() == 0 && d != end; ++d)
      count += serialbox::remove_all(d->path(), ec);
  if(ec.value())
    return -1;
  return filesystem::remove(p, ec) ? ++count : -1;
}

inline std::uintmax_t remove_all(const filesystem::path& p) {
  std::error_code ec;
  bool result = serialbox::remove_all(p, ec);
  if(ec.value())
    _GLIBCXX_THROW_OR_ABORT(filesystem::filesystem_error("cannot remove all", p, ec));
  return result;
}
}
#else
#include <boost/filesystem.hpp>
namespace filesystem = boost::filesystem;

namespace serialbox {
inline boost::uintmax_t remove_all(const filesystem::path& p) { return filesystem::remove_all(p); }

inline boost::uintmax_t remove_all(const filesystem::path& p,
                                   boost::system::error_code& ec) BOOST_NOEXCEPT {
  return filesystem::remove_all(p, ec);
}
}
#endif

#endif
