//===-- serialbox/Core/Frontend/STELLA/IJKSize.h ------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the defintion of the size container.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_IJKSIZE_H
#define SERIALBOX_CORE_FRONTEND_STELLA_IJKSIZE_H

#include <cassert>

namespace serialbox {

namespace stella {

/// \brief Container for i, j, k Size
///
/// \ingroup STELLA
class IJKSize {
public:
  IJKSize() {
    iSize_ = 0;
    jSize_ = 0;
    kSize_ = 0;
  }

  ~IJKSize() {}

  IJKSize(const IJKSize& other) { *this = other; }

  IJKSize& operator=(const IJKSize& other) {
    iSize_ = other.iSize_;
    jSize_ = other.jSize_;
    kSize_ = other.kSize_;
    return *this;
  }

  bool operator==(const IJKSize& other) const {
    return ((iSize() == other.iSize()) && (jSize() == other.jSize()) && (kSize() == other.kSize()));
  }

  /// \brief Init the container
  ///
  /// \param iSize size in i dimension
  /// \param jSize size in j dimension
  /// \param kSize size in k dimension
  void Init(const int iSize, const int jSize, const int kSize) {
    assert(iSize >= 0 && jSize >= 0 && kSize >= 0);
    iSize_ = iSize;
    jSize_ = jSize;
    kSize_ = kSize;
  }

  /// \brief Size in i dimension
  int iSize() const { return iSize_; }

  /// \brief Size in j dimension
  int jSize() const { return jSize_; }

  /// \brief Size in k dimension
  int kSize() const { return kSize_; }

  //// \brief Check if container is empty
  bool empty() const { return iSize_ <= 0 || jSize_ <= 0 || kSize_ <= 0; }

private:
  int iSize_, jSize_, kSize_;
};

} // namespace stella

} // namespace serialbox

#endif
