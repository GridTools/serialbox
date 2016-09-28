//===-- serialbox/Core/Frontend/STELLA/IJKBoundary.h --------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains defintion of the boundary container.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_IJKBOUNDARY_H
#define SERIALBOX_CORE_FRONTEND_STELLA_IJKBOUNDARY_H

namespace serialbox {

namespace stella {

/// \brief Container for the boundary size
///
/// Container for boundary offsets in all directions of 3D domain. Positive numbers indicate count
/// arrow direction, negative numbers the opposite. Therefore i-/j-/k-minus are typically negative.
///
/// \verbatim
///            ^ j-plus
///            |
/// i-minus <-----> i-plus
///            |
///            v j-minus
/// \endverbatim
class IJKBoundary {
public:
  IJKBoundary() {
    iMinusOffset_ = 0;
    iPlusOffset_ = 0;
    jMinusOffset_ = 0;
    jPlusOffset_ = 0;
    kMinusOffset_ = 0;
    kPlusOffset_ = 0;
  }

  ~IJKBoundary() {}

  IJKBoundary(const IJKBoundary& other) { *this = other; }

  IJKBoundary& operator=(const IJKBoundary& other) {
    iMinusOffset_ = other.iMinusOffset_;
    iPlusOffset_ = other.iPlusOffset_;
    jMinusOffset_ = other.jMinusOffset_;
    jPlusOffset_ = other.jPlusOffset_;
    kMinusOffset_ = other.kMinusOffset_;
    kPlusOffset_ = other.kPlusOffset_;
    return *this;
  }

  bool operator==(const IJKBoundary& other) const {
    return ((iMinusOffset() == other.iMinusOffset()) && (iPlusOffset() == other.iPlusOffset()) &&
            (jMinusOffset() == other.jMinusOffset()) && (jPlusOffset() == other.jPlusOffset()) &&
            (kMinusOffset() == other.kMinusOffset()) && (kPlusOffset() == other.kPlusOffset()));
  }

  /// \brief Init the container
  ///
  /// \param iMinusOffset   Offset of boundary in i-minus direction of domain
  /// \param iPlusOffset    Offset of boundary in i-plus direction of domain
  /// \param jMinusOffset   Offset of boundary in j-minus direction of domain
  /// \param jPlusOffset    Offset of boundary in j-plus direction of domain
  /// \param kMinusOffset   Offset of boundary in k-minus direction of domain
  /// \param kPlusOffset    Offset of boundary in k-plus direction of domain
  void Init(const int iMinusOffset, const int iPlusOffset, const int jMinusOffset,
            const int jPlusOffset, const int kMinusOffset, const int kPlusOffset) {
    iMinusOffset_ = iMinusOffset;
    iPlusOffset_ = iPlusOffset;
    jMinusOffset_ = jMinusOffset;
    jPlusOffset_ = jPlusOffset;
    kMinusOffset_ = kMinusOffset;
    kPlusOffset_ = kPlusOffset;
  }

  /// \brief Offset in i-minus direction
  int iMinusOffset() const { return iMinusOffset_; }

  /// \brief Offset in i-plus direction
  int iPlusOffset() const { return iPlusOffset_; }

  /// \brief Offset in j-minus direction
  int jMinusOffset() const { return jMinusOffset_; }

  /// \brief Offset in j-plus direction
  int jPlusOffset() const { return jPlusOffset_; }

  /// \brief Offset in k-minus direction
  int kMinusOffset() const { return kMinusOffset_; }

  /// \brief Offset in k-plus direction
  int kPlusOffset() const { return kPlusOffset_; }

private:
  int iMinusOffset_;
  int iPlusOffset_;
  int jMinusOffset_;
  int jPlusOffset_;
  int kMinusOffset_;
  int kPlusOffset_;
};

} // namespace stella

} // namespace serialbox

#endif
