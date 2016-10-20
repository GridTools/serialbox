//===-- serialbox/Core/Frontend/STELLA/KBoundary.h ----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the defintion of the k-boundary offset container.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_KBOUNDARY_H
#define SERIALBOX_CORE_FRONTEND_STELLA_KBOUNDARY_H

namespace serialbox {

namespace stella {

/// \brief Container for boundary offsets in k direction
///
/// Positive numbers indicate count arrow direction, negative numbers the opposite.
/// Therefore k-minus is typically negative.
///
/// \verbatim
///     ^ k-plus
///     |
///     v k-minus
/// \endverbatim
/// 
/// \ingroup STELLA
class KBoundary {
public:
  KBoundary() {
    kMinusOffset_ = 0;
    kPlusOffset_ = 0;
  }
  ~KBoundary() {}

  KBoundary(const KBoundary& other) { *this = other; }

  KBoundary& operator=(const KBoundary& other) {
    kMinusOffset_ = other.kMinusOffset_;
    kPlusOffset_ = other.kPlusOffset_;
    return *this;
  }

  bool operator==(const KBoundary& other) const {
    return ((kMinusOffset() == other.kMinusOffset()) && (kPlusOffset() == other.kPlusOffset()));
  }

  /// \brief Init the container
  ///
  /// \param kMinusOffset Offset of boundary in k-minus direction of domain
  /// \param kPlusOffset Offset of boundary in k-plus direction of domain
  void Init(const int kMinusOffset, const int kPlusOffset) {
    kMinusOffset_ = kMinusOffset;
    kPlusOffset_ = kPlusOffset;
  }

  /// \brief Offset in k-minus direction
  int kMinusOffset() const { return kMinusOffset_; }

  /// \brie Offset in k-plus direction
  int kPlusOffset() const { return kPlusOffset_; }

private:
  int kMinusOffset_;
  int kPlusOffset_;
};

} // namespace stella

} // namespace serialbox

#endif
