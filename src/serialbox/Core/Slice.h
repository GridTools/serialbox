//===-- serialbox/Core/Slice.h ------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the definition of the Slice which allows partial loading of the serialized
/// data
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SLICE_H
#define SERIALBOX_CORE_SLICE_H

#include <cassert>
#include <vector>

namespace serialbox {

/// \addtogroup core
/// @{

/// \brief Slice per dimension
struct SliceTriple {
  int start; ///< Starting index of the slice
  int stop;  ///< Stopping index of the slice (index `stop` is \b not included)
  int step;  ///< Step of the slice
};

/// \brief Specification of the slice indices which is used for partial loading of serialized data
///
/// The syntax follows closely the slicing syntax used in Python, the equivalent of
/// `[start1:stop1:step1] ... [startN, stopN, stepN]` is
/// `Slice(start1, stop1, step1) ... (startN, stopN, stepN)`
///
/// Consider the follwoing examples:
///
/// Python            | C++
/// ------            | -----
/// `[:]`             | `Slice(0, Slice::end, 1)` or `Slice()`
/// `[0:3][0:3]`      | `Slice(0, 3)(0, 3)`
/// `[1:10:2]`        | `Slice(1, 10, 2)`
/// `[:][1:5:2][:]`   | `Slice()(1, 5, 2)()`
///
class Slice {
public:
  struct Empty {};

  /// \brief Special position in the data
  enum SpecialPositionKind { end = -1 };

  /// \brief Initialize the slice of the first dimension
  ///
  /// The default arguments correspond to the full dimension (i.e no slicing in the first
  /// dimension).
  ///
  /// The python equivalent would be `[start:stop:step]`.
  ///
  /// \param start    Starting index of the slice
  /// \param stop     Stopping index of the slice (index `stop` is \b not included)
  /// \param step     Step of the slice
  Slice(int start = 0, int stop = end, int step = 1) noexcept {
    assert(start >= 0);
    assert(stop == end || stop >= start);
    assert(step > 0);
    sliceTriples_.push_back({start, stop, step});
  }

  Slice(Empty) {}
  Slice(const Slice&) = default;
  Slice(Slice&&) = default;
  Slice& operator=(const Slice&) = default;
  Slice& operator=(Slice&&) = default;

  /// \brief Append a slice to the `i-th` dimension where `i` is the current `size()` of the
  /// `sliceTriples` vector
  ///
  /// The default arguments correspond to the full dimension (i.e no slicing in the `i-th`
  /// dimension).
  ///
  /// The python equivalent would be `[start:stop:step]`.
  ///
  /// \param start    Starting index of the slice
  /// \param stop     Stopping index of the slice (index `stop` is \b not included)
  /// \param step     Step of the slice
  Slice& operator()(int start = 0, int stop = end, int step = 1) noexcept {
    assert(start >= 0);
    assert(stop == end || stop >= start);
    assert(step > 0);
    sliceTriples_.push_back({start, stop, step});
    return *this;
  }

  /// \brief Check if slice is empty
  bool empty() const noexcept { return sliceTriples_.empty(); }
  
  /// \brief Swap with `other`
  void swap(Slice& other) noexcept { sliceTriples_.swap(other.sliceTriples_); }

  /// \brief Get slice triples
  std::vector<SliceTriple>& sliceTriples() noexcept { return sliceTriples_; }
  const std::vector<SliceTriple>& sliceTriples() const noexcept { return sliceTriples_; }

private:
  std::vector<SliceTriple> sliceTriples_;
};

/// @}

} // namespace serialbox

#endif
