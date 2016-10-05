//===-- serialbox/Core/Timer.h ------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains a high resolution timer.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_TIMER_H
#define SERIALBOX_CORE_TIMER_H

#include <chrono>

namespace serialbox {

/// \brief High resolution timer
class Timer {
public:
  /// \brief Start the timer
  Timer() : start_(std::chrono::high_resolution_clock::now()) {}

  /// \brief Reset the timer to the current time
  inline void start() noexcept { start_ = std::chrono::high_resolution_clock::now(); }

  /// \brief Return the number of milliseconds elapsed since the timer was last reset
  inline double stop() noexcept {
    auto end = std::chrono::high_resolution_clock::now();
    return std::chrono::duration<double, std::milli>(end - start_).count();
  }

private:
  std::chrono::time_point<std::chrono::high_resolution_clock> start_;
};

} // namespace serialbox

#endif
