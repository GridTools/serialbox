//===-- Unittest/UnittestStorageView.cpp --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests of the StorageView.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Serializer/StorageView.h"
#include "serialbox/Support/STLExtras.h"
#include <gtest/gtest.h>
#include <initializer_list>
#include <numeric>

using namespace serialbox;

namespace {

// Represent a dummy storage
template <class T>
struct Storage {
  std::vector<T> data;
  std::vector<int> dims;
  std::vector<int> strides;
  std::vector<std::pair<int, int>> padding;

  // Initialize storage with random data
  Storage(std::initializer_list<int> _dims, std::initializer_list<int> _strides)
      : dims(_dims), strides(_strides) {
    padding.resize(dims.size(), std::make_pair<int, int>(0, 0));

    data.resize(size(), T());
    std::for_each(data.begin(), data.end(),
                  [](T& x) { x = static_cast<T>(std::rand()) / RAND_MAX; });
  }

  // Get total size of the storage
  int size() const noexcept {
    int size = 1;
    for(std::size_t i = 0; i < dims.size(); ++i)
      size *= (padding[i].first + dims[i] + padding[i].second);
    return size;
  }
};
}

TEST(StorageView, Constructor) {
  Storage<double> s({4, 4, 4}, {1, 4, 16});

  for(auto it : s.strides)
    std::cout << it << std::endl;

  std::cout << s.data.size() << std::endl;
  
  
}
