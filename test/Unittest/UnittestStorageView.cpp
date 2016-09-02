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

#include "serialbox/Support/StorageView.h"
#include "serialbox/Support/STLExtras.h"
#include <gtest/gtest.h>
#include <initializer_list>
#include <numeric>
#include <memory>

using namespace serialbox;

namespace {

// Represent a dummy storage
template <class T>
struct Storage {
  std::vector<T> data;
  std::vector<int> dims;
  std::vector<int> strides;
  std::vector<std::pair<int, int>> padding;

  Storage(std::initializer_list<int> _dims, std::initializer_list<int> _strides)
      : dims(_dims), strides(_strides) {
    padding.resize(dims.size(), std::make_pair<int, int>(0, 0));
    initData();
  }

  Storage(std::initializer_list<int> _dims, std::initializer_list<int> _strides,
          std::initializer_list<std::pair<int, int>> _padding)
      : dims(_dims), strides(_strides), padding(_padding) {
    initData();
  }

  // Initialize storage with random data
  void initData() {
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

//class StorageViewTest : public testing::Test {

//  std::shared_ptr<Storage<double>> storage_3d_;
//  std::shared_ptr<Storage<double>> storage_2d_padded_;

//  std::shared_ptr<StorageView> storageView_3d_;
//  std::shared_ptr<StorageView> storageView_2d_padded_;

//  virtual void SetUp() override {}

//  virtual void TearDown() override {}
//};

}

TEST(StorageView, Constructor) {
  
  // 3D storage
  Storage<double> storage_3d({4, 4, 4}, {1, 4, 16});
  StorageView storageView_3d(storage_3d.data.data(), toTypeID<double>::value, storage_3d.dims, storage_3d.strides,
                             storage_3d.padding);

  EXPECT_EQ(static_cast<void*>(storage_3d.data.data()), storageView_3d.data());
  EXPECT_EQ(storage_3d.strides, storageView_3d.strides());
  EXPECT_EQ(storage_3d.dims, storageView_3d.dims());
  EXPECT_EQ(storage_3d.padding, storageView_3d.padding());
  
//  // 2D storage (padding)
//  Storage<double> storage_2d_padded({4, 4}, {1, 10}, {{3, 3}, {3, 3}});
//  StorageView storageView_2d_padded(storage_2d_padded.data.data(), storage_2d_padded.dims,
//                                    storage_2d_padded.strides, storage_2d_padded.padding);
  
//  EXPECT_EQ(static_cast<void*>(storage_2d_padded.data.data()), storageView_2d_padded.data());
//  EXPECT_EQ(storage_2d_padded.strides, storageView_2d_padded.strides());
//  EXPECT_EQ(storage_2d_padded.dims, storageView_2d_padded.dims());
//  EXPECT_EQ(storage_2d_padded.padding, storageView_2d_padded.padding());
}
