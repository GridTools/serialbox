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
/// This file contains the unittests for the StorageView.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Support/STLExtras.h"
#include "serialbox/Support/StorageView.h"
#include "serialbox/Support/Type.h"
#include <gtest/gtest.h>
#include <cstring>
#include <initializer_list>
#include <memory>
#include <numeric>
#include <type_traits>

using namespace serialbox;

namespace {

// Represent a dummy storage
template <class T>
struct Storage {
  using value_type = T;

  std::vector<T> data;
  std::vector<int> dims;
  std::vector<int> strides;
  std::vector<std::pair<int, int>> padding;

  Storage(std::initializer_list<int> d, std::initializer_list<int> s, bool init = true)
      : dims(d), strides(s) {
    padding.resize(dims.size(), std::make_pair<int, int>(0, 0));
    data.resize(size(), T());
    if(init)
      initData();
  }

  Storage(std::initializer_list<int> d, std::initializer_list<int> s,
          std::initializer_list<std::pair<int, int>> p, bool init = true)
      : dims(d), strides(s), padding(p) {
    data.resize(size(), T());
    if(init)
      initData();
  }

  // Initialize storage with random data
  void initData() {
    for(int i = 0; i < data.size(); ++i)
      data[i] = i;
  }

  // Get total size of the storage
  int size() const noexcept {
    int size = 1;
    for(std::size_t i = 0; i < dims.size(); ++i)
      size *= (padding[i].first + dims[i] + padding[i].second);
    return size;
  }

  // TODO: Implement this efficiently
  T& at(std::initializer_list<int> index) {
    int pos = 0;
    int size = index.size();
    auto it = index.begin();

    for(int i = 0; i < size; ++i, ++it)
      pos += (strides[i] * (padding[i].first + (*it)));

    return *(data.data() + pos);
  }

  // Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const Storage& s) {
    stream << "Storage [\n";
    stream << "  data = {\n";
    for(auto d : s.data)
      stream << "    " << d << "\n";

    stream << "  }\n  size = " << s.size() << "\n";
    stream << "  dims = {";
    for(auto i : s.dims)
      stream << " " << i;

    stream << " }\n  strides = {";
    for(auto i : s.strides)
      stream << " " << i;

    stream << " }\n  padding = {";
    for(auto i : s.padding)
      stream << " [" << i.first << "," << i.second << "]";

    stream << " }\n]\n";
    return stream;
  }
};

class StorageViewTest : public testing::Test {
public:
  // Dimensions
  static constexpr int dim1 = 2;
  static constexpr int dim2 = 3;

  // Padding
  static constexpr int pad1_left = 1;
  static constexpr int pad1_right = 1;

  static constexpr int pad2_left = 1;
  static constexpr int pad2_right = 1;

  // --- 1D ---
  std::shared_ptr<Storage<double>> storage_1d;
  std::shared_ptr<Storage<double>> storage_1d_padded;

  // --- 2D ---
  std::shared_ptr<Storage<double>> storage_2d_col_major;
  std::shared_ptr<Storage<double>> storage_2d_col_major_padded;

  std::shared_ptr<Storage<double>> storage_2d_row_major;
  std::shared_ptr<Storage<double>> storage_2d_row_major_padded;

protected:
  virtual void SetUp() override {
    using il = std::initializer_list<int>;
    using ipl = std::initializer_list<std::pair<int, int>>;

    // --- 1D ---
    storage_1d = std::make_shared<Storage<double>>(il{dim1}, il{1});
    storage_1d_padded = std::make_shared<Storage<double>>(
        il{dim1}, il{1}, ipl{std::pair<int, int>(pad1_left, pad1_right)});

    // --- 2D ---
    storage_2d_col_major = std::make_shared<Storage<double>>(il{dim1, dim2}, il{1, dim1});
    storage_2d_col_major_padded =
        std::make_shared<Storage<double>>(il{dim1, dim2}, il{1, dim1 + pad1_left + pad1_right},
                                          ipl{std::pair<int, int>(pad1_left, pad1_right),
                                              std::pair<int, int>(pad2_left, pad2_right)});

    storage_2d_row_major = std::make_shared<Storage<double>>(il{dim1, dim2}, il{dim2, 1});
    storage_2d_row_major_padded =
        std::make_shared<Storage<double>>(il{dim1, dim2}, il{dim2 + pad2_left + pad2_right, 1},
                                          ipl{std::pair<int, int>(pad1_left, pad1_right),
                                              std::pair<int, int>(pad2_left, pad2_right)});
  }

  virtual void TearDown() override {}
};

} // anonymous namespace

template <class T>
static std::shared_ptr<StorageView> toStorageView(std::shared_ptr<Storage<T>>& storage) {
  auto type = ToTypeID<T>::value;
  return std::make_shared<StorageView>(storage->data.data(), type, storage->dims, storage->strides,
                                       storage->padding);
}

TEST_F(StorageViewTest, Construction) {

  // Constructor
  auto sv_1d = *toStorageView(storage_1d);
  EXPECT_EQ(static_cast<void*>(storage_1d->data.data()),
            static_cast<void*>(sv_1d.data()));
  EXPECT_EQ(storage_1d->strides, sv_1d.strides());
  EXPECT_EQ(storage_1d->dims, sv_1d.dims());
  EXPECT_EQ(storage_1d->padding, sv_1d.padding());

  // Copy constructor
  auto sv_2d_col_major = *toStorageView(storage_2d_col_major);

  StorageView copy_sv_1d(sv_1d);
  ASSERT_EQ(copy_sv_1d, sv_1d);

  StorageView copy_sv_2d(sv_2d_col_major);
  ASSERT_EQ(copy_sv_2d, sv_2d_col_major);

  // Swap
  sv_1d.swap(sv_2d_col_major);
  EXPECT_EQ(sv_1d, copy_sv_2d);
  EXPECT_EQ(sv_2d_col_major, copy_sv_1d);

  // Copy assignment
  sv_1d = sv_2d_col_major;
  EXPECT_EQ(sv_1d, sv_2d_col_major);
}

TEST_F(StorageViewTest, IteratorConstruction) {
  auto sv_1d = *toStorageView(storage_1d);
  auto sv_1d_pad = *toStorageView(storage_1d_padded);
  auto sv_2d_col_major = *toStorageView(storage_2d_col_major);
  auto sv_2d_col_major_pad = *toStorageView(storage_2d_col_major_padded);
  auto sv_2d_row_major = *toStorageView(storage_2d_row_major);
  auto sv_2d_row_major_pad = *toStorageView(storage_2d_row_major_padded);

  EXPECT_NE(sv_1d.begin(), sv_1d.end());
  EXPECT_EQ(sv_1d.begin(), sv_1d.begin());
  EXPECT_EQ(sv_1d.end(), sv_1d.end());
  EXPECT_NE(sv_1d.begin(), sv_2d_col_major.begin());

  // Check if begin() points to the beginning of the data

  // --- 1D ---
  EXPECT_EQ(static_cast<void*>(sv_1d.begin().ptr()), static_cast<void*>(sv_1d.data()));
  EXPECT_EQ(static_cast<void*>(sv_1d.begin().ptr()), static_cast<void*>(&storage_1d->at({0})));
  EXPECT_EQ(static_cast<void*>(sv_1d_pad.begin().ptr()),
            static_cast<void*>(&storage_1d_padded->at({0})));

  // --- 2D ---
  EXPECT_EQ(static_cast<void*>(sv_2d_col_major.begin().ptr()),
            static_cast<void*>(sv_2d_col_major.data()));
  EXPECT_EQ(static_cast<void*>(sv_2d_col_major.begin().ptr()),
            static_cast<void*>(&storage_2d_col_major->at({0, 0})));
  EXPECT_EQ(static_cast<void*>(sv_2d_col_major_pad.begin().ptr()),
            static_cast<void*>(&storage_2d_col_major_padded->at({0, 0})));

  EXPECT_EQ(static_cast<void*>(sv_2d_row_major.begin().ptr()),
            static_cast<void*>(sv_2d_row_major.data()));
  EXPECT_EQ(static_cast<void*>(sv_2d_row_major.begin().ptr()),
            static_cast<void*>(&storage_2d_row_major->at({0, 0})));
  EXPECT_EQ(static_cast<void*>(sv_2d_row_major_pad.begin().ptr()),
            static_cast<void*>(&storage_2d_row_major_padded->at({0, 0})));

  // Copy constructor
  auto it_sv_1d(sv_1d.begin());
  auto copy_it_sv_1d(it_sv_1d);
  EXPECT_EQ(copy_it_sv_1d, sv_1d.begin());

  auto it_sv_2d(sv_2d_col_major.begin());
  auto copy_it_sv_2d(it_sv_2d);
  EXPECT_EQ(copy_it_sv_2d, sv_2d_col_major.begin());

  // Copy assignment
  auto copy_assign_it_sv_1d = it_sv_1d;
  EXPECT_EQ(copy_assign_it_sv_1d, sv_1d.begin());

  // Swap
  it_sv_2d.swap(it_sv_1d);
  EXPECT_EQ(it_sv_1d, copy_it_sv_2d);
  EXPECT_EQ(it_sv_2d, copy_it_sv_1d);
}

TEST_F(StorageViewTest, IteratorCopy) {

  // The idea of this test is to copy a contiguous piece of data into the storages using the
  // StorageView and StorageViewIterator interfaces.

  std::vector<double> data(storage_2d_row_major_padded->size());
  std::iota(data.begin(), data.end(), 1);
  char* dataPtr;
  const int bytesPerElement = sizeof(double);

  // --- 1D ---

  // Copy data into storage_1d using StorageView and StorageViewIterator
  auto sv_1d = toStorageView(storage_1d);
  dataPtr = reinterpret_cast<char*>(data.data());
  for(auto it = sv_1d->begin(), end = sv_1d->end(); it != end; ++it, dataPtr += bytesPerElement)
    std::memcpy(it.ptr(), dataPtr, bytesPerElement);

  // Copy data into storage_1d_pad using StorageView and StorageViewIterator
  auto sv_1d_pad = toStorageView(storage_1d_padded);
  dataPtr = reinterpret_cast<char*>(data.data());
  for(auto it = sv_1d_pad->begin(), end = sv_1d_pad->end(); it != end;
      ++it, dataPtr += bytesPerElement)
    std::memcpy(it.ptr(), dataPtr, bytesPerElement);

  ASSERT_EQ(sv_1d->bytesPerElement(), bytesPerElement);
  ASSERT_EQ(sv_1d_pad->bytesPerElement(), bytesPerElement);
  ASSERT_EQ(storage_1d->dims[0], storage_1d_padded->dims[0]);

  // Check data was copied correctly
  for(int i = 0; i < storage_1d->dims[0]; ++i) {
    EXPECT_DOUBLE_EQ(data[i], storage_1d->at({i}));
    EXPECT_DOUBLE_EQ(data[i], storage_1d_padded->at({i}));
  }

  // --- 2D ---

  // --------------------------------

  //  std::cout << *storage_1d_padded << std::endl;

  //  for(int i = 0; i < storage_1d_padded->dims[0]; ++i)
  //    std::cout << storage_1d_padded->at({i}) << "\n";

  // --------------------------------

  //  std::cout << *storage_1d << std::endl;

  //  for(int i = 0; i < storage_1d->dims[0]; ++i)
  //    std::cout << storage_1d->at({i}) << "\n";

  // --------------------------------

  //    std::cout << *storage_2d_col_major << std::endl;

  //    for(int j = 0; j < storage_2d_col_major->dims[1]; ++j)
  //      for(int i = 0; i < storage_2d_col_major->dims[0]; ++i)
  //        std::cout << storage_2d_col_major->at({i, j}) << "\n";

  //    std::cout << *storage_2d_row_major << std::endl;

  //    for(int j = 0; j < storage_2d_row_major->dims[1]; ++j)
  //      for(int i = 0; i < storage_2d_row_major->dims[0]; ++i)
  //        std::cout << storage_2d_row_major->at({i, j}) << "\n";

  // ---------------------------------

  //    std::cout << *storage_2d_col_major_padded << std::endl;

  //    for(int j = 0; j < storage_2d_col_major_padded->dims[1]; ++j)
  //      for(int i = 0; i < storage_2d_col_major_padded->dims[0]; ++i)
  //        std::cout << storage_2d_col_major_padded->at({i, j}) << "\n";

  //  std::cout << (*reinterpret_cast<double*>(it_sv_2d.data<double>())) << std::endl;
}

TEST_F(StorageViewTest, Testing) {

  auto type = ToTypeID<double>::value;  
  const int bytesPerElement = sizeof(double);  
  
  std::vector<double> data({0.0, 1.0, 2.0, 3.0, 4.0, 5.0});
  char* dataPtr;
  
  // ----------------------------------------
  
  Storage<double> colMajor2D({2, 3}, {1, 2}, false); 

  StorageView svColMajor2D(colMajor2D.data.data(), type, colMajor2D.dims, colMajor2D.strides,
                           colMajor2D.padding);  
  
  dataPtr = reinterpret_cast<char*>(data.data());
  for(auto it = svColMajor2D.begin(), end = svColMajor2D.end(); it != end; ++it, dataPtr += bytesPerElement) {
    std::memcpy(it.ptr(), dataPtr, bytesPerElement);
//    std::cout << (*reinterpret_cast<double*>(it.ptr())) << std::endl;      
  }
  
//  std::cout << colMajor2D << std::endl; 
  
  // -------------------------------------
  
  Storage<double> rowMajor2D({2, 3}, {3, 1}, false);
  
  StorageView svRowMajor2D(rowMajor2D.data.data(), type, rowMajor2D.dims, rowMajor2D.strides,
                           rowMajor2D.padding);  
  
  dataPtr = reinterpret_cast<char*>(data.data());
  for(auto it = svRowMajor2D.begin(), end = svRowMajor2D.end(); it != end; ++it, dataPtr += bytesPerElement) {
    std::memcpy(it.ptr(), dataPtr, bytesPerElement);
//    std::cout << (*reinterpret_cast<double*>(it.ptr())) << std::endl;      
  }
  
//  std::cout << rowMajor2D << std::endl; 
  
  for(int j = 0; j < 3; ++j)
    for(int i = 0; i < 2; ++i) {
      ASSERT_DOUBLE_EQ(rowMajor2D.at({i, j}), colMajor2D.at({i, j})) << std::endl;
    }
}
