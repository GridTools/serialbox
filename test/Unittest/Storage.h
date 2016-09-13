//===-- Unittest/Storage.h ----------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements a dummy storage used for testing.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UNITTEST_STORAGE_H
#define SERIALBOX_UNITTEST_STORAGE_H

#include "serialbox/Core/Logging.h"
#include "serialbox/Core/StorageView.h"
#include "serialbox/Core/STLExtras.h"
#include <cstring>
#include <initializer_list>
#include <iostream>
#include <limits>
#include <memory>
#include <numeric>
#include <type_traits>
#include <utility>
#include <vector>

namespace serialbox {

namespace unittest {

/// \brief Represent a dummy storage to test the StorageView in absence of gridtools or STELLA
template <class T>
struct Storage {
  using value_type = T;
  
  /// \brief Storage order
  enum StorageOrderKind {
    RowMajor, ///< Stride 1 in last dimension
    ColMajor  ///< Stride 1 in first dimension
  };

  Storage() = default;
  Storage(const Storage&) = default;
  Storage(Storage&&) = default;
  Storage& operator=(const Storage&) = default;
  Storage& operator=(Storage&&) = default;

  /// \brief Members
  /// @{
  StorageOrderKind ordering;
  std::vector<T> data;
  std::vector<int> dims;
  std::vector<int> strides;
  std::vector<std::pair<int, int>> padding;
  /// @}

  /// \brief Initialize a storage of given dimensions
  Storage(StorageOrderKind ordering_, std::initializer_list<int> dims_, bool init = true)
      : ordering(ordering_), dims(dims_) {
    padding.resize(dims.size(), std::make_pair<int, int>(0, 0));
    data.resize(size(), T());
    computeStrides();

    if(init)
      initData();
  }

  /// \brief Initialize a storage of given dimensions and padding
  Storage(StorageOrderKind ordering_, std::initializer_list<int> dims_,
          std::initializer_list<std::pair<int, int>> padding_, bool init = true)
      : ordering(ordering_), dims(dims_), padding(padding_) {
    data.resize(size(), T());
    computeStrides();

    if(init)
      initData();
  }

  /// \brief Convert to stream
  int size() const noexcept {
    int size = 1;
    for(std::size_t i = 0; i < dims.size(); ++i)
      size *= (padding[i].first + dims[i] + padding[i].second);
    return size;
  }

  /// \brief Access data
  T& at(std::initializer_list<int> index) noexcept {
    int pos = 0;
    int size = index.size();
    auto it = index.begin();
    CHECK(size == static_cast<int>(dims.size())) << "invalid access";

    for(int i = 0; i < size; ++i, ++it)
      pos += (strides[i] * (padding[i].first + (*it)));

    return *(data.data() + pos);
  }

  /// \brief Convert to stream
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

  /// \brief Convert to StorageView
  StorageView toStorageView() {
    auto type = ToTypeID<T>::value;
    return StorageView(data.data(), type, dims, strides, padding);
  }

private:
  void computeStrides() {
    int numDim = dims.size();

    strides.resize(numDim);

    if(ordering == ColMajor) {
      int stride = 1;
      strides[0] = stride;

      for(int i = 1; i < numDim; ++i) {
        stride *= (padding[i - 1].first + dims[i - 1] + padding[i - 1].second);
        strides[i] = stride;
      }
    } else {
      int stride = 1;
      strides[numDim - 1] = stride;

      for(int i = numDim - 2; i >= 0; --i) {
        stride *= (padding[i + 1].first + dims[i + 1] + padding[i + 1].second);
        strides[i] = stride;
      }
    }
  }

  void initData() {
    for(std::size_t i = 0; i < data.size(); ++i)
      data[i] = i;
  }
};

} // namespace unittest

} // namespace serialbox

#endif
