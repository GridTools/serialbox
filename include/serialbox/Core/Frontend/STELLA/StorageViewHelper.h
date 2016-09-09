//===-- serialbox/Core/Frontend/STELLA/StorageViewHelper.h --------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains helper functions to create StorageViews of STELLA fields.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_STORAGEVIEWHELPER_H
#define SERIALBOX_CORE_FRONTEND_STELLA_STORAGEVIEWHELPER_H

#include "serialbox/Core/Frontend/STELLA/Config.h"
#include <utility>
#include <vector>

namespace serialbox {

namespace stella {

namespace internal {


//===------------------------------------------------------------------------------------------===//
//     Strides
//===------------------------------------------------------------------------------------------===//

template <typename TFieldType>
std::vector<int> getStrides(const TFieldType& dataField) {
  DataFieldStorageStrides<typename TFieldType::StorageFormat::StorageOrder> storageStrides;
  storageStrides.Init(dataField.storage().paddedSize());

  std::vector<int> strides(3);
  strides[0] = storageStrides.ComputeStride(1, 0, 0);
  strides[1] = storageStrides.ComputeStride(0, 1, 0);
  strides[2] = storageStrides.ComputeStride(0, 0, 1);

  return strides;
}

//===------------------------------------------------------------------------------------------===//
//     Dimensions
//===------------------------------------------------------------------------------------------===//

template <typename TFieldType>
std::vector<int> getDims(const TFieldType& dataField) {
  const IJKSize& size = dataField.storage().allocatedSize();

  std::vector<int> dims(3);
  dims[0] = size.iSize();
  dims[1] = size.jSize();
  dims[2] = size.kSize();

  return dims;
}

//===------------------------------------------------------------------------------------------===//
//     Padding
//===------------------------------------------------------------------------------------------===//
template <typename TFieldType>
std::vector<std::pair<int, int>> getPadding(const TFieldType& dataField) {

  std::vector<std::pair<int, int>> padding;
  // TODO
  return padding;
}

//===------------------------------------------------------------------------------------------===//
//     Data
//===------------------------------------------------------------------------------------------===//

template <typename TFieldType>
void* getDataPointer(const TFieldType& dataField) {
  const IJKIndex& originOffset = dataField.storage().originOffset();
  return static_cast<void*>(
      &dataField(-originOffset.iIndex(), -originOffset.jIndex(), -originOffset.kIndex()));
}

} // namespace internal

} // namespace stella

} // namespace serialbox

#endif
