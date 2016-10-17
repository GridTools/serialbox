/*===-- serialbox-c/Array.cpp -------------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains array type definitions of the C Interface of Serialbox.
 *
\*===------------------------------------------------------------------------------------------===*/

#include "serialbox-c/Array.h"
#include "serialbox-c/Utility.h"
#include <cstdlib>

using namespace serialboxC;

template <class ArrayType, class PrimitveType>
ArrayType* allocateArray(int len) {
  ArrayType* array = allocate<ArrayType>();
  array->data = allocate<PrimitveType>(len);
  array->len = len;
  return array;
};

#define SERIALBOX_ARRAY_CREATE_DESTROY_IMPL(type)                                                  \
  serialboxArrayOf##type##_t* serialboxArrayOf##type##Create(int len) {                            \
    return allocateArray<serialboxArrayOf##type##_t, serialbox##type##_t>(len);                    \
  }                                                                                                \
                                                                                                   \
  void serialboxArrayOf##type##Destroy(serialboxArrayOf##type##_t* array) {                        \
    std::free(array->data);                                                                        \
    std::free(array);                                                                              \
  }

SERIALBOX_ARRAY_CREATE_DESTROY_IMPL(Boolean);
SERIALBOX_ARRAY_CREATE_DESTROY_IMPL(Int32);
SERIALBOX_ARRAY_CREATE_DESTROY_IMPL(Int64);
SERIALBOX_ARRAY_CREATE_DESTROY_IMPL(Float32);
SERIALBOX_ARRAY_CREATE_DESTROY_IMPL(Float64);
SERIALBOX_ARRAY_CREATE_DESTROY_IMPL(String);
