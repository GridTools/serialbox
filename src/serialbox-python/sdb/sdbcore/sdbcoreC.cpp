//===-- sdbcoreC.cpp ----------------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains utility functions for the sdb core library
///
//===------------------------------------------------------------------------------------------===//

#include <Python.h>

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <numpy/ndarrayobject.h>

#include <cmath>
#include <vector>
#include <stdexcept>

namespace {

template <class T>
class NumpyArray {
public:
  NumpyArray(PyArrayObject* array)
      : data_((T*)PyArray_DATA(array)), size_(PyArray_SIZE(array)),
        shape_(PyArray_DIMS(array), PyArray_DIMS(array) + PyArray_NDIM(array)) {}

  /// \brief Access data pointer
  const T* data() const noexcept { return data_; }
  T* data() noexcept { return data_; }

  /// \brief Shape of the numpy array
  const std::vector<npy_intp>& shape() const noexcept { return shape_; }

  /// \brief Size of the array
  npy_intp size() const noexcept { return size_; }

  /// \brief Convert to string
  friend std::ostream& operator<<(std::ostream& stream, const NumpyArray& array) {
    stream << "shape = [ ";
    for(const auto& s : array.shape())
      stream << s << " ";
    stream << "], size = " << array.size() << ", data = " << array.data();
    return stream;
  }

private:
  T* data_;
  npy_intp size_;
  std::vector<npy_intp> shape_;
};

template <class T>
struct ErrorList {
  std::vector<int> index;
  T input_value;
  T reference_value;
};

/// \brief Compute positions of errors of input and reference fields
///
/// The error_position field is set to True if
///
///   absolute(input - reference) <= (atol + rtol * absolute(reference))
///
/// evaluates to False.
template <class T>
inline int compute_error_positions(const NumpyArray<T>& input, const NumpyArray<T>& reference,
                                   NumpyArray<bool>& error_positions, const double& atol,
                                   const double& rtol) noexcept {
  const int size = input.size();
  const T* input_ptr = input.data();
  const T* reference_ptr = reference.data();
  bool* error_positions_ptr = error_positions.data();

  int num_errors = 0;

  for(int i = 0; i < size; ++i) {

    const T a = input_ptr[i];
    const T b = reference_ptr[i];

    const bool res = !(std::abs(a - b) <= (atol + rtol * std::abs(b)));

    num_errors += res;
    error_positions_ptr[i] = res;
  }

  return num_errors;
}

inline void increment_index(std::vector<int>& index, const std::vector<npy_intp>& shape) noexcept {

  const int size = index.size();
  for(int i = 0; i < size; ++i)
    if(++index[i] < shape[i])
      break;
    else
      index[i] = 0;
}

/// \brief Compute list of errors with elements (index, input_value, reference_value)
template <class T>
inline void compute_error_list(const NumpyArray<T>& input, const NumpyArray<T>& reference,
                               const NumpyArray<bool>& error_positions,
                               std::vector<ErrorList<T>>& error_list) noexcept {
  const int size = input.size();
  const T* input_ptr = input.data();
  const T* reference_ptr = reference.data();
  const bool* error_positions_ptr = error_positions.data();

  const std::vector<npy_intp>& shape = input.shape();
  std::vector<int> index(input.shape().size(), 0);
  int error_list_idx = 0;

  for(int i = 0; i < size; ++i, increment_index(index, shape)) {
    if(error_positions_ptr[i]) {
      error_list[error_list_idx].index = index;
      error_list[error_list_idx].input_value = input_ptr[i];
      error_list[error_list_idx].reference_value = reference_ptr[i];
      error_list_idx++;
    }
  }
}

} // anonymous namespace

/// \brief Compute the list of errors and positions
static PyObject* sdbcoreC_make_error_list_c(PyObject* self, PyObject* args) {
  PyObject* input_field;
  PyArrayObject* input_array;

  PyObject* reference_field;
  PyArrayObject* reference_array;

  PyArrayObject* error_positions_array = NULL;
  PyObject* error_list_object = NULL;

  double atol;
  double rtol;

  //
  // Parse arguments
  //
  if(!PyArg_ParseTuple(args, "OOdd", &input_field, &reference_field, &atol, &rtol))
    return NULL;

  try {

    //
    // Extract numpy arrays
    //
    if(!(input_array =
             (PyArrayObject*)PyArray_FROM_OTF(input_field, NPY_DOUBLE, NPY_ARRAY_IN_ARRAY)))
      throw std::runtime_error("internal error: failed to extract input array");

    if(!(reference_array =
             (PyArrayObject*)PyArray_FROM_OTF(reference_field, NPY_DOUBLE, NPY_ARRAY_IN_ARRAY)))
      throw std::runtime_error("internal error: failed to extract reference array");

    NumpyArray<double> input(input_array);
    NumpyArray<double> reference(reference_array);

    if(input.shape() != reference.shape())
      throw std::runtime_error("internal error: dimension mismatch");

    //
    // Allocate error positions array (boolean array)
    //
    error_positions_array = (PyArrayObject*)PyArray_SimpleNew(PyArray_NDIM(input_array),
                                                              PyArray_DIMS(input_array), NPY_BOOL);
    NumpyArray<bool> error_positions(error_positions_array);

    //
    // Compute error positions
    //
    int num_errors = compute_error_positions(input, reference, error_positions, atol, rtol);

    //
    // Allocate list of errors
    //
    std::vector<ErrorList<double>> error_list(num_errors);
    error_list_object = PyList_New(error_list.size());

    //
    // Compute list of errors
    //
    compute_error_list(input, reference, error_positions, error_list);

    //
    // Prepare return
    //
    for(int i = 0; i < error_list.size(); ++i) {

      PyObject* list_element = PyList_New(3);

      PyObject* index_list = PyList_New(error_list[i].index.size());

      const int index_size = error_list[i].index.size();
      for(int ii = 0; ii < error_list[i].index.size(); ++ii)
        PyList_SetItem(index_list, ii, PyLong_FromLong(error_list[i].index[index_size - 1 - ii]));

      PyList_SetItem(list_element, 0, index_list);
      PyList_SetItem(list_element, 1, PyFloat_FromDouble(error_list[i].input_value));
      PyList_SetItem(list_element, 2, PyFloat_FromDouble(error_list[i].reference_value));

      PyList_SetItem(error_list_object, i, list_element);
    }

  } catch(std::runtime_error& e) {
    PyErr_SetString(PyExc_RuntimeError, e.what());

    Py_XDECREF(input_array);
    Py_XDECREF(reference_array);

    if(error_list_object)
      Py_XDECREF(error_list_object);

    if(error_positions_array)
      Py_XDECREF(error_positions_array);

    return NULL;
  }

  return Py_BuildValue("OO", error_list_object, error_positions_array);
}

//===------------------------------------------------------------------------------------------===//
//     Module definitions
//===------------------------------------------------------------------------------------------===//

// Method specification
static PyMethodDef module_methods[] = {
    {"make_error_list_c", sdbcoreC_make_error_list_c, METH_VARARGS, ""}, {NULL, NULL, 0, NULL}};

// Module specification
static struct PyModuleDef sdbcoreC_module_definition = {
    PyModuleDef_HEAD_INIT, "sdbcoreC", "This module provides C extensions to the sdbcore module.",
    -1, module_methods};

// Initialize the sdbcoreC module
PyMODINIT_FUNC PyInit_sdbcoreC(void) {
  Py_Initialize();
  import_array();
  return PyModule_Create(&sdbcoreC_module_definition);
}
