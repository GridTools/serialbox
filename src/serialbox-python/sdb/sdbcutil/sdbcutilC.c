/*===-- sdbcutil.c --------------------------------------------------------------------*- C -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains utility functions for the sdb core library.
 *
\*===------------------------------------------------------------------------------------------===*/

#include <Python.h>
#include <stdlib.h>
#include <stdio.h>

/* Available functions */
static PyObject* sdbcutilC_compute_error_list_c(PyObject* self, PyObject* args);

/* Method specification */
static PyMethodDef module_methods[] = {
    {"compute_error_list_c", sdbcutilC_compute_error_list_c, METH_VARARGS, ""},
    {NULL, NULL, 0, NULL}};

/* Module specification */
static struct PyModuleDef sdbcutilC_module_definition = {
    PyModuleDef_HEAD_INIT, "sdbcutilC", "This module provides C extensions to the sdbcore module.",
    -1, module_methods};

/* Initialize the module */
PyMODINIT_FUNC PyInit_sdbcutilC(void) {
  Py_Initialize();
  return PyModule_Create(&sdbcutilC_module_definition);
}

static PyObject* sdbcutilC_compute_error_list_c(PyObject* self, PyObject* args) {
  puts("hello from C");
  Py_RETURN_NONE;
}
