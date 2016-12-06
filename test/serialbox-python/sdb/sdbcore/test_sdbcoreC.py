#!/usr/bin/python3
# -*- coding: utf-8 -*-
##===-----------------------------------------------------------------------------*- Python -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license.
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
##
## Unittest of the sdbcore C extensions.
##
##===------------------------------------------------------------------------------------------===##

from unittest import TestCase, main

import numpy as np


class TestsdbcoreC(TestCase):
    def test_make_error_list_c(self):

        atol = 1e-05
        rtol = 1e-06

        try:
            from sdbcore.sdbcoreC import make_error_list_c
        except ImportError as e:
            return
            pass

        input_field = np.random.rand(12, 13, 14)
        reference_field = np.random.rand(12, 13, 1)

        #
        # Dimension mismatch -> Error
        #
        self.assertRaises(RuntimeError,
                          make_error_list_c, input_field, reference_field, atol, rtol)

        #
        # Success
        #
        input_field = np.random.rand(12, 13, 14)
        reference_field = np.copy(input_field)

        error_list, error_positions = make_error_list_c(input_field, reference_field, atol,
                                                        rtol)

        self.assertEqual(len(error_list), 0)

        #
        # 2 Failures
        #
        reference_field[2, 1, 3] = -1
        reference_field[1, 0, 2] = -1

        error_list, error_positions = make_error_list_c(input_field, reference_field, atol,
                                                        rtol)

        self.assertEqual(len(error_list), 2)

        self.assertTrue([[2, 1, 3], input_field[2, 1, 3], -1] in error_list)
        self.assertTrue([[1, 0, 2], input_field[1, 0, 2], -1] in error_list)

        #
        # Regression
        #
        from sdbcore.errorlist import make_error_list_python

        py_error_list, py_error_positions = make_error_list_python(input_field, reference_field, atol,
                                                        rtol)

        self.assertTrue(np.array_equal(error_positions, py_error_positions))

        for c_error, py_error in zip(error_list, py_error_list):
            self.assertEqual(list(c_error[0]), list(py_error[0]))
            self.assertEqual(c_error[1], py_error[1])
            self.assertEqual(c_error[2], py_error[2])

if __name__ == "__main__":
    main()
