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

import unittest
import numpy as np

class TestsdbcoreC(unittest.TestCase):
    def test_make_error_list_c(self):
        
        atol = 1e-05
        rtol = 1e-06
        
        try:
            from sdbcore.sdbcoreC import make_error_list_c
            
            input_field = np.random.rand(3, 2, 1)
            reference_field = np.random.rand(3, 2, 2)
        
            #
            # Dimension mismatch -> Error
            #
            self.assertRaises(RuntimeError, 
                              make_error_list_c, input_field, reference_field, atol, rtol)
          
            #
            # Success
            #
            input_field = np.random.rand(3, 2, 3)
            reference_field = np.copy(input_field)
            
            error_list, error_positions = make_error_list_c(input_field, reference_field, atol, rtol)
            
            self.assertEqual(len(error_list), 0)
            
            #
            # 2 Failures
            #
            reference_field[2, 1, 2] = -1
            reference_field[1, 0, 2] = -1

            error_list, error_positions = make_error_list_c(input_field, reference_field, atol, rtol)
            
            self.assertEqual(len(error_list), 2)
            self.assertTrue([[2, 1 ,2], input_field[2, 1 ,2], -1] in error_list)
            self.assertTrue([[1, 0 ,2], input_field[1, 0 ,2], -1] in error_list)
            
            
            #
            # Regression
            #
            from sdbcore.error_list import make_error_list_python
            
            self.assertEqual(make_error_list_c(input_field, reference_field, atol, rtol), 
                             make_error_list_python(input_field, reference_field, atol, rtol))
            
        except ImportError as e:
            pass
            
if __name__ == "__main__":
    unittest.main()

