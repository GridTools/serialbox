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
## Unittest of the serializer.
##
##===------------------------------------------------------------------------------------------===##

from serialbox import Serializer
import unittest

class TestSerializer(unittest.TestCase):
    def test_init(self):
         serializer = Serializer()

if __name__ == "__main__":
    unittest.main()
