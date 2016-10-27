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
## Unittest of the core utility functions.
##
##===------------------------------------------------------------------------------------------===##

import unittest

from serialbox import Config


class TestCore(unittest.TestCase):
    def test_config(self):
        config = Config().compile_options
        self.assertTrue("BOOST_VERSION" in config)


if __name__ == "__main__":
    unittest.main()
