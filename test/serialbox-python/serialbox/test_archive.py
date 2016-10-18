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
## Unittest of the archive utility functions.
##
##===------------------------------------------------------------------------------------------===##

import unittest

from serialbox import Archive


class TestArchive(unittest.TestCase):
    def test_archive(self):
        archives = Archive.registered_archives()
        self.assertTrue("Binary" in archives)

if __name__ == "__main__":
    unittest.main()
