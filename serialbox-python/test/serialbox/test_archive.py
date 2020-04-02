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

from serialbox import Archive, SerialboxError


class TestArchive(unittest.TestCase):
    def test_registered_archives(self):
        archives = Archive.registered_archives()
        self.assertTrue("Binary" in archives)

    def test_archive_from_extension(self):
        self.assertEqual(Archive.archive_from_extension("file.dat"), "Binary")
        self.assertRaises(SerialboxError, Archive.archive_from_extension, "file.X")

if __name__ == "__main__":
    unittest.main()
