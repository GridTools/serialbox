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
## Unittest of the Savepoint.
##
##===------------------------------------------------------------------------------------------===##

import unittest

from serialbox import Savepoint, MetaInfoMap

class TestSavepoint(unittest.TestCase):
    def test_init(self):
        # Default constructor
        sp1 = Savepoint("sp1")
        sp2 = Savepoint(name="sp2")

        # Construct with meta_info


    def test_get_name(self):
        sp = Savepoint("s")
        self.assertEqual(sp.name, "s")

    def test_equal(self):
        sp = Savepoint("s")
        sp_eq = Savepoint("s")
        sp_ne = Savepoint("s-XXX")

        self.assertEqual(sp, sp_eq)
        self.assertNotEqual(sp, sp_ne)

    def test_not_equal(self):
        sp = Savepoint("s")
        sp_eq = Savepoint("s")
        sp_ne = Savepoint("s-XXX")

        self.assertEqual(sp, sp_eq)
        self.assertNotEqual(sp, sp_ne)

    def test_clone(self):
        sp_to_clone = Savepoint("sp_to_copy")
        sp_clone = sp_to_clone.clone()
        self.assertEqual(sp_to_clone, sp_clone)
        self.assertEqual(sp_to_clone.name, sp_clone.name)

if __name__ == "__main__":
    unittest.main()
