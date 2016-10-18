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
        self.assertEqual(sp1.name, "sp1")

        sp2 = Savepoint(name="sp2")
        self.assertEqual(sp2.name, "sp2")

        # Construct with dictionary
        sp3 = Savepoint('sp3', metainfo={"key1": 5.0})
        self.assertEqual(sp3.metainfo.to_dict(), {"key1": 5.0})

        # Construct with MetaInfoMap
        metainfomap = MetaInfoMap({"key1": 5.0})
        sp4 = Savepoint('sp4', metainfo=metainfomap)
        self.assertEqual(sp4.metainfo.to_dict(), {"key1": 5.0})

    def test_get_name(self):
        sp = Savepoint("s")
        self.assertEqual(sp.name, "s")

    def test_comparison(self):
        # Different names
        sp1 = Savepoint("s")
        sp1_eq = Savepoint("s")
        sp1_ne = Savepoint("s-XXX")

        self.assertEqual(sp1, sp1_eq)
        self.assertNotEqual(sp1, sp1_ne)

        # Same names, different meta-info
        sp2 = Savepoint("s", {"key1": 5.0})
        sp2_eq = Savepoint("s", {"key1": 5.0})
        sp2_ne = Savepoint("s", {"key1": 10.0})

        self.assertEqual(sp1, sp1_eq)
        self.assertNotEqual(sp1, sp1_ne)

    def test_metainfo(self):
        sp = Savepoint("s")

        # Insert keys
        sp.metainfo.insert("key1", 5.0)
        sp.metainfo.insert("key2", "str")
        sp.metainfo.insert("key3", 1)
        self.assertEqual(sp.metainfo.size(), 3)

        # Check keys via aliasing
        metainfomap = sp.metainfo
        self.assertEqual(metainfomap["key1"], 5.0)
        self.assertEqual(metainfomap["key2"], "str")
        self.assertEqual(metainfomap["key3"], 1)

    def test_clone(self):
        sp_to_clone = Savepoint("sp_to_copy", metainfo={"key1": 5.0})
        sp_clone = sp_to_clone.clone()
        self.assertEqual(sp_to_clone, sp_clone)

        # Clear meta-info of 'sp_to_clone', assert 'sp_clone' still has the meta-info
        sp_to_clone.metainfo.clear()
        self.assertEqual(sp_clone.metainfo.to_dict(), {"key1": 5.0})


if __name__ == "__main__":
    unittest.main()
