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
## Unittest of the FieldMetaInfo.
##
##===------------------------------------------------------------------------------------------===##

import unittest

from serialbox import FieldMetaInfo, TypeID, SerialboxError, MetaInfoMap

class TestMetaInfo(unittest.TestCase):
    def test_init(self):
        # Default constructor
        info1 = FieldMetaInfo(TypeID.Float64, [12, 13, 14])
        self.assertEqual(info1.type, TypeID.Float64)
        self.assertEqual(info1.dims, [12, 13, 14])

        info2 = FieldMetaInfo(4, [12, 13, 14])
        self.assertEqual(info2.type, TypeID(4))
        self.assertEqual(info2.dims, [12, 13, 14])

        # Construct with dictionary
        info3 = FieldMetaInfo(TypeID.Float64, [12, 13, 14],  metainfo={"key1": 5.0})
        self.assertEqual(info3.type, TypeID.Float64)
        self.assertEqual(info3.dims, [12, 13, 14])
        self.assertEqual(info3.metainfo.to_dict(), {"key1": 5.0})

        # Construct with MetaInfoMap
        metainfomap = MetaInfoMap({"key1": 5.0})
        info4 = FieldMetaInfo(TypeID.Float64, [12, 13, 14],  metainfo=metainfomap)
        self.assertEqual(info4.type, TypeID.Float64)
        self.assertEqual(info4.dims, [12, 13, 14])
        self.assertEqual(info4.metainfo.to_dict(), {"key1": 5.0})

    def test_type(self):
        info = FieldMetaInfo(TypeID.Float64, [12, 13, 14])
        self.assertEqual(info.type, TypeID.Float64)

    def test_dims(self):
        info = FieldMetaInfo(TypeID.Float64, [12, 13, 14])
        self.assertEqual(info.dims, [12, 13, 14])


    def test_comparison(self):
        # Different dimensions
        info1 = FieldMetaInfo(TypeID.Float64, [12, 13])
        info1_eq = FieldMetaInfo(TypeID.Float64, [12, 13])
        info1_ne = FieldMetaInfo(TypeID.Float64, [50, 50])

        self.assertEqual(info1, info1_eq)
        self.assertNotEqual(info1, info1_ne)

        # Different meta-info
        info2 = FieldMetaInfo(TypeID.Float64, [12, 13], {"key1": [5.0, 5.1]})
        info2_eq = FieldMetaInfo(TypeID.Float64, [12, 13], {"key1": [5.0, 5.1]})
        info2_ne = FieldMetaInfo(TypeID.Float64, [12, 13], {"key1": 10.0})

        self.assertEqual(info2, info2_eq)
        self.assertNotEqual(info2, info2_ne)

    def test_metainfo(self):
        info = FieldMetaInfo(TypeID.Float64, [12, 13, 14])

        # Insert keys
        info.metainfo.insert("key1", 5.0)
        info.metainfo.insert("key2", "str")
        info.metainfo.insert("key3", 1)
        self.assertEqual(info.metainfo.size(), 3)

        # Check keys via aliasing
        metainfomap = info.metainfo
        self.assertEqual(metainfomap["key1"], 5.0)
        self.assertEqual(metainfomap["key2"], "str")
        self.assertEqual(metainfomap["key3"], 1)

    def test_clone(self):
        info_to_clone = FieldMetaInfo(TypeID.Float64, [12, 13], {"key1": 5.0})
        info_clone = info_to_clone.clone()
        self.assertEqual(info_to_clone, info_clone)

        # Clear meta-info of 'info_to_clone', assert 'info_clone' still has the meta-info
        info_to_clone.metainfo.clear()
        self.assertEqual(info_clone.metainfo.to_dict(), {"key1": 5.0})

if __name__ == "__main__":
    unittest.main()