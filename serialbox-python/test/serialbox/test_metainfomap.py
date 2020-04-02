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
## Unittest of the Metainfo.
##
##===------------------------------------------------------------------------------------------===##

import unittest

from serialbox import MetainfoMap, TypeID, SerialboxError


class TestMetainfo(unittest.TestCase):
    def test_init(self):
        #
        # Default constructor
        #
        map1 = MetainfoMap()
        self.assertTrue(map1.empty())
        self.assertEqual(map1.size(), 0)

        #
        # Dictionary constructor
        #
        map2 = MetainfoMap({"bool": True,
                            "int": 2,
                            "double": 5.1,
                            "string": "str"})

        self.assertFalse(map2.empty())

        self.assertTrue(map2.has_key("bool"))
        self.assertTrue(map2.has_key("int"))
        self.assertTrue(map2.has_key("double"))
        self.assertTrue(map2.has_key("string"))

        self.assertEqual(map2["bool"], True)
        self.assertEqual(map2["int"], 2)
        self.assertEqual(map2["double"], 5.1)
        self.assertEqual(map2["string"], "str")

    def test_insert(self):
        map = MetainfoMap()

        #
        # Insert keys (deduced typeid)
        #
        map.insert("bool", True)
        map.insert("int", 2)
        map.insert("double", 5.1)
        map.insert("string", "str")

        map.insert("bool_array", [True, False, True])
        map.insert("bool_int", [1, 2, 3, 4])
        map.insert("bool_double", [2.1, 5.2, 5.3])
        map.insert("bool_string", ["str1", "str2"])

        self.assertEqual(map["bool"], True)
        self.assertEqual(map["int"], 2)
        self.assertEqual(map["double"], 5.1)
        self.assertEqual(map["string"], "str")

        self.assertEqual(map["bool_array"], [True, False, True])
        self.assertEqual(map["bool_int"], [1, 2, 3, 4])
        self.assertEqual(map["bool_double"], [2.1, 5.2, 5.3])
        self.assertEqual(map["bool_string"], ["str1", "str2"])

        map.clear()

        #
        # Insert keys (explicit typeid)
        #
        map.insert("bool", True, TypeID.Boolean)
        map.insert("int32", 32, TypeID.Int32)
        map.insert("int64", 64, TypeID.Int64)
        map.insert("float32", 32.32, TypeID.Float32)
        map.insert("float64", 64.64, TypeID.Float64)
        map.insert("string", "str", TypeID.String)

        map.insert("array_bool", [True, False], TypeID.ArrayOfBoolean)
        map.insert("array_int32", [32, 33], TypeID.ArrayOfInt32)
        map.insert("array_int64", [64, 65], TypeID.ArrayOfInt64)
        map.insert("array_float32", [32, 33], TypeID.ArrayOfFloat32)
        map.insert("array_float64", [64.64, 65.65], TypeID.ArrayOfFloat64)
        map.insert("array_string", ["str1", "str2"], TypeID.ArrayOfString)

        self.assertEqual(map["bool"], True)
        self.assertEqual(map["int32"], 32)
        self.assertEqual(map["int64"], 64)
        self.assertAlmostEqual(map["float32"], 32.32, places=4)
        self.assertEqual(map["float64"], 64.64)
        self.assertEqual(map["string"], "str")

        self.assertEqual(map["array_bool"], [True, False])
        self.assertEqual(map["array_int32"], [32, 33])
        self.assertEqual(map["array_int64"], [64, 65])
        self.assertEqual(map["array_float32"], [32, 33])
        self.assertEqual(map["array_float64"], [64.64, 65.65])
        self.assertEqual(map["array_string"], ["str1", "str2"])

        map.clear()

        #
        # Insert existing key -> Error
        #
        map.insert("key", "value")
        self.assertRaises(SerialboxError, map.insert, "key", "value")

        #
        # TypeID wrong type -> Error
        #
        self.assertRaises(TypeError, map.insert, "key2", "value2", "not-a-typeid")

    def test_clear(self):
        map = MetainfoMap()
        map.insert("key", 1)

        self.assertFalse(map.empty())
        map.clear()
        self.assertTrue(map.empty())

    def test_clone(self):
        map_to_clone = MetainfoMap()
        map_to_clone.insert("key", 1)

        map_clone = map_to_clone.clone()
        self.assertEqual(map_clone, map_to_clone)

        # Clear 'map_to_clone', assert 'map_clone' is unaffected
        map_to_clone.clear()
        self.assertTrue(map_to_clone.empty())
        self.assertFalse(map_clone.empty())
        self.assertEqual(map_clone["key"], 1)

    def test_get_dict(self):
        map = MetainfoMap()
        map.insert("key1", 1)
        map.insert("key2", 2)
        map.insert("key_array", [1, 2, 3, 4])

        d = map.to_dict()
        self.assertEqual(len(d), map.size())
        self.assertEqual(d["key1"], 1)
        self.assertEqual(d["key2"], 2)
        self.assertEqual(d["key_array"], [1, 2, 3, 4])

    def test_to_string(self):
        map = MetainfoMap()
        map.insert("key1", 1)

        mapstr = str(map)
        self.assertTrue("key" in mapstr)
        self.assertTrue("1" in mapstr)

    def test_comparison(self):
        m1 = MetainfoMap()
        m2 = MetainfoMap()

        m1.insert("key1", 1)
        m2.insert("key1", 2)

        self.assertEqual(m1, m1)
        self.assertNotEqual(m1, m2)

    def test_iteration(self):
        dic = {"bool": True,
               "int": 2,
               "double": 5.1,
               "string": "str"}

        map = MetainfoMap(dic)
        for key, value in map:
            self.assertEqual(value, dic[key])

    def test_deletion(self):
        map = MetainfoMap()
        map.insert("key", 2)

        self.assertTrue(map.has_key("key"))
        del map["key"]
        self.assertFalse(map.has_key("key"))


if __name__ == "__main__":
    unittest.main()
