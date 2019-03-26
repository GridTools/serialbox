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
## Test sdbcore.StencilData
##
##===------------------------------------------------------------------------------------------===##

from collections import Counter
from unittest import TestCase, main

from sdbcore.stencildata import StencilData
from sdbtest.serializerdatamock import SerializerDataMock


class TestSerializerData(TestCase):
    def test_init(self):
        serializer_data = SerializerDataMock("Input", "dir", "prefix")
        serializer_data.make_serializer_empty()

        stencil_data = StencilData(serializer_data)

        self.assertEqual(stencil_data.serializer.directory, "dir")
        self.assertEqual(stencil_data.serializer.prefix, "prefix")

    def test_update_stencil_and_field_list(self):
        serializer_data = SerializerDataMock("Input", "dir", "prefix")
        serializer_data.add_stencil("s1", 2, ["u", "v", "w"])
        serializer_data.add_stencil("s2", 2, ["u", "v", "w", "tt"])
        serializer_data.add_stencil("s3", 2, [])

        stencil_data = StencilData(serializer_data)

        # Check s1, ...,  s3 are in stencil list
        self.assertTrue(Counter(stencil_data.stencil_list) == Counter(["s1", "s2", "s3"]))

        # Check fields of s1
        stencil_data.set_selected_stencil(stencil_data.stencil_list.index("s1"))
        self.assertTrue(Counter(stencil_data.field_list) == Counter(["u", "v", "w"]))

        # Check fields of s2
        stencil_data.set_selected_stencil(stencil_data.stencil_list.index("s2"))
        self.assertTrue(Counter(stencil_data.field_list) == Counter(["u", "v", "w", "tt"]))

        # Check fields of s2
        stencil_data.set_selected_stencil(stencil_data.stencil_list.index("s3"))
        self.assertFalse(stencil_data.field_list)


if __name__ == "__main__":
    main()
