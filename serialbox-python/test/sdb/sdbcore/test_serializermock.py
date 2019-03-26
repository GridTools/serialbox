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
## Test sdbtest.SerializerMock
##
##===------------------------------------------------------------------------------------------===##

from unittest import TestCase, main

from sdbtest.serializermock import SerializerMock, OpenModeKind


class TestSerializerData(TestCase):
    def test_init(self):
        serializer = SerializerMock(OpenModeKind.Read, ".", "stencil")
        self.assertEqual(serializer.mode, OpenModeKind.Read)
        self.assertEqual(serializer.directory, ".")
        self.assertEqual(serializer.prefix, "stencil")

    def test_add_stencil(self):
        serializer = SerializerMock(OpenModeKind.Read, ".", "stencil")

        #
        # Add stencil ...
        #
        serializer.add_stencil("Stencil", 2, ["field1", "field2"])

        self.assertTrue(serializer.global_metainfo.has_key("stencils"))
        self.assertTrue("Stencil" in serializer.global_metainfo["stencils"])

        self.assertEqual(len(serializer.savepoint_list()), 4)
        self.assertEqual(serializer.savepoint_list()[0].name, "Stencil__in")
        self.assertEqual(serializer.savepoint_list()[1].name, "Stencil__out")
        self.assertEqual(serializer.savepoint_list()[2].name, "Stencil__in")
        self.assertEqual(serializer.savepoint_list()[3].name, "Stencil__out")

        for sp in serializer.savepoint_list():
            self.assertTrue("field1" in serializer.fields_at_savepoint(sp))
            self.assertTrue("field2" in serializer.fields_at_savepoint(sp))

        #
        # Add another stencil ...
        #
        serializer.add_stencil("Stencil2", 1, ["field1"])
        self.assertTrue("Stencil" in serializer.global_metainfo["stencils"])
        self.assertTrue("Stencil2" in serializer.global_metainfo["stencils"])


if __name__ == "__main__":
    main()
