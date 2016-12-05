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
## Unittest of the sdbcore.SerializerData
##
##===------------------------------------------------------------------------------------------===##

from unittest import TestCase, main

from sdbcore.serializerdatalistener import SerializerDataDirectoryAndPrefixListener
from sdbtest.serializerdatamock import SerializerDataMock
from sdbtest.testlistener import TestListener


class SerializerDataDirectoryAndPrefixListenerTester(SerializerDataDirectoryAndPrefixListener,
                                                     TestListener):
    def __init__(self):
        super().__init__()

    def prefix_changed(self, prefix):
        self.increment_count()

    def directory_changed(self, directory):
        self.increment_count()


class TestSerializerData(TestCase):
    def test_init(self):
        serializer_data = SerializerDataMock("Input")
        self.assertEqual(serializer_data.name, "Input")

    def test_setter(self):
        serializer_data = SerializerDataMock("Input")

        test_listener = SerializerDataDirectoryAndPrefixListenerTester()
        serializer_data.register_as_serializer_data_directory_and_prefix_listener(test_listener)
        self.assertEqual(test_listener.count, 0)

        # Set directory and check if listener gets informed about change
        serializer_data.directory = "dir"
        self.assertEqual(test_listener.count, 1)

        # Set prefix and check if listener gets informed about change
        serializer_data.prefix = "prefix"
        self.assertEqual(test_listener.count, 2)

        self.assertEqual(serializer_data.directory, "dir")
        self.assertEqual(serializer_data.prefix, "prefix")

    def test_make_serializer(self):
        serializer_data = SerializerDataMock("Input", "dir", "prefix")
        serializer_data.add_stencil("Stencil", 2, ["u", "v", "w"])

        self.assertEqual(serializer_data.directory, "dir")
        self.assertEqual(serializer_data.prefix, "prefix")

        self.assertTrue(serializer_data.is_valid())


if __name__ == "__main__":
    main()
