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
## Unittest of the serializer.
##
##===------------------------------------------------------------------------------------------===##

import logging
import os
import shutil
import unittest

from serialbox import *


class TestSerializer(unittest.TestCase):
    archive = "Binary"

    def set_path(self):
        self.path = os.path.join(os.path.dirname(os.path.realpath(__file__)), "unittest-tmp-dir",
                                 "TestSerializer", self._testMethodName)

    def setUp(self):
        self.set_path()
        logging.debug("Creating: %s" % self.path)
        os.makedirs(self.path, exist_ok=True)

    def tearDown(self):
        logging.debug("Deleting: %s" % self.path)
        shutil.rmtree(self.path)

    def test_init(self):
        #
        # Directory does not exist -> Error
        #
        self.assertRaises(SerialboxError, Serializer, OpenModeKind.Read, self.path, "field",
                          self.archive)

        #
        # Archive not registered -> Error
        #
        self.assertRaises(SerialboxError, Serializer, OpenModeKind.Write, self.path, "field",
                          "wrong-archive")

        #
        # Write
        #
        ser = Serializer(OpenModeKind.Write, self.path, "field", self.archive)
        self.assertEqual(ser.mode, OpenModeKind.Write)
        self.assertEqual(ser.prefix, "field")
        self.assertEqual(ser.directory, self.path)

        #
        # Write (OpenModeKind as int)
        #
        ser = Serializer(OpenModeKind(1), self.path, "field", self.archive)
        self.assertEqual(ser.mode, OpenModeKind.Write)
        self.assertEqual(ser.prefix, "field")
        self.assertEqual(ser.directory, self.path)
        ser.update_meta_data()

        #
        # Read
        #
        ser = Serializer(OpenModeKind.Read, self.path, "field", self.archive)
        self.assertEqual(ser.mode, OpenModeKind.Read)

        #
        # Append
        #
        ser = Serializer(OpenModeKind.Append, self.path, "field", self.archive)
        self.assertEqual(ser.mode, OpenModeKind.Append)

    def test_serialization_status(self):
        self.assertGreaterEqual(Serializer.status(), 0)

        #
        # Disable serialization
        #
        Serializer.disable()
        self.assertEqual(Serializer.status(), -1)

        #
        # Enable serialization
        #
        Serializer.enable()
        self.assertEqual(Serializer.status(), 1)

    def test_savepoint(self):
        ser = Serializer(OpenModeKind.Write, self.path, "field", self.archive)

        #
        # Add savepoints
        #
        sp1 = Savepoint('s1', {"key1": 5.0})
        ser.add_savepoint(sp1)
        self.assertRaises(SerialboxError, ser.add_savepoint, sp1)

        sp2 = Savepoint('s2', {"key1": 1.0})
        ser.add_savepoint(sp2)

        #
        # Get savepoint vector
        #
        savepoints = ser.savepoints()
        self.assertEqual(savepoints[0], sp1)
        self.assertEqual(savepoints[1], sp2)

        self.assertTrue(ser.has_savepoint(sp1))
        self.assertTrue(ser.has_savepoint(sp2))

if __name__ == "__main__":
    unittest.main()
