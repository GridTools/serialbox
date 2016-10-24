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

import numpy as np

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
        ser = Serializer(OpenModeKind.Write.value, self.path, "field", self.archive)
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

    def test_global_meta_info(self):
        ser_write = Serializer(OpenModeKind.Write, self.path, "field", self.archive)

        ser_write.global_metainfo.insert("key1", 5.0)
        ser_write.global_metainfo.insert("key2", 1)
        ser_write.global_metainfo.insert("key3", "str")
        ser_write.update_meta_data()

        ser_append = Serializer(OpenModeKind.Append, self.path, "field", self.archive)
        self.assertEqual(ser_append.global_metainfo["key1"], 5.0)
        self.assertEqual(ser_append.global_metainfo["key2"], 1)
        self.assertEqual(ser_append.global_metainfo["key3"], "str")

    def test_serialization_status(self):
        self.assertGreaterEqual(Serializer.status(), 0)

        #
        # Disable serialization
        #
        Serializer.disable()
        self.assertEqual(Serializer.status(), Serializer.Disabled)

        #
        # Enable serialization
        #
        Serializer.enable()
        self.assertEqual(Serializer.status(), Serializer.Enabled)

    def test_savepoint(self):
        ser = Serializer(OpenModeKind.Write, self.path, "field", self.archive)

        #
        # Add savepoints
        #
        sp1 = Savepoint('s1')
        ser.register_savepoint(sp1)
        self.assertRaises(SerialboxError, ser.register_savepoint, sp1)

        sp2 = Savepoint('sp', {"key1": 1.0})
        ser.register_savepoint(sp2)

        sp3 = Savepoint('sp', {"key1": 2.0})
        ser.register_savepoint(sp3)

        #
        # Get savepoint vector
        #
        savepoints = ser.savepoints()
        self.assertEqual(savepoints[0], sp1)
        self.assertEqual(savepoints[1], sp2)
        self.assertEqual(savepoints[2], sp3)

        self.assertTrue(ser.has_savepoint(sp1))
        self.assertTrue(ser.has_savepoint(sp2))
        self.assertTrue(ser.has_savepoint(sp3))

        #
        # Get all savepoints with `name`
        #
        savepoints_with_name = ser.get_savepoint("sp")
        self.assertTrue(sp2 in savepoints_with_name)
        self.assertTrue(sp3 in savepoints_with_name)

    def test_field(self):
        ser_write = Serializer(OpenModeKind.Write, self.path, "field", self.archive)

        field1 = FieldMetaInfo(TypeID.Float64, [12, 13, 14])
        field2 = FieldMetaInfo(TypeID.Float32, [1024, 1024])
        field3 = FieldMetaInfo(TypeID.Int32, [4096])

        #
        # Add fields
        #
        ser_write.register_field("field1", field1)
        self.assertTrue(ser_write.has_field("field1"))

        ser_write.register_field("field2", field2)
        self.assertTrue(ser_write.has_field("field2"))

        ser_write.register_field("field3", field3)
        self.assertTrue(ser_write.has_field("field3"))

        # Add existing field -> Error
        self.assertRaises(SerialboxError, ser_write.register_field, "field1", field1)

        # Query non-existing field
        self.assertFalse(ser_write.has_field("non-existing"))

        #
        # Fieldnames
        #
        fieldnames = ser_write.fieldnames()
        self.assertTrue("field1" in fieldnames)
        self.assertTrue("field2" in fieldnames)
        self.assertTrue("field3" in fieldnames)

        #
        # Write meta-data to disk
        #
        ser_write.update_meta_data()

        #
        # Query field-meta-info
        #
        ser_read = Serializer(OpenModeKind.Read, self.path, "field", self.archive)

        self.assertEqual(ser_read.get_field_metainfo("field1"), field1)
        self.assertEqual(ser_read.get_field_metainfo("field2"), field2)
        self.assertEqual(ser_read.get_field_metainfo("field2"), field2)

    def test_write_and_read_implicit(self):
        ser_write = Serializer(OpenModeKind.Write, self.path, "field", self.archive)

        #
        # Setup fields
        #
        field1_input = np.random.rand(16)
        field2_input = np.random.rand(4, 4)
        field3_input = np.random.rand(2, 2, 2)
        sp = Savepoint("sp")

        #
        # Write fields (implicitly register savepoint and fields)
        #
        ser_write.write("field1", sp, field1_input)
        ser_write.write("field2", sp, field2_input)
        ser_write.write("field3", sp, field3_input)

        #
        # Read fields
        #
        ser_read = Serializer(OpenModeKind.Read, self.path, "field", self.archive)
        field1_output = ser_read.read("field1", sp)
        field2_output = ser_read.read("field2", sp)
        field3_output = ser_read.read("field3", sp)

        #
        # Validate
        #
        self.assertTrue(np.allclose(field1_input, field1_output))
        self.assertTrue(np.allclose(field2_input, field2_output))
        self.assertTrue(np.allclose(field3_input, field3_output))

        #
        # Failures
        #

        # Reading with OpenModeKind.Write -> Error
        self.assertRaises(SerialboxError, ser_write.read, "field1", sp)

        # Writing with OpenModeKind.Read -> Error
        self.assertRaises(SerialboxError, ser_read.write, "field1", field1_input, sp)

        # Read non-existent field -> Error
        self.assertRaises(SerialboxError, ser_read.read, "field-non-existent", sp)

        # Read at non-existent savepoint -> Error
        self.assertRaises(SerialboxError, ser_read.read, "field1", Savepoint('sp2'))

    def test_write_and_read_explict(self):
        ser_write = Serializer(OpenModeKind.Write, self.path, "field", self.archive)
        N = 5

        #
        # Setup fields
        #
        field_bool = np.ndarray(dtype=np.bool, shape=[N, N, N])
        field_int32 = np.ndarray(dtype=np.int32, shape=[N, N, N])
        field_int64 = np.ndarray(dtype=np.int64, shape=[N, N, N])
        field_float32 = np.ndarray(dtype=np.float32, shape=[N, N, N])
        field_float64 = np.ndarray(dtype=np.float64, shape=[N, N, N])
        for i in range(N):
            for j in range(N):
                for k in range(N):
                    rnd = np.random.rand(1)
                    field_bool[i, j, k] = True if rnd > 0.5 else False
                    field_int32[i, j, k] = 100 * rnd
                    field_int64[i, j, k] = 100 * rnd
                    field_float32[i, j, k] = rnd
                    field_float64[i, j, k] = rnd

        #
        # Register field
        #
        register_field = lambda n, t, f: ser_write.register_field(n, FieldMetaInfo(t, f.shape))

        register_field("field_bool", TypeID.Boolean, field_bool)
        register_field("field_int32", TypeID.Int32, field_int32)
        register_field("field_int64", TypeID.Int64, field_int64)
        register_field("field_float32", TypeID.Float32, field_float32)
        register_field("field_float64", TypeID.Float64, field_float64)

        #
        # Register savepoint
        #
        sp_bool = Savepoint("sp_bool")
        sp_ints = Savepoint("sp_ints")
        sp_floats = Savepoint("sp_floats")
        ser_write.register_savepoint(sp_bool)
        ser_write.register_savepoint(sp_ints)
        ser_write.register_savepoint(sp_floats)

        #
        # Write
        #
        ser_write.write("field_bool", sp_bool, field_bool, False)
        ser_write.write("field_int32", sp_ints, field_int32, False)
        ser_write.write("field_int64", sp_ints, field_int64, False)
        ser_write.write("field_float32", sp_floats, field_float32, False)
        ser_write.write("field_float64", sp_floats, field_float64, False)

        #
        # Read fields
        #
        ser_read = Serializer(OpenModeKind.Read, self.path, "field", self.archive)
        field_bool_output = ser_read.read("field_bool", sp_bool)
        field_int32_output = ser_read.read("field_int32", sp_ints)
        field_int64_output = ser_read.read("field_int64", sp_ints)
        field_float32_output = ser_read.read("field_float32", sp_floats)
        field_float64_output = ser_read.read("field_float64", sp_floats)

        #
        # Validate
        #
        self.assertTrue(np.allclose(field_bool_output, field_bool))
        self.assertTrue(np.allclose(field_int32_output, field_int32))
        self.assertTrue(np.allclose(field_int64_output, field_int64))
        self.assertTrue(np.allclose(field_float32_output, field_float32))
        self.assertTrue(np.allclose(field_float64_output, field_float64))

    def test_stateless_serialization(self):
        field_input = np.random.rand(2, 2, 2)
        field_output = np.random.rand(2, 2, 2)

        #
        # Read & write from file (Binary archive)
        #
        Serializer.to_file("field", field_input, os.path.join(self.path, "test.dat"))
        Serializer.from_file("field", field_output, os.path.join(self.path, "test.dat"))
        self.assertTrue(np.allclose(field_input, field_output))

        #
        # Read & write from file (NetCDF archive)
        #
        if "SERIALBOX_HAS_NETCDF" in Config().compile_options:
            Serializer.to_file("field", field_input, os.path.join(self.path, "test.nc"))
            Serializer.from_file("field", field_output, os.path.join(self.path, "test.nc"))
            self.assertTrue(np.allclose(field_input, field_output))

        #
        # Invalid file extension
        #
        self.assertRaises(SerialboxError, Serializer.to_file, "field", field_input, "test.X")

if __name__ == "__main__":
    unittest.main()
