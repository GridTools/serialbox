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
## This example demonstrates how to only load parts (slices) of a serialized field. This can
## significantly improve performance if one is only interested in a small part of the data.
##
## This example is also available in all other languages supported by Serialbox.
##
##===------------------------------------------------------------------------------------------===##

#
# First, we have to make sure Python finds the Serialbox module. Alternatively, you can also set the 
# environment variable PYTHONPATH.
#
import os
import sys
import time
sys.path.append(os.path.dirname(os.path.realpath(__file__)) + '/../python')
sys.path.append(os.path.dirname(os.path.realpath(__file__)) + '/../../src/serialbox-python')

#
# Import Serialbox and numpy
#
import serialbox as ser
import numpy as np


def main():
    #
    # Initialize the serializer. At the moment sliced loading is only supported by the Binary 
    # archive
    #
    serializer_write = ser.Serializer(ser.OpenModeKind.Write, "./slice", "field", "Binary")

    #
    # Allocate 3D numpy arrays
    #
    field_in = np.random.rand(512, 512, 80)
    field_out = np.zeros((512, 512, 80))
    
    #
    # Write the numpy array to disk at savepoint `sp`
    #
    start = time.time()
        
    savepoint = ser.Savepoint('sp')
    serializer_write.write('field', savepoint, field_in)
    
    print("Serializer.write      : %8.2f s" % (time.time() - start))
    
    #
    # Initialize a serializer for reading.
    #
    serializer_read = ser.Serializer(ser.OpenModeKind.Read, "./slice", "field", "Binary")
    
    #
    # Assume we are only interested in a certain layer of the data (k = 50), we can use the slice  
    # object (ser.Slice) to encode this information and instruct the serializer to only load
    # the desired data. Note that you still need to allocate memory for the whole field!
    #
    start = time.time()
    
    serializer_read.read_slice('field', savepoint, ser.Slice[:, :, 50], field_out)
    
    print("Serializer.read_slice : %8.2f s" % (time.time() - start))
    assert(np.allclose(field_in[:, :, 50], field_out[:, :, 50]))
    
    #
    # You can of course load the full data and slice it afterwards with numpy which yields the same
    # result, though most likely slower.
    #
    start = time.time()
    
    serializer_read.read('field', savepoint, field_out)
    
    print("Serializer.read       : %8.2f s" % (time.time() - start))
    assert(np.allclose(field_in[:, :, 50], field_out[:, :, 50]))
    
    #
    # Remove directory
    #
    import shutil
    shutil.rmtree("./slice")

if __name__ == '__main__':
    main()

