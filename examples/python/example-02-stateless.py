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
## This example demonstrates how to use stateless serializations i.e serialize fields without the
## need to register fields or savpoints. In addition, the usage of the logging infrastructure 
## is exemplified.
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
sys.path.append(os.path.dirname(os.path.realpath(__file__)) + '/../python')
sys.path.append(os.path.dirname(os.path.realpath(__file__)) + '/../../src/serialbox-python')

#
# Import Serialbox and numpy
#
import serialbox as ser
import numpy as np

def main():

    #
    # Allocate 3D numpy arrays
    #
    field_in = np.random.rand(10, 15, 20)
    field_out = np.random.rand(10, 15, 20)
    
    #
    # Enable logging, by default it is turned off
    #
    ser.Logging.enable()

    #
    # Write the numpy array to disk. The archive will be deduced from the file extension (here 
    # ".dat" implies the Binary archive). This method has the same purpose as the built-in numpy 
    # method numpy.save but the written data can be used in C, C++ and Fortran.
    #
    ser.Serializer.to_file("field", field_in, "field.dat")
    
    #
    # Read the written field from file (note that this method performs no consistency checks so you
    # have to know what you are doing).
    #
    ser.Serializer.from_file("field", field_out, "field.dat")

    #
    # Verify the result
    #
    assert(np.allclose(field_in, field_out))

    #
    # It might be more intresting to use different archives. In this example we use the NetCDF 
    # archive (extension ".nc") if the library supports it. Note that you can know also read this 
    # file with the python module of NetCDF-4
    #
    if "SERIALBOX_HAS_NETCDF" in ser.Config().compile_options:
        ser.Serializer.to_file("field", field_in, "field.nc")
        ser.Serializer.from_file("field", field_out, "field.nc")
        assert(np.allclose(field_in, field_out))

if __name__ == '__main__':
    main()

