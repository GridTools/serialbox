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
## This example demonstrates how to setup a Serializer, add global meta-information, register
## fields and savepoints and serialize/deserialize numpy arrays using the Python interface of
## Serialbox.
##
## In this small example we will repeatedly apply a two dimensional laplacian stencil to an input
## field `phi`. Before and after each invocation of the laplacian stencil, we will serialize the
## data to disk.
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
sys.path.append(os.path.dirname(__file__) + '../../src/serialbox-python')
sys.path.append(os.path.dirname(__file__) + '../../python')

#
# Import Serialbox and numpy
#
import serialbox as ser
import numpy as np


def laplacian_stencil(phi):
    """ Compute the 2D laplacian on the inner domain of field `phi` and return result in `lap`
    """
    lap = np.zeros_like(phi)
    N, M = phi.shape
    for i in range(1, N - 1):
        for j in range(1, M - 1):
            lap[i, j] = phi[i + 1, j] + phi[i - 1, j] + phi[i, j + 1] + phi[i, j - 1] \
                        - 4 * phi[i, j]
    return lap

##===------------------------------------------------------------------------------------------===##
##  write()
##
## In this function we first prepare the Serializer for writing, add some global meta-information
## and register the fields `phi` and `lap`. Later, we apply the `laplacianStencil` to `phi` and
## `lap` and serialize every iteration `phi` as an input and `lap` as an output of the stencil.
##
##===------------------------------------------------------------------------------------------===##
def write():

    #
    # Create a Serializer for writing. Besides the open-policy we have to specify the `directory`
    # in which the Serializer is created and the `prefix` of all files. In case the directory does
    # not exist, it will be created. In addition, if the directory is not empty, all fields with the 
    # same `prefix` will be erased (this behaviour can be inhibited using the Append mode). 
    #
    serializer = ser.Serializer(ser.OpenModeKind.Write, "./laplacian/", "field")

    #
    # Allocate a 2D numpy array and fill it with some random numbers
    #
    phi = np.random.rand(10, 10)

    #
    # Register the field within the Serializer. Note that for the Python Interface this step is not
    # strictly necessary as it can be done implicitly in the write method.
    #
    fieldmetainfo = ser.FieldMetaInfo(ser.TypeID.Float64, phi.shape)
    serializer.register_field('phi', fieldmetainfo)

    #
    # Add some global meta-information to the serializer. Besides the usual `key = value` pair,
    # you can also add `key = {value1, ..., valueN}` pairs.
    #
    serializer.global_metainfo.insert('answer', 42)
    serializer.global_metainfo.insert('halos', [1, 1, 1, 1])

    #
    # Up to this point nothing has been written to disk. Using update_meta_data() will force a write
    # all meta-information to the corresponding JSON files. Note that the meta-data is updated after
    # each call and thus a manual update of the meta-data is seldom required. If you are curious you
    # can inspect the files './laplacian/MetaData-field.json' and
    # './laplacian/ArchiveMetaData-field.json'
    #
    serializer.update_meta_data()

    #
    # We now apply the `laplacian_stencil` three times to phi. In each iteration we will create an
    # input and output savepoint where we save the current `phi` field (input) and `lap` field
    # (output)
    #
    for t in range(3):
        #
        # Create a Savepoint. Savepoints can have the same name as long as they have different
        # meta-information. In our case we will always store the current time step `t` as a
        # meta-information, thus making it unique.
        #
        savepoint_in = ser.Savepoint('laplacian-in')
        savepoint_in.metainfo.insert('time', t)

        #
        # Register the Savepoint.
        #
        serializer.register_savepoint(savepoint_in)

        #
        # Write phi to disk at our input savepoint. This will create the file `field_phi.dat` upon
        # first invocation and afterwards the data is appended.
        #
        serializer.write('phi', phi, savepoint_in)

        #
        # Apply the laplacian_stencil to phi
        #
        lap = laplacian_stencil(phi)

        #
        # Create the output savepoint. This time we directly initialize the meta-information of the
        # savepoint.
        #
        savepoint_out = ser.Savepoint('laplacian-out', {'time': t})

        #
        # Write lap to disk. Note that here we implicitly register the field `lap` upon first
        # invocation. Same goes for the output savepoint.
        #
        serializer.write('lap', lap, savepoint_out)

        #
        # Finally, we swap phi with lap
        #
        phi = lap



##===------------------------------------------------------------------------------------------===##
##  read()
##
## In this function we initialize the Serializer for reading with our serialized data from the 
## write() method. First, we query some meta-data, like the global meta-information, the 
## dimensions of field `phi` or the vector of savepoints. 
## Afterwards, we apply the same three time steps of the `laplacianStencil` to `phi` to compute 
## `lap` but this time we compare the result (i.e the content of `lap`) to the  to the reference 
## loaded from disk `lap_reference` which we computed in the write() method. Obviously, the results 
## will match as we apply the exact same stencil but in a real world scenario you might use a 
## different implementations of the stencil and this is where Serialbox has it's use case. 
##
##===------------------------------------------------------------------------------------------===##
def read():

    #
    # Create a Serializer for reading. This gives access to the previously written data. 
    #
    serializer = ser.Serializer(ser.OpenModeKind.Read, "./laplacian/", "field")

    #
    # Access the global meta-information
    #    
    print("The answer is %s" % serializer.global_metainfo["answer"])
    print("The halo boundaries are %s" % serializer.global_metainfo["halos"])

    #
    # Access the field meta-information
    # 
    print("The registered fields are: %s" % serializer.fieldnames())
    print("Dimensions of phi: %s" % serializer.get_field_metainfo("phi").dims)

    #
    # Access the savepoints. The savepoints are ordered in the order they were inserted. For a more
    # elaborate example on how to query savepoints, see example-02-savepoints.py.
    #
    savepoints = serializer.savepoints()
    print("Savepoints:")
    for sp in savepoints:
        print(" ", sp)

    #
    # We will now perform the same iterations as in the write method but this time we will read
    # phi as an input from disk, compute the laplacian and compare the result to the stored output
    # of lap on disk (loaded as `lap_refrence`).
    #
    for t in range(3):
        #
        # Get the current input savepoint at time t (the factor of 2 is due to the fact that we 
        # stored input and output in alternating order).
        #
        savepoint_in = savepoints[2 * t]

        #
        # Load phi from disk. A numpy.array with the correct type and shape will be automatically 
        # allocated.
        #
        phi = serializer.read('phi', savepoint_in)

        #
        # Apply the laplacian_stencil to phi
        #
        lap = laplacian_stencil(phi)

        #
        # Load the refrence output of lap ...
        #
        savepoint_out = savepoints[2 * t + 1]
        lap_reference = serializer.read('lap', savepoint_out)

        #
        # ... and compare the results.
        #
        assert (np.allclose(lap, lap_reference))


##===------------------------------------------------------------------------------------------===##
##  main()
##
## Here we call our write() and read() functions.
##
##===------------------------------------------------------------------------------------------===##
def main():

    write()

    read()

if __name__ == '__main__':
    main()

