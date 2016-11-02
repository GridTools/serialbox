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
## This example demonstrates the asynchronous API of Serialbox which can improve the throughput of
## read operations.
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
# Import Serialbox
#
import serialbox as ser
import numpy as np


def main():
    N = 512; M = 512; K = 80
    savepoint = ser.Savepoint('sp')

    #
    # First, we write some data to disk ...
    #
    serializer_write = ser.Serializer(ser.OpenModeKind.Write, "./async", "Field", "Binary")

    field_1 = np.random.rand(N, M, K)
    field_2 = np.random.rand(N, M, K)
    field_3 = np.random.rand(N, M, K)
    field_4 = np.random.rand(N, M, K)
    field_5 = np.random.rand(N, M, K)
    field_6 = np.random.rand(N, M, K)

    serializer_write.write('field_1', savepoint, field_1)
    serializer_write.write('field_2', savepoint, field_2)
    serializer_write.write('field_3', savepoint, field_3)
    serializer_write.write('field_4', savepoint, field_4)
    serializer_write.write('field_5', savepoint, field_5)
    serializer_write.write('field_6', savepoint, field_6)

    #
    # ... and read it again.
    #
    serializer_read = ser.Serializer(ser.OpenModeKind.Read, "./async", "Field", "Binary")

    start = time.time()

    field_1_rd = serializer_read.read('field_1', savepoint)
    field_2_rd = serializer_read.read('field_2', savepoint)
    field_3_rd = serializer_read.read('field_3', savepoint)
    field_4_rd = serializer_read.read('field_4', savepoint)
    field_5_rd = serializer_read.read('field_5', savepoint)
    field_6_rd = serializer_read.read('field_6', savepoint)

    print("Serializer.read       : %8.2f s" % (time.time() - start))

    #
    # Read operations are usually embarrassingly parallel and we can leverage this parallelism by
    # launching the operations asynchronously. If the archive is not thread-safe or if the library
    # was not configured with `SERIALBOX_ASYNC_API` the method falls back to synchronous execution.
    # To synchronize the tasks in the end, we can add a blocking Serializer.wait_for_all().
    #
    start = time.time()

    field_1_rd_async = serializer_read.read_async('field_1', savepoint)
    field_2_rd_async = serializer_read.read_async('field_2', savepoint)
    field_3_rd_async = serializer_read.read_async('field_3', savepoint)
    field_4_rd_async = serializer_read.read_async('field_4', savepoint)
    field_5_rd_async = serializer_read.read_async('field_5', savepoint)
    field_6_rd_async = serializer_read.read_async('field_6', savepoint)
    serializer_read.wait_for_all()

    print("Serializer.read_async : %8.2f s" % (time.time() - start))

    #
    # Finally, we verify the read operations actually do the same.
    #
    assert(np.allclose(field_1_rd, field_1_rd_async))
    assert(np.allclose(field_2_rd, field_2_rd_async))
    assert(np.allclose(field_3_rd, field_3_rd_async))
    assert(np.allclose(field_4_rd, field_4_rd_async))
    assert(np.allclose(field_5_rd, field_5_rd_async))
    assert(np.allclose(field_6_rd, field_6_rd_async))

    #
    # Remove directory
    #
    import shutil
    shutil.rmtree("./async")

if __name__ == '__main__':
    main()
