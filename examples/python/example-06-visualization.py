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
## This example demonstrates the visualization API for numpy fields.
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
# Import Serialbox
#
from serialbox.visualizer import Visualizer
import numpy as np


def main():
    #
    # Create a random field ...
    #
    field = np.random.rand(32, 32, 80)

    #
    # ... and display it
    #
    vis = Visualizer(field, "field")


if __name__ == '__main__':
    main()
