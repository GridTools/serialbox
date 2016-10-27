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
## This file contains the slicing functionality of the serializer.
##
##===------------------------------------------------------------------------------------------===##


class _Slice(object):
    """Slice implementation of the Python Interface.
    """

    def __init__(self):
        pass

    def __getitem__(self, slice):
        """ Define a slice.

        :return: Tuple of slices
        :rtype: tuble[slice]
        """
        return slice


Slice = _Slice()
