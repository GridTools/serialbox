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
    """Specification of the slice indices which are used for partial loading of serialized data.

    To avoid instantiation, use the global object serialbox.Slice.

      >>> Slice[:, :, 5]
      (slice(None, None, None), slice(None, None, None), 5)

    See `here <https://docs.scipy.org/doc/numpy/reference/arrays.indexing.html>`_.
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
