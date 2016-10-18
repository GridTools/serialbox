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
## This file contains the Python Interface to the ArchiveFactory.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import POINTER

from .common import get_library
from .error import invoke
from .metainfomap import ArrayOfStringImpl

lib = get_library()


def register_library(library):
    library.serialboxArchiveGetRegisteredArchives.argtypes = None
    library.serialboxArchiveGetRegisteredArchives.restype = POINTER(ArrayOfStringImpl)


class Archive(object):
    """Provide information about the registered archives
    """

    @staticmethod
    def registered_archives():
        """Get a list of strings of the registered archives

        :return: Registered archives
        :rtype: list[str]
        """
        array = invoke(lib.serialboxArchiveGetRegisteredArchives)
        list_array = []
        for i in range(array.contents.len):
            list_array += [array.contents.data[i].decode()]
        invoke(lib.serialboxArrayOfStringDestroy, array)
        return list_array


register_library(lib)
