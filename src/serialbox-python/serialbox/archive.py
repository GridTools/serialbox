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

from ctypes import POINTER, c_char_p

from .common import get_library, extract_string
from .error import invoke
from .metainfomap import ArrayOfStringImpl

lib = get_library()


def register_library(library):
    library.serialboxArchiveGetRegisteredArchives.argtypes = None
    library.serialboxArchiveGetRegisteredArchives.restype = POINTER(ArrayOfStringImpl)

    library.serialboxArchiveGetArchiveFromExtension.argtypes = [c_char_p]
    library.serialboxArchiveGetArchiveFromExtension.restype = c_char_p

class Archive(object):
    """Provide information about the registered archives
    """

    @staticmethod
    def registered_archives():
        """Get a list of strings of the registered archives.

        :return: Registered archives
        :rtype: :class:`list` [:class:`str`]
        """
        array = invoke(lib.serialboxArchiveGetRegisteredArchives)
        list_array = []
        for i in range(array.contents.len):
            list_array += [array.contents.data[i].decode()]
        invoke(lib.serialboxArrayOfStringDestroy, array)
        return list_array


    @staticmethod
    def archive_from_extension(filename):
        """ Deduce the name of the `archive` according to the extension of the `filename`.

        Only the registered archives are taken into account!

        ===========  ========
        Extensions   Archives
        ===========  ========
         .dat, .bin  Binary
         .nc         NetCDF
        ===========  ========

        :param filename: Name of the file
        :type filename: str
        :return: Name of the registered archive matching the file extension
        :rtype: str
        :raises SerialboxError: if extensions is invalid or no registered archive supports it.
        """
        filestr = extract_string(filename)[0]
        return invoke(lib.serialboxArchiveGetArchiveFromExtension, filestr).decode()

register_library(lib)
