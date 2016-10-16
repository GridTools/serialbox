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
## This file contains the savepoint implementation of the Python Interface.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import c_char_p, c_void_p, c_int

from .common import get_library, extract_string
from .error import invoke

lib = get_library()


def register_library(library):
    library.serialboxSavepointCreate.argtypes = [c_char_p]
    library.serialboxSavepointCreate.restype = c_void_p
    library.serialboxSavepointCreateFromSavepoint.restype = c_void_p
    library.serialboxSavepointGetName.restype = c_char_p
    library.serialboxSavepointEqual.restype = c_int
    library.serialboxSavepointToString.restype = c_char_p


class Savepoint(object):
    """Savepoint implementation of the Python Interface.

    Savepoints are primarily identified by their `name` and further distinguished by their
    `meta_info`. Savepoints are used within the :class:`Serializer` to discriminate fields
    at different points in time.
    """

    def __init__(self, name, metainfo=None):
        """Initialize the Savepoint.

        This method prepares the savepoint for usage and gives a name, which is the only required
        information for the savepoint to be usable. Meta-information can be added after the
        initialization has been performed.

        :param name: str -- Name of the savepoint
        :param metainfo: tuple -- key=value pairs to add to the meta-information of the Savepoint
        :raises: SerialboxError -- Savepoint could not be initialized
        """
        namestr = extract_string(name)[0]
        self.__savepoint = c_void_p(invoke(lib.serialboxSavepointCreate, namestr))

    @property
    def name(self):
        """Name of the savepoint.
        """
        return invoke(lib.serialboxSavepointGetName, self.__savepoint).decode()

    def clone(self):
        """Clone the Savepoint by performing a deepcopy
        """
        clone = Savepoint('')
        clone.__savepoint = c_void_p(
            invoke(lib.serialboxSavepointCreateFromSavepoint, self.__savepoint))
        return clone

    def __eq__(self, other):
        return invoke(lib.serialboxSavepointEqual, self.__savepoint, other.__savepoint)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __del__(self):
        invoke(lib.serialboxSavepointDestroy, self.__savepoint)

    def __repr__(self):
        return '<savepoint {0}>'.format(self.name)

    def __str__(self):
        return invoke(lib.serialboxSavepointToString, self.__savepoint).decode()


register_library(lib)
