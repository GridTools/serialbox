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

from ctypes import c_char_p, c_void_p, c_int, Structure, POINTER

from .common import get_library, extract_string
from .error import invoke
from .metainfomap import MetaInfoMap, MetaInfoImpl

lib = get_library()


class SavepointImpl(Structure):
    """ Mapping of serialboxSavepoint_t """
    _fields_ = [("impl", c_void_p), ("ownsData", c_int)]


def register_library(library):
    library.serialboxSavepointCreate.argtypes = [c_char_p]
    library.serialboxSavepointCreate.restype = POINTER(SavepointImpl)

    library.serialboxSavepointCreateFromSavepoint.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointCreateFromSavepoint.restype = POINTER(SavepointImpl)

    library.serialboxSavepointDestroy.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointDestroy.restype = None

    library.serialboxSavepointGetName.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointGetName.restype = c_char_p

    library.serialboxSavepointEqual.argtypes = [POINTER(SavepointImpl), POINTER(SavepointImpl)]
    library.serialboxSavepointEqual.restype = c_int

    library.serialboxSavepointToString.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointToString.restype = c_char_p

    library.serialboxSavepointGetMetaInfo.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointGetMetaInfo.restype = POINTER(MetaInfoImpl)


class Savepoint(object):
    """Savepoint implementation of the Python Interface.

    Savepoints are primarily identified by their `name` and further distinguished by their
    `meta_info`. Savepoints are used within the :class:`Serializer` to discriminate fields
    at different points in time.
    """

    def __init__(self, name, metainfo=None, impl=None):
        """Initialize the Savepoint.

        This method prepares the savepoint for usage and gives a name, which is the only required
        information for the savepoint to be usable. Meta-information can be added after the
        initialization has been performed.

        :param name: str -- Name of the savepoint
        :param metainfo: dict -- {Key:value} pair dictionary used for initializing the
                                 meta-information of the Savepont
        :param impl: Directly set the implementation pointer (internal use)
        :raises: SerialboxError -- Savepoint could not be initialized
        """
        if impl:
            self.__savepoint = impl
        else:
            namestr = extract_string(name)[0]
            self.__savepoint = invoke(lib.serialboxSavepointCreate, namestr)

        if metainfo:
            if isinstance(metainfo, MetaInfoMap):
                metainfo = metainfo.to_dict()

            metainfomap = self.metainfo
            for key, value in metainfo.items():
                metainfomap.insert(key, value)

    @property
    def name(self):
        """Name of the Savepoint.

        :return: Name of the savepoint
        :rtype: str
        """
        return invoke(lib.serialboxSavepointGetName, self.__savepoint).decode()

    @property
    def metainfo(self):
        """Meta-information of the Savepoint.

        :return: Refrence to the meta-information map
        :rtype: MetaInfoMap
        """
        return MetaInfoMap(impl=invoke(lib.serialboxSavepointGetMetaInfo, self.__savepoint))

    def clone(self):
        """Clone the Savepoint by performing a deepcopy.

        :return: Clone of the savepoint
        :rtype: Savepoint
        """
        return Savepoint('',
                         impl=invoke(lib.serialboxSavepointCreateFromSavepoint, self.__savepoint))

    def __eq__(self, other):
        """Test for equality.

        Savepoints compare equal if their names and meta-infor compare equal.

        :return: True if self == other, False otherwise
        :rtype: bool
        """
        return bool(invoke(lib.serialboxSavepointEqual, self.__savepoint, other.__savepoint))

    def __ne__(self, other):
        """Test for inequality.

        :return: True if self != other, False otherwise
        :rtype: bool
        """
        return not self.__eq__(other)

    def impl(self):
        """Get implementation pointer.
        """
        return self.__savepoint

    def __del__(self):
        invoke(lib.serialboxSavepointDestroy, self.__savepoint)

    def __repr__(self):
        return '<Savepoint {0}>'.format(self.__str__())

    def __str__(self):
        return invoke(lib.serialboxSavepointToString, self.__savepoint).decode()


register_library(lib)
