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
## This file contains the field meta-information implementation of the Python Interface.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import c_void_p, c_char_p, c_int, Structure, POINTER

from .common import get_library
from .error import invoke
from .metainfomap import MetaInfoMap, MetaInfoImpl
from .type import *

lib = get_library()


class FieldMetaInfoImpl(Structure):
    """ Mapping of serialboxFieldMetaInfo_t """
    _fields_ = [("impl", c_void_p), ("ownsData", c_int)]


def register_library(library):
    #
    # Construction & Destruction
    #
    library.serialboxFieldMetaInfoCreate.argtypes = [c_int, POINTER(c_int), c_int]
    library.serialboxFieldMetaInfoCreate.restype = POINTER(FieldMetaInfoImpl)

    library.serialboxFieldMetaInfoCreateFromFieldMetaInfo.argtypes = [POINTER(FieldMetaInfoImpl)]
    library.serialboxFieldMetaInfoCreateFromFieldMetaInfo.restype = POINTER(FieldMetaInfoImpl)

    library.serialboxFieldMetaInfoDestroy.argtypes = [POINTER(FieldMetaInfoImpl)]
    library.serialboxFieldMetaInfoDestroy.restype = None

    #
    # Utility
    #
    library.serialboxFieldMetaInfoEqual.argtypes = [POINTER(FieldMetaInfoImpl),
                                                    POINTER(FieldMetaInfoImpl)]
    library.serialboxFieldMetaInfoEqual.restype = c_int

    library.serialboxFieldMetaInfoToString.argtypes = [POINTER(FieldMetaInfoImpl)]
    library.serialboxFieldMetaInfoToString.restype = c_char_p

    #
    # Dimensions and TypeID
    #
    library.serialboxFieldMetaInfoGetTypeID.argtypes = [POINTER(FieldMetaInfoImpl)]
    library.serialboxFieldMetaInfoGetTypeID.restype = c_int

    library.serialboxFieldMetaInfoGetDimensions.argtypes = [POINTER(FieldMetaInfoImpl)]
    library.serialboxFieldMetaInfoGetDimensions.restype = POINTER(c_int)

    library.serialboxFieldMetaInfoGetNumDimensions.argtypes = [POINTER(FieldMetaInfoImpl)]
    library.serialboxFieldMetaInfoGetNumDimensions.restype = c_int

    #
    #  Meta-information
    #
    library.serialboxFieldMetaInfoGetMetaInfo.argtypes = [POINTER(FieldMetaInfoImpl)]
    library.serialboxFieldMetaInfoGetMetaInfo.restype = POINTER(MetaInfoImpl)


class FieldMetaInfo(object):
    """FieldMetaInfo implementation of the Python Interface.

    FieldMetaInfos store the meta-information of fields. Each FieldMetaInfo stores the type
    (:class:`TypeID`) and dimension of the corresponding field and, optionally, arbitrary
    meta-information in the form of a :class:`MetaInfoMap`.
    """

    def __init__(self, type, dims, metainfo=None, impl=None):
        """Initialize the FieldMetaInfo.

        :param type: TypeID, int -- Type of the field. Either given as a :class:`TypeID` or an
                                    integer.
        :param dims: list -- List of dimensions.
        :param metainfo: Key-value pair dictionary used to set the meta-information
        :type metainfo: dict, MetaInfoMap
        :param impl: Directly set the implementation pointer (internal use)
        """
        if impl:
            self.__fieldmetainfo = impl
        else:
            if isinstance(type, TypeID):
                typeid = c_int(type.value)
            else:
                typeid = c_int(type)

            dims_array = (c_int * len(dims))()
            num_dims = c_int(len(dims))
            for i in range(len(dims)):
                dims_array[i] = dims[i]

            self.__fieldmetainfo = invoke(
                lib.serialboxFieldMetaInfoCreate, typeid, dims_array, num_dims)

        if metainfo:
            if isinstance(metainfo, MetaInfoMap):
                metainfo = metainfo.to_dict()

            metainfomap = self.metainfo
            for key, value in metainfo.items():
                metainfomap.insert(key, value)

    def clone(self):
        """Clone the FieldMetaInfo map by performing a deepcopy.

        :return: Clone of the FieldMetaInfo
        :rtype: FieldMetaInfo
        """
        return FieldMetaInfo(None, [],
                             impl=invoke(lib.serialboxFieldMetaInfoCreateFromFieldMetaInfo,
                                         self.__fieldmetainfo))

    @property
    def type(self):
        """Type of the associated field

        :return: Type of the field
        :rtype: TypeID
        """
        return TypeID(invoke(lib.serialboxFieldMetaInfoGetTypeID, self.__fieldmetainfo))

    @property
    def dims(self):
        """ Dimensions of the associated field.

        :return: Dimensions of the field
        :rtype: list
        """
        num_dims = invoke(lib.serialboxFieldMetaInfoGetNumDimensions, self.__fieldmetainfo)
        dims_array = invoke(lib.serialboxFieldMetaInfoGetDimensions, self.__fieldmetainfo)
        dims_list = []
        for i in range(num_dims):
            dims_list += [dims_array[i]]
        return dims_list

    @property
    def metainfo(self):
        """Meta-information of the associated field.

        :return: Refrence to the meta-information map
        :rtype: MetaInfoMap
        """
        return MetaInfoMap(impl=invoke(lib.serialboxFieldMetaInfoGetMetaInfo, self.__fieldmetainfo))

    def __eq__(self, other):
        """Test for equality.

        Savepoints compare equal if their names and meta-infor compare equal.

        :return: True if self == other, False otherwise
        :rtype: bool
        """
        return bool(
            invoke(lib.serialboxFieldMetaInfoEqual, self.__fieldmetainfo, other.__fieldmetainfo))

    def __ne__(self, other):
        """Test for inequality.

        :return: True if self != other, False otherwise
        :rtype: bool
        """
        return not self.__eq__(other)

    def impl(self):
        """Get implementation pointer.
        """
        return self.__fieldmetainfo

    def __del__(self):
        invoke(lib.serialboxFieldMetaInfoDestroy, self.__fieldmetainfo)

    def __str__(self):
        return invoke(lib.serialboxFieldMetaInfoToString, self.__fieldmetainfo).decode()

    def __repr__(self):
        return '<FieldMetaInfo {0}>'.format(self.__str__())

register_library(lib)
