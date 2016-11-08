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
from .metainfomap import MetainfoMap, MetainfoImpl
from .type import *

lib = get_library()


class FieldMetainfoImpl(Structure):
    """ Mapping of serialboxFieldMetainfo_t """
    _fields_ = [("impl", c_void_p), ("ownsData", c_int)]


def register_library(library):
    #
    # Construction & Destruction
    #
    library.serialboxFieldMetainfoCreate.argtypes = [c_int, POINTER(c_int), c_int]
    library.serialboxFieldMetainfoCreate.restype = POINTER(FieldMetainfoImpl)

    library.serialboxFieldMetainfoCreateFromFieldMetainfo.argtypes = [POINTER(FieldMetainfoImpl)]
    library.serialboxFieldMetainfoCreateFromFieldMetainfo.restype = POINTER(FieldMetainfoImpl)

    library.serialboxFieldMetainfoDestroy.argtypes = [POINTER(FieldMetainfoImpl)]
    library.serialboxFieldMetainfoDestroy.restype = None

    #
    # Utility
    #
    library.serialboxFieldMetainfoEqual.argtypes = [POINTER(FieldMetainfoImpl),
                                                    POINTER(FieldMetainfoImpl)]
    library.serialboxFieldMetainfoEqual.restype = c_int

    library.serialboxFieldMetainfoToString.argtypes = [POINTER(FieldMetainfoImpl)]
    library.serialboxFieldMetainfoToString.restype = c_char_p

    #
    # Dimensions and TypeID
    #
    library.serialboxFieldMetainfoGetTypeID.argtypes = [POINTER(FieldMetainfoImpl)]
    library.serialboxFieldMetainfoGetTypeID.restype = c_int

    library.serialboxFieldMetainfoGetDimensions.argtypes = [POINTER(FieldMetainfoImpl)]
    library.serialboxFieldMetainfoGetDimensions.restype = POINTER(c_int)

    library.serialboxFieldMetainfoGetNumDimensions.argtypes = [POINTER(FieldMetainfoImpl)]
    library.serialboxFieldMetainfoGetNumDimensions.restype = c_int

    #
    #  Meta-information
    #
    library.serialboxFieldMetainfoGetMetainfo.argtypes = [POINTER(FieldMetainfoImpl)]
    library.serialboxFieldMetainfoGetMetainfo.restype = POINTER(MetainfoImpl)


class FieldMetainfo(object):
    """FieldMetainfo implementation of the Python Interface.

    FieldMetainfos store the meta-information of fields. Each FieldMetainfo stores the type
    (:class:`TypeID <serialbox.TypeID>`) and dimension of the corresponding field and, optionally,
    arbitrary meta-information in the form of a :class:`MetainfoMap <serialbox.MetainfoMap>`.

        >>> f = FieldMetainfo(TypeID.Float64, [256, 256, 80])
        >>> f
        <FieldMetainfo type = double, dims = [256, 256, 80], metainfo = {}>
        >>> f.metainfo.insert('key', 5)
        >>> f
        <FieldMetainfo type = double, dims = [256, 256, 80], metainfo = {"key": 5}>
        >>>
    """

    def __init__(self, type, dims, metainfo=None, impl=None):
        """Initialize the FieldMetainfo.

        :param type: Type of the field.
        :type type: :class:`TypeID <serialbox.TypeID>`, int
        :param dims: List of dimensions.
        :type dims: :class:`list` [:class:`int`]
        :param metainfo: Key-value pair dictionary used to set the meta-information
        :type metainfo: dict, MetainfoMap
        :param impl: Directly set the implementation pointer [internal use]
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
                lib.serialboxFieldMetainfoCreate, typeid, dims_array, num_dims)

        if metainfo:
            if isinstance(metainfo, MetainfoMap):
                metainfo = metainfo.to_dict()

            metainfomap = self.metainfo
            for key, value in metainfo.items():
                metainfomap.insert(key, value)

    def clone(self):
        """Clone the FieldMetainfo map by performing a deepcopy.

            >>> f = FieldMetainfo(TypeID.Float64, [256, 256, 80])
            >>> f_clone = f.clone()
            >>> del f
            >>> f_clone
            <FieldMetainfo type = double, dims = [256, 256, 80], metainfo = {}>
            >>>

        :return: Clone of the FieldMetainfo
        :rtype: FieldMetainfo
        """
        return FieldMetainfo(None, [],
                             impl=invoke(lib.serialboxFieldMetainfoCreateFromFieldMetainfo,
                                         self.__fieldmetainfo))

    @property
    def type(self):
        """Type of the associated field

        :return: Type of the field
        :rtype: :class:`TypeID <serialbox.TypeID>`
        """
        return TypeID(invoke(lib.serialboxFieldMetainfoGetTypeID, self.__fieldmetainfo))

    @property
    def dims(self):
        """ Dimensions of the associated field.

        :return: Dimensions of the field
        :rtype: :class:`list` [:class:`int`]
        """
        num_dims = invoke(lib.serialboxFieldMetainfoGetNumDimensions, self.__fieldmetainfo)
        dims_array = invoke(lib.serialboxFieldMetainfoGetDimensions, self.__fieldmetainfo)
        dims_list = []
        for i in range(num_dims):
            dims_list += [dims_array[i]]
        return dims_list

    @property
    def metainfo(self):
        """Meta-information of the associated field.

        :return: Refrence to the meta-info map
        :rtype: :class:`MetainfoMap <serialbox.MetainfoMap>`
        """
        return MetainfoMap(impl=invoke(lib.serialboxFieldMetainfoGetMetainfo, self.__fieldmetainfo))

    def __eq__(self, other):
        """Test for equality.

        FieldMetainfos compare equal if their type, dimensions and meta-infos compare equal.

        :return: `True` if self == other, `False` otherwise
        :rtype: bool
        """
        return bool(
            invoke(lib.serialboxFieldMetainfoEqual, self.__fieldmetainfo, other.__fieldmetainfo))

    def __ne__(self, other):
        """Test for inequality.

        FieldMetainfos compare equal if their type, dimensions and meta-infos compare equal.

        :return: `True` if self != other, `False` otherwise
        :rtype: bool
        """
        return not self.__eq__(other)

    def impl(self):
        return self.__fieldmetainfo

    def __del__(self):
        invoke(lib.serialboxFieldMetainfoDestroy, self.__fieldmetainfo)

    def __str__(self):
        return invoke(lib.serialboxFieldMetainfoToString, self.__fieldmetainfo).decode()

    def __repr__(self):
        return '<FieldMetainfo {0}>'.format(self.__str__())


register_library(lib)
