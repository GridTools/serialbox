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
## This file contains the type and enum definitions of Serialbox
##
##===------------------------------------------------------------------------------------------===##

from enum import Enum

import numpy as np

from .error import SerialboxError


class OpenModeKind(Enum):
    """ Policy for opening files in the Serializer and Archive
    """
    Read = 0
    Write = 1
    Append = 2


class TypeID(Enum):
    """Type-id of types recognized by serialbox.

    This enum corresponds to the enum definitions of the C/C++ library.
    """
    Invalid = 0

    Boolean = 1
    Int32 = 2
    Int64 = 3
    Float32 = 4
    Float64 = 5
    String = 6

    Array = 0x10
    ArrayOfBoolean = Array | Boolean
    ArrayOfInt32 = Array | Int32
    ArrayOfInt64 = Array | Int64
    ArrayOfFloat32 = Array | Float32
    ArrayOfFloat64 = Array | Float64
    ArrayOfString = Array | String


BooleanTypes = (bool, np.bool,)
Int32Types = (int, np.int8, np.int16, np.int32, np.uint8, np.uint16, np.uint32,)
Int64Types = (np.int64, np.uint64,)
Float32Types = (np.float32,)
Float64Types = (float, np.float64,)
StringTypes = (bytes, str,)


def numpy2TypeID(dtype):
    """Convert numpy.dtype to serialbox.TypeID
    """
    if dtype in BooleanTypes:
        return TypeID.Boolean
    elif dtype in Int32Types:
        return TypeID.Int32
    elif dtype in Int64Types:
        return TypeID.Int64
    elif dtype in Float32Types:
        return TypeID.Float32
    elif dtype in Float64Types:
        return TypeID.Float64
    else:
        raise SerialboxError("cannot map numpy.dtype (%s) to serialbox.TypeID" % dtype)


def typeID2numpy(typeid):
    """Convert serialbox.TypeID to numpy.dtype
    """
    if typeid == TypeID.Boolean:
        return np.bool
    elif typeid == TypeID.Int32:
        return np.int32
    elif typeid == TypeID.Int64:
        return np.int64
    elif typeid == TypeID.Float32:
        return np.float32
    elif typeid == TypeID.Float64:
        return np.float64
    else:
        raise SerialboxError("cannot map serialbox.TypeID (%s) to numpy.dtype" % typeid)


__all__ = ['TypeID', 'OpenModeKind', 'BooleanTypes', 'Int32Types', 'Int64Types', 'Float32Types',
           'Float64Types', 'StringTypes', 'numpy2TypeID', 'typeID2numpy']
