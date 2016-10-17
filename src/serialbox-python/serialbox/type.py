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

__all__ = ['TypeID', 'BooleanTypes', 'Int32Types', 'Int64Types', 'Float32Types', 'Float64Types',
           'StringTypes']
