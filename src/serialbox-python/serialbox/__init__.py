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
## Python Interface of Serialbox.
##
##===------------------------------------------------------------------------------------------===##

"""'Serialbox Python Interface'"""

__versioninfo__ = (2, 0, 1)
__version__ = '.'.join(str(v) for v in __versioninfo__) + '-dev'

#
# Check python version
#
from sys import version_info

if version_info < (3, 4):
    raise Exception("Serialbox requires atleast python 3.4")

#
# Check if numpy is available
#
try:
    from numpy import __version__
except ImportError:
    raise Exception("Serialbox requires numpy")

#
# Import submodules (the .core should to be imported first)
#
from .core import Config
from .type import TypeID, OpenModeKind
from .error import SerialboxError
from .logging import Logging
from .serializer import Serializer
from .savepoint import Savepoint
from .metainfomap import MetaInfoMap
from .fieldmetainfo import FieldMetaInfo
from .archive import Archive

__all__ = ['Config', 'TypeID', 'SerialboxError', 'Logging', 'Serializer', 'Savepoint',
           'MetaInfoMap', 'FieldMetaInfo', 'OpenModeKind', 'Archive']