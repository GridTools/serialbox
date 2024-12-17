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

from packaging import version as pkg_version

# note: version should not be changed manually, use bump-my-version instead:
# install: pip install bump-my-version
# example: bump-my-version bump patch
__version__ = "2.6.2"
__versioninfo__ = pkg_version.parse(__version__)

#
# Check python version
#
from sys import version_info

if version_info < (3, 4):
    raise Exception("Serialbox requires at least python 3.4")

#
# Check if numpy is available
#
try:
    import numpy

    del numpy
except ImportError:
    raise Exception("Serialbox requires numpy")

#
# Import submodules (the .core should to be imported first)
#
from .core import Config
from .type import TypeID, OpenModeKind
from .error import SerialboxError
from .serlogging import Logging
from .serializer import Serializer
from .savepoint import Savepoint, SavepointCollection
from .metainfomap import MetainfoMap
from .fieldmetainfo import FieldMetainfo
from .archive import Archive
from .slice import Slice

__all__ = [
    "Config",
    "TypeID",
    "SerialboxError",
    "Logging",
    "Serializer",
    "Savepoint",
    "SavepointCollection",
    "MetainfoMap",
    "FieldMetainfo",
    "OpenModeKind",
    "Archive",
    "Slice",
]
