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

if version_info < (3, 0):
    raise Exception("serialbox requires python 3")

#
# Import submodules (the .core has to be imported first)
#
from .core import Config
from .error import SerialboxError
from .logging import Logging
from .serializer import Serializer
from .savepoint import Savepoint

__all__ = ['Config', 'SerialboxError', 'Logging', 'Serializer', 'Savepoint']
