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

from .version import __version__, __versioninfo__

SDBCORE_HAS_C = True

try:
    import sdbcore.sdbcoreC
except ImportError:
    SDBCORE_HAS_C = False

__all__ = ['__version__', '__versioninfo__', 'SDBCORE_HAS_C']
