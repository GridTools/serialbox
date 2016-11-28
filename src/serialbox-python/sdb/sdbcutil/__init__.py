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

from sdbcore import __version__ as __sdbcore_version__
from sdbcore import __versioninfo__ as __sdbcore_versioninfo__

__version__ = __sdbcore_version__
__versioninfo__ = __sdbcore_versioninfo__

SDBCUTIL_HAS_C = True

try:
    import sdbcutilC
except ImportError:
    SDBCUTIL_HAS_C = False

__all__ = ['__version__', '__versioninfo__', 'SDBCUTIL_HAS_C']
