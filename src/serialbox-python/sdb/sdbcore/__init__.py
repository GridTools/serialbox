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

from .error import fatal_error
from .logger import Logger
from .version import __version__, __versioninfo__

# Check if Serialbox is available
try:
    from serialbox import __version__

    Logger.info("importing Serialbox (%s)" % __version__)
except ImportError as e:
    from .error import fatal_error
    fatal_error(e)


__all__ = ['__version__', '__versioninfo__']
