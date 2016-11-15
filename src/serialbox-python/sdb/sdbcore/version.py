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

__versioninfo__ = (0, 0, 1)
__version__ = '.'.join(str(v) for v in __versioninfo__)

class Version(object):
    def __init__(self):
        pass

    def serialbox_version(self):
        from serialbox import __version__ as serialbox_vers
        return serialbox_vers

    def numpy_version(self):
        from numpy import __version__ as numpy_vers
        return numpy_vers

    def sdb_version(self):
        return __version__
