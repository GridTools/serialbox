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

    @staticmethod
    def serialbox_version():
        from serialbox import __version__ as serialbox_vers
        return serialbox_vers

    @staticmethod
    def numpy_version():
        from numpy import __version__ as numpy_vers
        return numpy_vers

    @staticmethod
    def sdb_version():
        return __version__
