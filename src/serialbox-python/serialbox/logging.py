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
## This file contains the Logging implementation of the Python Interface.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import c_int

from .common import get_library

lib = get_library()


def register_library(library):
    library.serialboxLoggingIsEnabled.restype = c_int


class Logging(object):
    """Logging infastrucutre

    By default, logging is disabled. If ``SERIALBOX_DISABLE_LOGGING`` was used during
    compilation of the library, the functions do nothing (see :class:`Config <serialbox.Config>`).
    """

    @staticmethod
    def enable():
        """Enable logging.
        """
        lib.serialboxLoggingEnable()

    @staticmethod
    def disable():
        """Disable logging.
        """
        lib.serialboxLoggingDisable()

    @staticmethod
    def is_enabled():
        """Check if logging is enabled.

        :return: `True` if logging is enabled, `False` otherwise
        :rtype: bool
        """
        return bool(lib.serialboxLoggingIsEnabled)


register_library(lib)
