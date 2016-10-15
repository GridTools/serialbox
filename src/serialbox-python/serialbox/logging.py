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
    lib.serialboxLoggingIsEnabled.restype = c_int

class Logging:
    """Logging infastrucutre"""

    @staticmethod
    def enable():
      """Enable logging

      By default, the logging is disabled. If `SERIALBOX_DISABLE_LOGGING` is defined, the function
      does nothing.
      """
      lib.serialboxLoggingEnable()

    @staticmethod
    def disable():
      """Disable logging

      By default, the logging is disabled. If `SERIALBOX_DISABLE_LOGGING` is defined, the function
      does nothing.
      """
      lib.serialboxLoggingDisable()

    @staticmethod
    def is_enabled():
      """Check if logging is enabled

      :return: True if logging is enabled, False otherwise
      :rtype: bool
      """
      return bool(lib.serialboxLoggingIsEnabled)

register_library(lib)
print("init logging")

