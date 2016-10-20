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
## Setup serialboxC library.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import c_char_p

from .common import get_library

lib = get_library()


def register_library(library):
    library.serialboxConfigOptions.restype = c_char_p


class Config(object):
    """Configurations used when compiling the serialboxC library"""

    @staticmethod
    def to_dict():
        """Get configuration options used during compilation

        :return: Dictionary of {key:value} pair of the configuration options
        :rtype: dict
        """
        configstr = lib.serialboxConfigOptions().decode()
        config_options = dict()
        for config in configstr.split(";"):
            key, value = config.split("=", 1)
            config_options[key] = value
        return config_options


def init_serialbox():
    """Initialize the SerialboxC library by installing the error handler.
    """
    lib.serialboxInstallFatalErrorHandler(lib.serialboxStateErrorHandler)


register_library(lib)
init_serialbox()
