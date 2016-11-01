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
    """Configurations used when compiling the ``serialboxC`` library. The configuration is stored
    as a python dictionary :class:`dict`.

        >>> config = Config()
        >>> d = config.get_dict()
        >>> d
        {'SERIALBOX_HAS_NETCDF': '1',
         'SERIALBOX_CXX_FLAGS': '-std=c++11  -march=native -fPIC -O2 -g -DNDEBUG',
         'BOOST_VERSION': '1_61',
         'SERIALBOX_ASYNC_API': '1',
         'SERIALBOX_CXX_COMPILER': '/usr/bin/clang',
         'SERIALBOX_HAS_OPENSSL': '1'}
        >>>
    """

    instance = None  # Singleton instance

    class __Config(object):
        def __init__(self):
            self.compile_options = self.get_compile_options()

        def get_compile_options(self):
            configstr = lib.serialboxConfigOptions().decode()
            config_options = dict()
            for config in configstr.split(";"):
                key, value = config.split("=", 1)
                config_options[key] = value
            return config_options

    def get_dict(self):
        """ Dictionary of compile options

        :return: Compile options
        :rtype: :class:`dict`
        """
        return self.instance.compile_options

    def __init__(self):
        if not Config.instance:
            Config.instance = Config.__Config()

    def __getattr__(self, name):
        return getattr(self.instance, name)

    def __str__(self):
        return str(self.instance.compile_options)

    def __repr__(self):
        return "<Config {0}>".format(self.instance.compile_options)

def init_serialbox():
    """Initialize the SerialboxC library by installing the error handler.
    """
    lib.serialboxInstallFatalErrorHandler(lib.serialboxStateErrorHandler)


register_library(lib)
init_serialbox()
