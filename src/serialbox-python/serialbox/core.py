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
## Setup core functionality of serialbox.
##
##===------------------------------------------------------------------------------------------===##

from .common import get_library

lib = get_library()

def register_library(library):
    """Register library functions of SerialboxC

    :param library: refrence to the SerialboxC shared library.
    :type library: ctypes.CDLL.
    """
    print(type(library))

def init_serialbox():
    """Initialize serialbox by installing the error handler.
    """
    pass

register_library(lib)
init_serialbox()

