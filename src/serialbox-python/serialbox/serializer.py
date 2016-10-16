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
## This file contains the Serializer implementation of the Python Interface.
##
##===------------------------------------------------------------------------------------------===##

from .common import get_library, extract_string
from .error import invoke, SerialboxError

lib = get_library()


def register_library(library):
    pass


class Serializer(object):
    def __init__(self):
        errstr, errlen = extract_string("oh siieht")

        try:
            invoke(lib.serialboxFatalError, errstr)
        except SerialboxError as e:
            print(e)


register_library(lib)