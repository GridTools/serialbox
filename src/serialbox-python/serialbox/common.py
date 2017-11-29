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
## This file contains shared utility functions.
##
##===------------------------------------------------------------------------------------------===##

import platform
from ctypes import cdll, c_char_p, util
from os import path


def to_c_string(string):
    """Convert python string to C-string.

    :param str string: Python string
    :return: char* and length of the string
    :rtype: (ctypes.c_char_p, int)
    """
    val = string.encode('ascii') if type(string) == str else string
    return c_char_p(val), len(val)


def get_library():
    """Obtain a reference to the SerialboxC shared library.

    :return: refrence to the SerialboxC shared library
    :rtype: ctypes.CDLL
    :raises Exception: serialboxC shared library could not be loaded or found
    """

    # On Linux, ctypes.cdll.LoadLibrary() respects LD_LIBRARY_PATH
    # while ctypes.util.find_library() doesn't.
    # See http://docs.python.org/2/library/ctypes.html#finding-shared-libraries
    #
    # To make it possible to run the unit tests without installing the SerialboxC shared
    # library into a default linker search path.  Always Try ctypes.cdll.LoadLibrary()
    # with all possible library names first, then try ctypes.util.find_library().
    name = 'SerialboxC'
    errors = []

    cwd = path.dirname(path.realpath(__file__))
    dirs = (cwd, path.join(cwd, "../../lib"),)

    for d in dirs:
        t = platform.system()
        if t == 'Darwin':
            pfx, ext = 'lib', '.dylib'
        elif t == 'Windows':
            pfx, ext = '', '.dll'
        else:
            pfx, ext = 'lib', '.so'

        try:
            lib = cdll.LoadLibrary(path.join(d, pfx + name + ext))
        except OSError as e:
            errors += [e]
            pass
        else:
            return lib

        t = util.find_library(name)
        if t:
            return cdll.LoadLibrary(t)

    print(errors)
    raise Exception("'serialboxC' shared library not found")
