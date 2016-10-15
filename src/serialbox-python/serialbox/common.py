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

from ctypes import cdll, c_char_p

import ctypes.util
import platform
import os

def extract_string(string):
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
    names = ['SerialboxC']
    
    cwd = os.path.dirname(os.path.realpath(__file__))
    dirs = (cwd, os.path.join(cwd, "../../lib"), )
  
    for d in dirs:  
        t = platform.system()
        if t == 'Darwin':
            pfx, ext = 'lib', '.dylib'
        elif t == 'Windows':
            pfx, ext = '', '.dll'
        else:
            pfx, ext = 'lib', '.so'

        for i in names:
            try:
                lib = cdll.LoadLibrary(os.path.join(d, pfx + i + ext))
            except OSError:
                pass
            else:
                return lib

        for i in names:
            t = ctypes.util.find_library(i)
            if t:
                return cdll.LoadLibrary(t)

    raise Exception("'serialboxC' shared library not found")

    
