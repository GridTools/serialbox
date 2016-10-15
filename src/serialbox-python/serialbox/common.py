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
## Common utility functions to load the serialboxC shared library.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import cdll

import ctypes.util
import platform
import os

def get_library():
    """Obtain a reference to the SerialboxC shared library.

    :returns: ctypes.CDLL -- refrence to the SerialboxC shared library.
    :raises: OSError, Exception
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
                print(os.path.join(d, pfx + i + ext))
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

    
