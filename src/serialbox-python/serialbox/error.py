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
## This file contains the error handling of the Python Interface of Serialbox.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import c_int, c_char_p

from .common import get_library

lib = get_library()

def register_library(library):
    lib.serialboxStateErrorHandlerHasError.restype = c_int
    lib.serialboxStateErrorHandlerGetErrorMessage.restype = c_char_p

class SerialboxError(Exception):
    """Raised when an operation results in an error.

    :attribute str message: explanation of the error
    """
    def __init__(self, message):
        self.message = message


def invoke(function, *args):
    """ Invoke function with *args and raise SerialboxError in case of an error. 

    :param function: function to invoke
    :param *args: arguments of the function to invoke
    :raises SerialboxError: function invocation resulted in an error
    """
    function(*args)
    if lib.serialboxStateErrorHandlerHasError():
        error_message = lib.serialboxStateErrorHandlerGetErrorMessage()
        raise SerialboxError(error_message.decode())

register_library(lib)
print("init error")

