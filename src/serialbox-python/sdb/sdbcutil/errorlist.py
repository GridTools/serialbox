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

from sdbcore.logger import Logger
from sdbcutil import SDBCUTIL_HAS_C


class ErrorList(object):
    def __init__(self, input_field, reference_field, atol, rtol):
        #
        # if SDBCUTIL_HAS_C:
        #     Logger.info("Using sdbcutil C interface")
        #     from sdbcutilC import compare_c as compare
        # else:

        Logger.info("Using sdbcutil Python interface")
        from .sdbcutilpython import compare_python as compare

        self.__error_list = compare(input_field, reference_field, atol, rtol)

    def list(self):
        return self.__error_list
