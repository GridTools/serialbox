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
    def __init__(self, input_field, reference_field, atol, rtol, force_python=False):

        if not force_python and SDBCUTIL_HAS_C:
            Logger.info("Using sdbcutil C interface")
            from sdbcutilC import compute_error_list_c as compute_error_list
        else:
            Logger.info("Using sdbcutil Python interface")
            from .sdbcutilpython import compute_error_list_python as compute_error_list

        self.__error_list, self.__error_positions = compute_error_list(input_field, reference_field,
                                                                       atol, rtol)

    def list(self):
        return self.__error_list

    def positions(self):
        return self.__error_positions
