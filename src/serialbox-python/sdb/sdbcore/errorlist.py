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

from numpy import logical_not, isclose, nditer

from sdbcore import SDBCORE_HAS_C
from sdbcore.logger import Logger


def make_error_list_python(input_field, reference_field, atol, rtol):
    """Python implementation of creating the ErrorList.

    :param input_field: Input field
    :type input_field: numpy.array
    :param reference_field: Refrence field
    :type reference_field: numpy.array
    :param atol: Absolute tolerance used for comparison
    :type atol: float
    :param rtol: Relative tolerance used for comparison
    :type rtol: float
    :return: list of errors and positions
    """
    error_positions = logical_not(
        isclose(input_field, reference_field, atol, rtol))

    error_list = []

    it_value = nditer(error_positions, flags=["multi_index"])
    it_input = nditer(input_field)
    it_reference = nditer(reference_field)

    while not it_value.finished:
        if it_value.value:
            error_list += [[it_value.multi_index, it_input.value, it_reference.value]]

        it_value.iternext()
        it_input.iternext()
        it_reference.iternext()

    return error_list, error_positions


class ErrorList(object):
    def __init__(self, input_field, reference_field, atol, rtol, force_python=False):

        if not force_python and SDBCORE_HAS_C:
            Logger.info("Using sdbcutil C interface")
            from sdbcore.sdbcoreC import make_error_list_c as make_error_list
        else:
            Logger.info("Using sdbcutil Python interface")
            make_error_list = make_error_list_python

        self.__error_list, self.__error_positions = make_error_list(input_field, reference_field,
                                                                    atol, rtol)

    def list(self):
        return self.__error_list

    def positions(self):
        return self.__error_positions
