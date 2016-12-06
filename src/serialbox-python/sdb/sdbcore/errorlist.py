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


class ErrorList(object):
    def __init__(self, input_field, reference_field, atol, rtol):
        self.__error_positions = logical_not(
            isclose(input_field, reference_field, atol, rtol))

        self.__error_list = []

        it_value = nditer(self.__error_positions, flags=["multi_index"])
        it_input = nditer(input_field)
        it_reference = nditer(reference_field)

        while not it_value.finished:
            if it_value.value:
                self.__error_list += [[it_value.multi_index, it_input.value, it_reference.value]]

            it_value.iternext()
            it_input.iternext()
            it_reference.iternext()

    def list(self):
        return self.__error_list

    def positions(self):
        return self.__error_positions
