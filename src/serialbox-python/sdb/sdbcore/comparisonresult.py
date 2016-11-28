#!/usr/bin/python3
# -*- coding: utf-8 -*-
##===-----------------------------------------------------------------------------*- python -*-===##
##
##                                   s e r i a l b o x
##
## this file is distributed under terms of bsd license.
## see license.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

import numpy as np

from sdbcore.logger import Logger
from sdbcutil.errorlist import ErrorList


class ComparisonResult(object):
    """Store a comparison result as a dictionary

    A comparison result is an immutable object.
    """

    def __init__(self, dict_result):
        self.__result = dict_result

        # Auxiliary datastructures are computed lazily
        self.__list_of_errors = None
        self.__position_of_errors = None

        self.__input_field = None
        self.__reference_field = None

        self.__error_list = None

    def __getitem__(self, key):
        return self.__result[key]

    def get_dict(self):
        return self.__result

    @property
    def shape(self):
        return self.input_field.shape if self.input_field.shape == self.reference_field.shape else None

    @property
    def input_shape(self):
        return self.input_field.shape

    @property
    def reference_shape(self):
        return self.reference_field.shape

    def shapes_match(self):
        return self.shape != None

    @property
    def input_field(self):
        if self.__input_field is None:
            serializer = self.__result["input_serializer"]
            field_name = self.__result["input_field_name"]
            savepoint = self.__result["input_savepoint"]
            self.__input_field = serializer.read(field_name, savepoint)
        return self.__input_field

    @property
    def reference_field(self):
        if self.__reference_field is None:
            serializer = self.__result["reference_serializer"]
            field_name = self.__result["reference_field_name"]
            savepoint = self.__result["reference_savepoint"]
            self.__reference_field = serializer.read(field_name, savepoint)
        return self.__reference_field

    @property
    def position_of_errors(self):

        if self.__position_of_errors is None:
            Logger.info(
                "Computing position of errors of input field '%s' and reference field '%s'" % (
                    self.__result["input_field_name"], self.__result["reference_field_name"]))

            self.__position_of_errors = np.logical_not(
                np.isclose(self.input_field, self.reference_field,
                           atol=self.__result["atol"],
                           rtol=self.__result["rtol"]))

        return self.__position_of_errors

    @property
    def list_of_errors(self):

        self.__error_list = ErrorList(self.input_field, self.reference_field, self.__result["atol"],
                                      self.__result["rtol"])

        if self.__list_of_errors is None:
            Logger.info("Computing list of errors of input field '%s' and reference field '%s'" % (
                self.__result["input_field_name"], self.__result["reference_field_name"]))
            self.__list_of_errors = []

            it_value = np.nditer(self.position_of_errors, flags=["multi_index"])
            it_input = np.nditer(self.input_field)
            it_reference = np.nditer(self.reference_field)

            while not it_value.finished:
                if it_value.value:
                    self.__list_of_errors += [[it_value.multi_index, it_input.value,
                                               it_reference.value]]

                it_value.iternext()
                it_input.iternext()
                it_reference.iternext()

        return self.__list_of_errors
