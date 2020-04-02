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

from sdbcore.errorlist import ErrorList


class ComparisonResult(object):
    """Store a comparison result as a dictionary

    A comparison result is an immutable object.
    """

    def __init__(self, dict_result):
        self.__result = dict_result

        # Auxiliary data structures are computed lazily
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

    def __get_error_list(self):
        if self.__error_list is None:
            self.__error_list = ErrorList(self.input_field, self.reference_field,
                                          self.__result["atol"],
                                          self.__result["rtol"])
        return self.__error_list

    def get_error_positions(self):
        return self.__get_error_list().positions()

    def get_error_list(self):
        return self.__get_error_list().list()
