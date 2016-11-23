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

from .logger import Logger


class ComparisonResult(object):
    def __init__(self, input_stencil_data, reference_stencil_data):
        self.__input_stencil_data = input_stencil_data
        self.__reference_stencil_data = reference_stencil_data

        self.__result_dict = dict()

    def reset(self):
        self.__result_dict.clear()

    def add_stencils(self, input_stencil, reference_stencil):
        self.__result_dict[
            "stencil_name"] = input_stencil if input_stencil == reference_stencil else (
            "%s vs. %s" % (input_stencil, reference_stencil))
        self.__result_dict["result"] = []

    def compare_fields(self,
                       stage,
                       intent,
                       input_field,
                       input_field_name,
                       input_savepoint,
                       reference_field,
                       reference_field_name,
                       reference_savepoint):
        match = np.allclose(input_field, reference_field)
        self.__result_dict["result"] += [{"stage": stage,
                                          "intent": intent,
                                          "input_field_name": input_field_name,
                                          "input_savepoint": input_savepoint,
                                          "reference_field_name": reference_field_name,
                                          "reference_savepoint": reference_savepoint,
                                          "match": match}]

        Logger.info(
            "Comparing field '%s' vs. '%s' of stage '%s' [%s], result: %s" % (input_field_name,
                                                                              reference_field_name,
                                                                              stage, intent, match))

    def __getitem__(self, key):
        return self.__result_dict[key]
