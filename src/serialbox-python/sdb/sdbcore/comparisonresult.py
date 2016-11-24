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
                       intent,
                       input_stage,
                       input_stencil,
                       input_field,
                       input_field_name,
                       input_savepoint,
                       input_serializer,
                       reference_stage,
                       reference_stencil,
                       reference_field,
                       reference_field_name,
                       reference_savepoint,
                       reference_serializer):
        match = np.allclose(input_field, reference_field)
        self.__result_dict["result"] += [{"intent": intent,
                                          "input_stage": input_stage,
                                          "input_stencil": input_stencil,
                                          "input_field_name": input_field_name,
                                          "input_savepoint": input_savepoint,
                                          "input_serializer": input_serializer,
                                          "reference_field_name": reference_field_name,
                                          "reference_savepoint": reference_savepoint,
                                          "reference_stage": reference_stage,
                                          "reference_stencil": reference_stencil,
                                          "reference_serializer": reference_serializer,
                                          "match": match}]

        Logger.info(
            "Comparing field '%s' vs. '%s' of stage '%s' [%s], result: %s" % (input_field_name,
                                                                              reference_field_name,
                                                                              input_stage, intent,
                                                                              match))

    def __getitem__(self, key):
        return self.__result_dict[key]
