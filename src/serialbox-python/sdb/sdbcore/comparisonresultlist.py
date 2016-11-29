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

from .comparisonresult import ComparisonResult
from .logger import Logger

class ComparisonResultList(object):
    """Store a list of comparison results of an input and reference serializer.
    """

    def __init__(self):
        self.__results = []
        self.__input_stencil = None
        self.__reference_stencil = None

    def reset(self):
        self.__results.clear()
        self.__input_stencil = None
        self.__reference_stencil = None

    def append(self,
               match,
               intent,
               input_stage,
               input_field_name,
               input_savepoint,
               input_serializer,
               reference_stage,
               reference_field_name,
               reference_savepoint,
               reference_serializer,
               rtol,
               atol):
        self.__results += [ComparisonResult({"intent": intent,
                                             "input_stage": input_stage,
                                             "input_stencil": self.input_stencil,
                                             "input_field_name": input_field_name,
                                             "input_savepoint": input_savepoint,
                                             "input_serializer": input_serializer,
                                             "reference_field_name": reference_field_name,
                                             "reference_savepoint": reference_savepoint,
                                             "reference_stage": reference_stage,
                                             "reference_stencil": self.reference_stencil,
                                             "reference_serializer": reference_serializer,
                                             "match": match,
                                             "rtol": rtol,
                                             "atol": atol})]

        Logger.info(
            "Comparing field '%s' vs. '%s' of stage '%s' [%s], result: %s" % (input_field_name,
                                                                              reference_field_name,
                                                                              input_stage, intent,
                                                                              match))

    @property
    def results(self):
        return self.__results

    def shared_stencil_name(self):
        return self.__input_stencil if self.__input_stencil == self.__reference_stencil else (
            "%s vs. %s" % (self.__input_stencil, self.__reference_stencil))

    def __set_input_stencil(self, input_stencil):
        self.__input_stencil = input_stencil

    def __get_input_stencil(self):
        return self.__input_stencil

    input_stencil = property(__get_input_stencil, __set_input_stencil)

    def __set_reference_stencil(self, reference_stencil):
        self.__reference_stencil = reference_stencil

    def __get_reference_stencil(self):
        return self.__reference_stencil

    reference_stencil = property(__get_reference_stencil, __set_reference_stencil)
