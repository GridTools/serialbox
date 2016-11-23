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

from serialbox import SerialboxError

from .comparisonresult import ComparisonResult
from .logger import Logger


def field_in_list(field_name, field_list):
    for idx in range(len(field_list)):
        field = field_list[idx]
        if field_name == field:
            return idx
    return -1


class StencilFieldMapper(object):
    def __init__(self, input_stencil_data, reference_stencil_data):
        self.__input_stencil_data = input_stencil_data
        self.__reference_stencil_data = reference_stencil_data
        self.__comparison_result = ComparisonResult(self.__input_stencil_data,
                                                    self.__reference_stencil_data)

    def match_fields(self):
        Logger.info("Matching fields")

        input_fields = self.__input_stencil_data.field_list
        reference_fields = self.__reference_stencil_data.field_list
        reference_fields_seen = []

        # Rearrange the field list in the reference serializer
        for idx in range(len(input_fields)):
            input_field = input_fields[idx]

            idx_in_ref = field_in_list(input_field, reference_fields)

            if idx_in_ref != -1:
                self.__input_stencil_data.set_enable_field(idx, True)
                reference_fields_seen += [input_field]

                # Move field to match index in input (i.e idx)
                if idx != idx_in_ref:
                    self.__reference_stencil_data.move_field(input_field, idx)

            else:
                self.__input_stencil_data.set_enable_field(idx, False)

        for field in reference_fields:
            if field in reference_fields_seen:
                self.__reference_stencil_data.set_enable_field(field, True)
            else:
                self.__reference_stencil_data.set_enable_field(field, False)

    def compare_fields(self, input_fields, reference_fields):

        # Name of the stencils
        input_stencil_name = self.__input_stencil_data.selected_stencil
        reference_stencil_name = self.__reference_stencil_data.selected_stencil
        shared_stencil_name = input_stencil_name if input_stencil_name == reference_stencil_name else (
            "%s vs. %s" % (input_stencil_name, reference_stencil_name))

        make_error_msg = lambda title, msg: "<b>%s</b> <br />%s." % (title, msg)

        Logger.info("Comparing fields of input stencil '%s' to fields of refrence stencil '%s'" % (
            input_stencil_name,
            reference_stencil_name))
        Logger.info("Input fields:     %s" % input_fields)
        Logger.info("Reference fields: %s" % reference_fields)

        self.__comparison_result.reset()
        self.__comparison_result.add_stencils(input_stencil_name, reference_stencil_name)

        # Check for empty fields
        if not input_fields:
            raise RuntimeError(make_error_msg(input_stencil_name, "No input fields selected"))

        if not reference_fields:
            raise RuntimeError(
                make_error_msg(reference_stencil_name, "No reference fields selected"))

        # Check for inconsistent lengths
        if len(input_fields) != len(reference_fields):
            raise RuntimeError(
                make_error_msg(shared_stencil_name,
                               "Number of input fields does not match number of reference fields"))

        input_serializer = self.__input_stencil_data.serializer
        reference_serializer = self.__reference_stencil_data.serializer

        try:
            # Get savepoint list
            input_savepoint_list = [sp for sp in input_serializer.savepoint_list() if
                                    sp.name.startswith(input_stencil_name)]
            reference_savepoint_list = [sp for sp in reference_serializer.savepoint_list() if
                                        sp.name.startswith(reference_stencil_name)]

            Logger.info("Input savepoints:     %s" % input_savepoint_list)
            Logger.info("Reference savepoints: %s" % reference_savepoint_list)

            # Iterate savepoints & compare
            for input_savepoint, reference_savepoint in zip(input_savepoint_list,
                                                            reference_savepoint_list):
                for input_field_name, reference_field_name in zip(input_fields, reference_fields):
                    input_stage = input_savepoint.metainfo["stage_name"]
                    reference_stage = reference_savepoint.metainfo["stage_name"]

                    # TODO: Provide proper handling if the stages don't match etc...
                    stage = input_stage
                    intent = "in" if input_savepoint.name.endswith("in") else "out"

                    # Load fields
                    input_field = input_serializer.read(input_field_name, input_savepoint)
                    reference_field = reference_serializer.read(reference_field_name,
                                                                reference_savepoint)

                    # Compare
                    self.__comparison_result.compare_fields(stage,
                                                            intent,
                                                            input_field,
                                                            input_field_name,
                                                            input_savepoint,
                                                            reference_field,
                                                            reference_field_name,
                                                            reference_savepoint)
        except SerialboxError as e:
            raise RuntimeError(make_error_msg(shared_stencil_name, e.message))

    @property
    def comparison_result(self):
        return self.__comparison_result
