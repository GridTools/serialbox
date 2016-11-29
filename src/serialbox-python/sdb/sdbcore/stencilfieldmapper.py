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

from numpy import allclose
from serialbox import SerialboxError

from .comparisonresultlist import ComparisonResultList
from .logger import Logger
from .stencildatalistener import StencilDataDataListener


def field_in_list(field_name, field_list):
    for idx in range(len(field_list)):
        field = field_list[idx]
        if field_name == field:
            return idx
    return -1


class StencilFieldMapper(StencilDataDataListener):
    def __init__(self, input_stencil_data, reference_stencil_data, use_async_api=True):

        self.__use_async_api = use_async_api

        # References
        self.__input_stencil_data = input_stencil_data
        self.__reference_stencil_data = reference_stencil_data

        # Register as listener
        self.__reference_stencil_data.register_as_data_listener(self)
        self.__input_stencil_data.register_as_data_listener(self)

        # Data
        self.__comparison_result_list = ComparisonResultList()
        self.__comparison_result_list_dirty = True

        self.__rtol = 1e-08
        self.__atol = 1e-05

    def __get_atol(self):
        return self.__atol

    def __set_atol(self, atol):
        self.__atol = atol

    atol = property(__get_atol, __set_atol)

    def __get_rtol(self):
        return self.__rtol

    def __set_rtol(self, rtol):
        self.__rtol = rtol

    rtol = property(__get_rtol, __set_rtol)

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
                self.__input_stencil_data.set_field_enabled(idx, True)
                reference_fields_seen += [input_field]

                # Move field to match index in input (i.e idx)
                if idx != idx_in_ref:
                    self.__reference_stencil_data.move_field(input_field, idx)

            else:
                self.__input_stencil_data.set_field_enabled(idx, False)

        for field in reference_fields:
            if field in reference_fields_seen:
                self.__reference_stencil_data.set_field_enabled(field, True)
            else:
                self.__reference_stencil_data.set_field_enabled(field, False)

    def compare_fields(self, input_fields, reference_fields):
        if not self.__comparison_result_list_dirty:
            return

        make_error_msg = lambda title, msg: "<b>%s</b> <br />%s." % (title, msg)

        # Check if atol or rtol are valid
        def check_tolerance(str, value):
            try:
                float(value)
            except ValueError:
                raise RuntimeError(make_error_msg(str,
                                                  "cannot convert '%s' to a valid floating point number" %
                                                  value))

        check_tolerance("Absolute tolerance:", self.__atol)
        check_tolerance("Relative tolerance:", self.__rtol)
        self.__atol = float(self.__atol)
        self.__rtol = float(self.__rtol)

        self.__comparison_result_list.reset()
        self.__comparison_result_list.input_stencil = self.__input_stencil_data.selected_stencil
        self.__comparison_result_list.reference_stencil = self.__reference_stencil_data.selected_stencil

        Logger.info("Comparing fields of input stencil '%s' to fields of reference stencil '%s'" % (
            self.__comparison_result_list.input_stencil,
            self.__comparison_result_list.reference_stencil))
        Logger.info("Input fields:     %s" % input_fields)
        Logger.info("Reference fields: %s" % reference_fields)

        # Check for empty fields
        if not input_fields:
            raise RuntimeError(make_error_msg(self.__comparison_result_list.input_stencil,
                                              "No input fields selected"))

        if not reference_fields:
            raise RuntimeError(
                make_error_msg(self.__comparison_result_list.reference_stencil,
                               "No reference fields selected"))

        # Check for inconsistent lengths
        if len(input_fields) != len(reference_fields):
            raise RuntimeError(
                make_error_msg(self.__comparison_result_list.shared_stencil_name(),
                               "Number of input fields does not match number of reference fields"))

        input_serializer = self.__input_stencil_data.serializer
        input_stencil = self.__input_stencil_data.selected_stencil

        reference_serializer = self.__reference_stencil_data.serializer
        reference_stencil = self.__reference_stencil_data.selected_stencil

        try:
            # Get savepoint list
            input_savepoint_list = [sp for sp in input_serializer.savepoint_list() if
                                    sp.name.startswith(input_stencil)]
            reference_savepoint_list = [sp for sp in reference_serializer.savepoint_list() if
                                        sp.name.startswith(reference_stencil)]

            Logger.info("Input savepoints:     %s" % input_savepoint_list)
            Logger.info("Reference savepoints: %s" % reference_savepoint_list)

            # Iterate savepoints & compare
            for input_savepoint, reference_savepoint in zip(input_savepoint_list,
                                                            reference_savepoint_list):
                fields_at_input_savepoint = input_serializer.fields_at_savepoint(input_savepoint)
                fields_at_reference_savepoint = reference_serializer.fields_at_savepoint(
                    reference_savepoint)

                for input_field_name, reference_field_name in zip(input_fields, reference_fields):
                    input_stage = input_savepoint.metainfo["stage_name"]
                    reference_stage = reference_savepoint.metainfo["stage_name"]

                    stage = input_stage
                    intent = "in" if input_savepoint.name.endswith("in") else "out"

                    # Not every field might be participating in the current stage
                    if input_field_name in fields_at_input_savepoint and reference_field_name in fields_at_reference_savepoint:

                        # Load fields
                        if self.__use_async_api:
                            input_field = input_serializer.read_async(input_field_name,
                                                                      input_savepoint)
                            reference_field = reference_serializer.read_async(reference_field_name,
                                                                              reference_savepoint)

                            input_serializer.wait_for_all()
                            reference_serializer.wait_for_all()
                        else:
                            input_field = input_serializer.read(input_field_name,
                                                                input_savepoint)
                            reference_field = reference_serializer.read(reference_field_name,
                                                                        reference_savepoint)
                        # Compare fields
                        match = allclose(input_field, reference_field, rtol=self.__rtol,
                                         atol=self.__atol)

                        # Append result
                        self.__comparison_result_list.append(match,
                                                             intent,
                                                             input_stage,
                                                             input_field_name,
                                                             input_savepoint,
                                                             input_serializer,
                                                             reference_stage,
                                                             reference_field_name,
                                                             reference_savepoint,
                                                             reference_serializer,
                                                             self.__rtol,
                                                             self.__atol)

        except SerialboxError as e:
            raise RuntimeError(
                make_error_msg(self.__comparison_result_list.shared_stencil_name(), e.message))

        self.__comparison_result_list_dirty = False

    def data_changed(self):
        self.__comparison_result_list_dirty = True

    @property
    def comparison_result_list(self):
        return self.__comparison_result_list
