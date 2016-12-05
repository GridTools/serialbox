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

from sdbcore.comparisonresultlist import ComparisonResultList
from sdbcore.logger import Logger
from sdbcore.stencildatalistener import StencilDataDataListener
from serialbox import SerialboxError


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
        self.__comparison_result_list = []
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

    def initial_field_match(self):
        Logger.info("Matching fields ...")

        input_fields = self.__input_stencil_data.field_list
        input_fields_state = [False] * len(input_fields)

        reference_fields = self.__reference_stencil_data.field_list
        reference_fields_state = [False] * len(reference_fields)
        reference_fields_seen = []

        #
        # 1. Sweep - Set enable state of reference and input field list
        #
        Logger.info("Set enable state of reference and input field list")
        for idx in range(len(input_fields)):
            input_field = input_fields[idx]

            enable = False
            if input_field in reference_fields:
                reference_fields_seen += [input_field]
                enable = True

            self.__input_stencil_data.set_field_enabled(input_field, enable)
            input_fields_state[idx] = enable

        for idx in range(len(reference_fields)):
            reference_field = reference_fields[idx]

            enable = reference_field in reference_fields_seen

            self.__reference_stencil_data.set_field_enabled(reference_field, enable)
            reference_fields_state[idx] = enable

        #
        # 2. Sweep - Move all disabled fields to the back
        #
        Logger.info("Move disabled fields of input and reference field list to the back")

        input_fields_reordered = input_fields[:]
        for idx in range(len(input_fields)):
            if not input_fields_state[idx]:
                self.__input_stencil_data.move_field(input_fields[idx], len(input_fields) - 1)
                input_fields_reordered.insert(len(input_fields) - 1,
                                              input_fields_reordered.pop(idx))

        reference_fields_reordered = reference_fields[:]
        for idx in range(len(reference_fields)):
            if not reference_fields_state[idx]:
                self.__reference_stencil_data.move_field(reference_fields[idx],
                                                         len(reference_fields) - 1)
                reference_fields_reordered.insert(len(reference_fields) - 1,
                                                  reference_fields_reordered.pop(idx))
        #
        # 3. Sweep - Move the enabled reference fields to match the input positions
        #
        Logger.info("Move reference fields to match input field list")
        for idx in range(input_fields_state.count(True)):
            reference_field = reference_fields_reordered[idx]
            self.__reference_stencil_data.move_field(reference_field, idx)

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

        self.__comparison_result_list = ComparisonResultList()
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

                input_stage = input_savepoint.metainfo["stage_name"]
                reference_stage = reference_savepoint.metainfo["stage_name"]

                invocation_count_input = input_savepoint.metainfo["invocation_count"]
                invocation_count_refrence = reference_savepoint.metainfo["invocation_count"]

                # Invocation counts have to match
                if invocation_count_input != invocation_count_refrence:
                    continue

                for input_field_name, reference_field_name in zip(input_fields, reference_fields):

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
                        if input_field.shape == reference_field.shape:
                            match = allclose(input_field, reference_field, rtol=self.__rtol,
                                             atol=self.__atol)
                        else:
                            match = False

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
                                                             self.__atol,
                                                             invocation_count_input)

        except SerialboxError as e:
            raise RuntimeError(
                make_error_msg(self.__comparison_result_list.shared_stencil_name(), e.message))

        self.__comparison_result_list_dirty = False

    def data_changed(self):
        self.__comparison_result_list_dirty = True

    @property
    def comparison_result_list(self):
        return self.__comparison_result_list

    @property
    def input_field_list(self):
        return self.__input_stencil_data.field_list

    @property
    def reference_field_list(self):
        return self.__reference_stencil_data.field_list
