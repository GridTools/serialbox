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
##
## Test sdbcore.StencilFieldMapper
##
##===------------------------------------------------------------------------------------------===##


from enum import Enum
from unittest import TestCase, main

from sdbcore.logger import Logger, Level
from sdbcore.stencildata import StencilData
from sdbcore.stencildatalistener import StencilDataFieldListListener
from sdbcore.stencilfieldmapper import StencilFieldMapper
from sdbtest.serializerdatamock import SerializerDataMock


class CheckState(Enum):
    Checked = 0
    Unchecked = 1


class TestFieldListener(StencilDataFieldListListener):
    """Mock the StencilFieldListWidget.
    """

    def __init__(self, stencil_data):
        self.list_value = stencil_data.field_list
        self.list_check = [CheckState.Unchecked] * len(self.list_value)
        self.__name = stencil_data.name

    def move_field(self, name_or_idx, idx):
        if isinstance(name_or_idx, str):
            from_idx = self.list_value.index(name_or_idx)
        else:
            from_idx = name_or_idx

        name = self.list_value[from_idx]
        check_state = self.list_check[from_idx]

        Logger.info("Moving item with name '%s' to index '%i' of '%s'" % (name, idx, self.__name))

        del self.list_value[from_idx]
        del self.list_check[from_idx]

        self.list_value.insert(idx, name)
        self.list_check.insert(idx, check_state)

    def remove_field(self, name):
        pass

    def num_fields(self):
        pass

    def set_field_enabled(self, name_or_idx, enable):
        idx = self.list_value.index(name_or_idx) if isinstance(name_or_idx, str) else name_or_idx
        self.list_check[idx] = CheckState.Checked if enable else CheckState.Unchecked

        Logger.info("Setting enable status of item with name '%s' of '%s' to %s" % (
            self.list_value[idx], self.__name, enable))

    def add_field(self, name, idx=None):
        pass

    def remove_all_fields(self):
        pass

    def __str__(self):
        return str(self.list_value) + "\n" + str(self.list_check)


class TestStencilFieldMapper(TestCase):
    def assertCheckedFieldsEqual(self, field_listener1, field_listener2):
        field_list1 = [f for f, c in zip(field_listener1.list_value, field_listener1.list_check) if
                       c == CheckState.Checked]
        field_list2 = [f for f, c in zip(field_listener2.list_value, field_listener2.list_check) if
                       c == CheckState.Checked]
        self.assertEqual(field_list1, field_list2)

    @staticmethod
    def make_serializer_data(name):
        return SerializerDataMock(name, "dir", "prefix")

    @staticmethod
    def make_stencil_data(name, stencil, num_stage, fields):
        serializer_data = TestStencilFieldMapper.make_serializer_data(name)
        serializer_data.add_stencil(stencil, num_stage, fields)

        stencil_data = StencilData(serializer_data)

        # Register listener
        field_listener = TestFieldListener(stencil_data)
        stencil_data.register_as_field_list_listener(field_listener)

        # Select stencil
        stencil_data.set_selected_stencil(stencil_data.stencil_list.index(stencil))

        return stencil_data, field_listener

    @staticmethod
    def make_stencil_field_mapper(stencil1, num_stage1, fields1, stencil2, num_stage2, fields2):
        input_stencil_data, input_field_listener = TestStencilFieldMapper.make_stencil_data(
            "Input", stencil1, num_stage1, fields1)

        reference_stencil_data, reference_field_listener = TestStencilFieldMapper.make_stencil_data(
            "Reference", stencil2, num_stage2, fields2)

        # Create StencilFieldMapper
        return StencilFieldMapper(input_stencil_data,
                                  reference_stencil_data), input_field_listener, reference_field_listener

    def test_match_fields_1(self):
        field_mapper, input_field_listener, reference_field_listener = TestStencilFieldMapper.make_stencil_field_mapper(
            "s1", 1, ["A", "C", "B"],
            "s1", 1, ["A", "C", "B"])

        # The fields habe been sorted (alphabetically), we update them here
        input_field_listener.list_value = field_mapper.input_field_list
        reference_field_listener.list_value = field_mapper.reference_field_list

        field_mapper.initial_field_match()

        # Check "checked" fields are equal i.e ["A", "B", "C"]
        self.assertCheckedFieldsEqual(input_field_listener, reference_field_listener)
        self.assertEqual(input_field_listener.list_value, ["A", "B", "C"])

        # Check all check states are set to Checked
        self.assertEqual(input_field_listener.list_check[1:], input_field_listener.list_check[:-1])
        self.assertEqual(reference_field_listener.list_check[1:],
                         reference_field_listener.list_check[:-1])

    def test_match_fields_2(self):
        Logger.set_level(Level.info)

        field_mapper, input_field_listener, reference_field_listener = TestStencilFieldMapper.make_stencil_field_mapper(
            "s1", 1, ["A", "B", "C"],
            "s1", 1, ["A", "BB", "B", "C"])

        # The fields habe been sorted (alphabetically), we update them here
        input_field_listener.list_value = field_mapper.input_field_list
        reference_field_listener.list_value = field_mapper.reference_field_list

        field_mapper.initial_field_match()

        # Check "checked" fields are equal i.e ["A", "B", "C"]
        self.assertCheckedFieldsEqual(input_field_listener, reference_field_listener)

        # Check "tt" has been move to the 3rd position i.e the end
        self.assertEqual(reference_field_listener.list_value.index("BB"), 3)
        self.assertEqual(reference_field_listener.list_check[3], CheckState.Unchecked)

    def test_match_fields_3(self):
        field_mapper, input_field_listener, reference_field_listener = TestStencilFieldMapper.make_stencil_field_mapper(
            "s1", 1, ["A", "A1", "A2", "B"],
            "s1", 1, ["A", "A3", "B"])

        # The fields habe been sorted, we update them here
        input_field_listener.list_value = field_mapper.input_field_list
        reference_field_listener.list_value = field_mapper.reference_field_list

        field_mapper.initial_field_match()

        # Check "checked" fields are equal i.e ["A", "B"]
        self.assertCheckedFieldsEqual(input_field_listener, reference_field_listener)


if __name__ == "__main__":
    main()
