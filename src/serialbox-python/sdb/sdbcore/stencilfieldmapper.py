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

    def match_fields(self):
        Logger.info("Matching fields")

        input_fields = self.__input_stencil_data.field_list
        reference_fields = self.__reference_stencil_data.field_list
        reference_field_seen = [False] * len(reference_fields)

        # Rearrange the field list in the reference serializer
        for idx in range(len(input_fields)):
            input_field = input_fields[idx]

            idx_in_ref = field_in_list(input_field, reference_fields)

            if idx_in_ref != -1:
                self.__input_stencil_data.set_enable_field(idx, True)
                reference_field_seen[idx_in_ref] = True

                # Move idx_in_ref to match index in input (i.e idx)
                if idx != idx_in_ref:
                    self.__reference_stencil_data.move_field(idx_in_ref, idx)

            else:
                self.__input_stencil_data.set_enable_field(idx, False)

        # Disable all unseen fields
        for idx in range(len(reference_field_seen)):
            self.__reference_stencil_data.set_enable_field(idx, reference_field_seen[idx])

