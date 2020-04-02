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
## Mock class of serialbox.Serializer
##
##===------------------------------------------------------------------------------------------===##

from serialbox import MetainfoMap, Savepoint, OpenModeKind

__all__ = ["OpenModeKind"]


class SerializerMock(object):
    """Partial mock implementation of serialbox.Serializer used in sdb.
    """

    def __init__(self, mode, directory, prefix):
        self.__mode = mode
        self.__directory = directory
        self.__prefix = prefix

        self.__global_metainfo = MetainfoMap()
        self.__savepoint_list = []
        self.__field_list_of_savepoint = dict()

    def add_stencil(self, stencil, num_stages, fieldlist, invocation_count=1):
        for ic in range(invocation_count):
            for i in range(num_stages):
                self.__add_field_to_stage(stencil, "stage-" + str(i), i, ic, list(fieldlist))

    def __add_field_to_stage(self, stencil, stage_name, stage_id, invocation_count, fieldlist):
        """Add field as in/output of the specfied stage and stencil.
        """

        #
        # Add stencil to global meta-information
        #
        if not self.__global_metainfo.has_key("stencils"):
            self.__global_metainfo.insert("stencils", [stencil])
        elif not stencil in self.__global_metainfo["stencils"]:
            stencils = self.__global_metainfo["stencils"]
            del self.global_metainfo["stencils"]
            self.__global_metainfo.insert("stencils", stencils + [stencil])

        #
        # Append input savepoint
        #
        input_savepoint = Savepoint(stencil + "__in",
                                    {"stage_name": stage_name, "stage_id": stage_id,
                                     "invocation_count": invocation_count})

        self.__savepoint_list += [input_savepoint]
        self.__field_list_of_savepoint[input_savepoint] = fieldlist

        #
        # Append output savepoint
        #
        output_savepoint = Savepoint(stencil + "__out",
                                     {"stage_name": stage_name, "stage_id": stage_id + 1,
                                      "invocation_count": invocation_count})

        self.__savepoint_list += [output_savepoint]
        self.__field_list_of_savepoint[output_savepoint] = fieldlist

    @property
    def global_metainfo(self):
        return self.__global_metainfo

    @property
    def mode(self):
        return self.__mode

    @property
    def prefix(self):
        return self.__prefix

    @property
    def directory(self):
        return self.__directory

    @property
    def global_metainfo(self):
        return self.__global_metainfo

    def savepoint_list(self):
        return self.__savepoint_list

    def fields_at_savepoint(self, savepoint):
        return self.__field_list_of_savepoint[savepoint]

    def __repr__(self):
        s = "<SerializerMock\n metainfo: " + str(self.__global_metainfo) + "\n savepont_list:\n"
        for sp in self.__savepoint_list:
            s += "  " + str(sp)
        return s

    def __str__(self):
        return self.__repr__()
