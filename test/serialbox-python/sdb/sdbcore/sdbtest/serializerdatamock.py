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

from sdbcore.serializerdata import SerializerData
from sdbtest.serializermock import SerializerMock, OpenModeKind


class SerializerDataMock(SerializerData):
    """Partial mock implementation of sdbcore.SerializerData.
    """

    def __init__(self, *args, **kwargs):
        super(SerializerDataMock, self).__init__(*args, **kwargs)

    def make_serializer_empty(self):
        self.serializer = SerializerMock(OpenModeKind.Read, self.directory, self.prefix)

    def add_stencil(self, stencil, num_stages, fieldlist, invocation_count=1):
        if not self.serializer:
            self.make_serializer_empty()
        self.serializer.add_stencil(stencil, num_stages, fieldlist, invocation_count)
