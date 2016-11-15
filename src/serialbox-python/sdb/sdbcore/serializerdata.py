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

from serialbox import Serializer, SerialboxError, OpenModeKind


class SerializerData(object):
    def __init__(self, name, directory="", prefix="", serializer=None):
        self.__name = name
        self.__directory = directory
        self.__prefix = prefix
        self.__serializer = None

    def make_serializer(self):
        try:
            self.__serializer = Serializer(OpenModeKind.Read, self.directory, self.prefix)
        except SerialboxError as e:
            self.__serializer = None
            raise RuntimeError("<b>%s:</b><br />%s" % (self.name, e))

    def is_valid(self):
        return (self.__serializer is not None)

    @property
    def directory(self):
        return self.__directory

    @directory.setter
    def directory(self, directory):
        self.__directory = directory

    @property
    def prefix(self):
        return self.__prefix

    @prefix.setter
    def prefix(self, prefix):
        self.__prefix = prefix

    @property
    def name(self):
        return self.__name

    @property
    def serializer(self):
        return self.__serializer

    @serializer.setter
    def serializer(self, serializer):
        self.__serializer = serializer
