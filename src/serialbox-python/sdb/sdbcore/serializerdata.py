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

from sdbcore.logger import Logger

class SerializerData(object):
    def __init__(self, name, directory="", prefix=""):
        Logger.info("Setup SerializerData of '%s'" % name)

        self.__name = name
        self.__directory = directory
        self.__prefix = prefix
        self.__serializer = None
        self.__data_changed = True
        self.__stencil_listeners = []

    def make_serializer(self, force=False):
        try:
            if force or self.__data_changed:
                Logger.info("Creating new Serializer in SerializerData of '%s'" % self.__name)
                self.__serializer = Serializer(OpenModeKind.Read, self.directory, self.prefix)
                self.__data_changed = False

                for stencil_listener in self.__stencil_listeners:
                    stencil_listener.reload()

        except SerialboxError as e:
            self.__serializer = None
            self.__data_changed = True
            raise RuntimeError("<b>%s:</b><br />%s" % (self.name, e))

    def is_valid(self):
        return (self.__serializer is not None)

    def reload(self):
        self.make_serializer(True)

    @property
    def directory(self):
        return self.__directory

    @directory.setter
    def directory(self, directory):
        self.__data_changed = True
        self.__directory = directory

    @property
    def prefix(self):
        return self.__prefix

    @prefix.setter
    def prefix(self, prefix):
        self.__data_changed = True
        self.__prefix = prefix

    @property
    def name(self):
        return self.__name

    @property
    def serializer(self):
        return self.__serializer

    @serializer.setter
    def serializer(self, serializer):
        self.__data_changed = True
        self.__serializer = serializer

    def register_as_stencil_listener(self, listener):
        self.__stencil_listeners += [listener]
