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

from .logger import Logger
from .serializerdatalistener import SerializerDataListener, SerializerDataDirectoryAndPrefixListener


class SerializerData(object):
    def __init__(self, name, directory="", prefix=""):
        Logger.info("Setup SerializerData of '%s'" % name)

        self.__name = name
        self.__directory = directory
        self.__prefix = prefix
        self.__serializer = None
        self.__data_changed = True

        self.__serializer_data_listeners = []
        self.__serializer_data_directory_and_prefix_listeners = []

    def make_serializer(self, force=False):
        try:
            if force or self.__data_changed:
                Logger.info("Creating new Serializer in SerializerData of '%s'" % self.__name)
                self.__serializer = Serializer(OpenModeKind.Read, self.directory, self.prefix)
                self.__data_changed = False

                for stencil_listener in self.__serializer_data_listeners:
                    stencil_listener.reload()

        except SerialboxError as e:
            self.__serializer = None
            self.__data_changed = True
            raise RuntimeError("<b>%s:</b><br />%s" % (self.name, e))

    def is_valid(self):
        return (self.__serializer is not None)

    def reload(self):
        self.make_serializer(True)

    def __get_directory(self):
        return self.__directory

    def __set_directory(self, directory):
        self.__data_changed = True
        self.__directory = directory

        for listener in self.__serializer_data_directory_and_prefix_listeners:
            listener.directory_changed(directory)

    directory = property(__get_directory, __set_directory)

    def __get_prefix(self):
        return self.__prefix

    def __set_prefix(self, prefix):
        self.__data_changed = True
        self.__prefix = prefix

        for listener in self.__serializer_data_directory_and_prefix_listeners:
            listener.prefix_changed(prefix)

    prefix = property(__get_prefix, __set_prefix)

    def __get_name(self):
        return self.__name

    def __set_name(self, name):
        self.__name = name

    name = property(__get_name, __set_name)

    def __get_serializer(self):
        return self.__serializer

    def __set_serializer(self, serializer):
        self.__data_changed = True
        self.__serializer = serializer

    serializer = property(__get_serializer, __set_serializer)

    def register_as_serializer_data_listener(self, listener):
        if not isinstance(listener, SerializerDataListener):
            raise RuntimeError("listener is not a SerailizerDataListener: %s" % type(listener))

        self.__serializer_data_listeners += [listener]

    def register_as_serializer_data_directory_and_prefix_listener(self, listener):
        if not isinstance(listener, SerializerDataDirectoryAndPrefixListener):
            raise RuntimeError(
                "listener is not a SerializerDataDirectoryAndPrefixListener: %s" % type(listener))

        self.__serializer_data_directory_and_prefix_listeners += [listener]
