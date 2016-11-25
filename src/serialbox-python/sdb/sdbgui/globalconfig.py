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

from collections import MutableMapping


class GlobalConfig(MutableMapping):
    """Expose configuration options (passed via command-line)
    """

    class __GlobalConfig:
        def __init__(self):
            self.config = dict()

    __instance = None

    def __init__(self):
        if not GlobalConfig.__instance:
            GlobalConfig.__instance = GlobalConfig.__GlobalConfig()

    def __getitem__(self, key):
        return self.__instance.config[self.__keytransform__(key)]

    def __setitem__(self, key, value):
        self.__instance.config[self.__keytransform__(key)] = value

    def __delitem__(self, key):
        del self.__instance.config[self.__keytransform__(key)]

    def __iter__(self):
        return iter(self.__instance.config)

    def __len__(self):
        return len(self.__instance.config)

    def __keytransform__(self, key):
        return key
