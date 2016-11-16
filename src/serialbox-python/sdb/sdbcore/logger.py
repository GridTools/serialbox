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

from datetime import datetime
from enum import Enum
from sys import stderr, stdout
from threading import get_ident


class Level(Enum):
    info = 1
    warning = 2
    error = 3


class BaseLogger(object):
    def __init__(self):
        self.__level = Level.warning

    def info(self, msg):
        self.__log(msg, Level.info)

    def warning(self, msg):
        self.__log(msg, Level.warning)

    def error(self, msg):
        self.__log(msg, Level.error)

    def __log(self, msg, level):
        file = stderr if level == Level.error else stdout
        if level.value >= self.__level.value:
            # Boost.Log style
            print("[%s] [%s] [%s]%s%s" % (
                datetime.now().strftime("%G-%m-%d %H:%M:%S.%f"),
                "{0:#0{1}x}".format(get_ident(), 18),
                level.name,
                " " * (len(Level.info.name) + 4 - len(level.name)),
                msg), file=file)
        file.flush()

    def set_level(self, level):
        self.__level = level

    def enable_serialbox_logging(self):
        from serialbox import Logging
        Logging.enable()


Logger = BaseLogger()
