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
from inspect import currentframe, getsourcefile, stack
from os import path
from re import search
from sys import stderr, stdout
from threading import get_ident


class Level(Enum):
    info = 1
    warning = 2
    error = 3
    disabled = 4


class BaseLogger(object):
    def __init__(self):
        self.__level = Level.disabled

    def info(self, msg):
        self.__log(msg, Level.info)

    def warning(self, msg):
        self.__log(msg, Level.warning)

    def error(self, msg):
        self.__log(msg, Level.error)

    def __log(self, msg, level):
        if level.value >= self.__level.value:
            filestream = stderr if level == Level.error else stdout
            file = ""
            line = ""
            class_name = ""

            current_frame = currentframe()
            current_stack = stack()
            try:
                # File name
                file_path_full = getsourcefile(current_frame.f_back.f_back)
                head, tail = path.split(path.dirname(file_path_full))
                file = path.join(tail, path.basename(file_path_full))

                # Line number
                line = ":" + str(current_frame.f_back.f_back.f_lineno)

                # Class name of callee
                class_name_str = str(current_stack[2][0].f_locals["self"].__class__)
                regex_result = search(r"\'([A-Za-z0-9_.]+)\'", class_name_str)
                class_name_with_namespaces = regex_result.groups(1)[0]
                class_name = ":" + class_name_with_namespaces.split(".")[-1]
            except (OSError, KeyError):
                pass
            finally:
                del current_frame
                del current_stack

            msg = "%s%s%s: %s" % (file, line, class_name, msg)

            # Boost.Log style
            print("[%s] [%s] [%s]%s%s" % (
                datetime.now().strftime("%G-%m-%d %H:%M:%S.%f"),
                "{0:#0{1}x}".format(get_ident(), 18),
                level.name,
                " " * (len(Level.info.name) + 4 - len(level.name)),
                msg), file=filestream)

    def set_level(self, level):
        self.__level = level

    def enable_serialbox_logging(self):
        from serialbox import Logging
        Logging.enable()


Logger = BaseLogger()
