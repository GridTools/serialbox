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
## Simplify listener tests
##
##===------------------------------------------------------------------------------------------===##


class TestListener(object):
    def __init__(self):
        self.__count = 0

    def increment_count(self):
        self.__count += 1

    @property
    def count(self):
        return self.__count
