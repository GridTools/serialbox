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

from os import path

from PyQt5.QtGui import QMovie

from sdbcore.logger import Logger


class Movie(QMovie):
    """Safe initialization of QMovie
    """

    ImagePath = path.join(path.dirname(path.realpath(__file__)), "images")

    def __init__(self, filename):
        filename = path.join(Icon.ImagePath, filename)
        if not path.isfile(filename):
            Logger.error("Path to movie does not exist: %s" % filename)

        super().__init__(filename)
