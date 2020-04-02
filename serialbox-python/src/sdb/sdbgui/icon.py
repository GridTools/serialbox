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

from PyQt5.QtGui import QIcon

from sdbcore.logger import Logger


class Icon(QIcon):
    """Safe initialization of QIcon
    """

    ImagePath = path.join(path.dirname(path.realpath(__file__)), "images")

    def __init__(self, filename):
        filename = path.join(Icon.ImagePath, filename)
        if not path.isfile(filename):
            Logger.error("Path to icon does not exist: %s" % filename)

        super().__init__(filename)
