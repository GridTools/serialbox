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

from PyQt5.QtWidgets import QWidget
from sdbcore.version import Version

class ErrorListWidget(QWidget):

    def __init__(self, errorwindow):
        super().__init__(errorwindow)

        self.__widget_errorwindow = errorwindow

        #
        # if Version().ipython_version():
        #     from .ipythonwidget import
