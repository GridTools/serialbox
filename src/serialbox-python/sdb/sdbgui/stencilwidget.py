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

from PyQt5.QtWidgets import (QWidget, QGridLayout, QLabel, QLineEdit, QPushButton, QFileDialog,
                             QComboBox)

from sdbcore import Logger

class StencilWidget(QWidget):
    def __init__(self, serializer_data):
        super().__init__()

        self.__serializer_data = serializer_data

        self.__name = serializer_data.name
        Logger.info("Initialize serializer widget: %s" % self.__name)

