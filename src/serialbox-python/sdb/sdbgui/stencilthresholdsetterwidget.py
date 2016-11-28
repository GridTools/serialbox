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

from PyQt5.QtGui import QPixmap
from PyQt5.QtWidgets import QWidget, QLineEdit, QLabel, QHBoxLayout

from sdbcore.logger import Logger


class StencilThresholdSetterWidget(QWidget):
    def __init__(self, parent, stencil_field_mapper):
        super().__init__(parent)

        # Data
        self.__stencil_field_mapper = stencil_field_mapper

        # Widget
        self.__widget_label_rtol = QLabel("Relative tolerance: ")
        self.__widget_lineedit_rtol = QLineEdit("%s" % stencil_field_mapper.rtol)
        self.__widget_lineedit_rtol.textChanged[str].connect(self.set_rtol)
        self.__widget_icon_rtol = QLabel("")

        self.__widget_label_atol = QLabel("Absolute tolerance: ")
        self.__widget_lineedit_atol = QLineEdit("%s" % stencil_field_mapper.atol)
        self.__widget_lineedit_atol.textChanged[str].connect(self.set_atol)
        self.__widget_icon_atol = QLabel("")

        hbox = QHBoxLayout()
        hbox.addWidget(self.__widget_label_rtol)
        hbox.addWidget(self.__widget_lineedit_rtol)
        hbox.addWidget(self.__widget_icon_rtol)
        hbox.addStretch(1)
        hbox.addWidget(self.__widget_label_atol)
        hbox.addWidget(self.__widget_lineedit_atol)
        hbox.addWidget(self.__widget_icon_atol)
        hbox.addStretch(1)
        self.setLayout(hbox)

    def set_atol(self, atol):
        Logger.info("Setting absolute tolerance to: %s" % atol)
        self.__stencil_field_mapper.atol = atol

        try:
            float(atol)
        except ValueError:
            self.__widget_icon_atol.setPixmap(QPixmap("sdbgui/images/error.png"))
            self.__widget_icon_atol.setToolTip(
                "%s cannot be converted to a valid floating point number" % atol)
            return
        self.__widget_icon_atol.clear()

    def set_rtol(self, rtol):
        Logger.info("Setting relative tolerance to: %s" % rtol)
        self.__stencil_field_mapper.rtol = rtol

        try:
            float(rtol)
        except ValueError:
            self.__widget_icon_rtol.setPixmap(QPixmap("sdbgui/images/error.png"))
            self.__widget_icon_rtol.setToolTip(
                "%s cannot be converted to a valid floating point number" % rtol)
            return
        self.__widget_icon_rtol.clear()
