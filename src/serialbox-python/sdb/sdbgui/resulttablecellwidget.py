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
from PyQt5.QtWidgets import QLabel, QWidget, QHBoxLayout

from .pixmap import Pixmap

class ResultTableCellWidget(QWidget):
    def __init__(self, match):
        super().__init__()
        self.__match = match
        self.__widget_icon = QLabel()

        layout = QHBoxLayout(self)
        layout.addStretch(1)
        layout.addWidget(self.__widget_icon)
        layout.addStretch(1)
        self.setLayout(layout)

    def set_icon(self, draw_success, draw_failure):
        if self.__match:
            if draw_success:
                self.__widget_icon.setPixmap(Pixmap("success.png"))
            else:
                self.__widget_icon.setPixmap(Pixmap("none.png"))
        else:
            if draw_failure:
                self.__widget_icon.setPixmap(Pixmap("failure.png"))
            else:
                self.__widget_icon.setPixmap(Pixmap("none.png"))
