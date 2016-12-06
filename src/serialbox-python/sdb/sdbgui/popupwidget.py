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

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QWidget


class PopupWidget(QWidget):
    """Popup widget which center itself over the parent window.
    """

    def __init__(self, parent, offset = 0.1, scale = 0.8):
        super().__init__()
        geometry = parent.geometry()
        geometry.setY(geometry.y() + int(offset * parent.geometry().height()))
        geometry.setX(geometry.x() + int(offset * parent.geometry().width()))
        geometry.setWidth(scale * parent.geometry().width())
        geometry.setHeight(scale * parent.geometry().height())
        self.setGeometry(geometry)

        # close if parents dies
        parent.destroyed.connect(self.close)

