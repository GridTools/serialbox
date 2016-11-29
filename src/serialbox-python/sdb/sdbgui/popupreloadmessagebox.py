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

from PyQt5.QtWidgets import QMessageBox

from sdbcore.logger import Logger

class PopupReloadMessageBox(QMessageBox):
    def __init__(self, parent, path):
        super().__init__(parent)
        self.setWindowTitle("Reload serializers")
        self.setText(path)
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Ok)
        self.show()