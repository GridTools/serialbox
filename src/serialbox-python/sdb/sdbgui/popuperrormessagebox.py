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

from sdbcore import Logger


class PopupErrorMessageBox(QMessageBox):
    def __init__(self, parent, msg):
        super().__init__(parent)
        Logger.error(msg.replace("<b>", "").replace("</b>", "").replace("<br />", ":"))
        self.setWindowTitle("Error")
        self.setIcon(QMessageBox.Critical)
        self.setText(msg)
        self.setStandardButtons(QMessageBox.Ok)
        self.show()
