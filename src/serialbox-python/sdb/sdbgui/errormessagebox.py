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

class ErrorMessageBox():
    def __init__(self, parent, msg):
        msgbox = QMessageBox(parent)
        msgbox.setWindowTitle("Error")
        msgbox.setIcon(QMessageBox.Critical)
        msgbox.setText(msg)
        msgbox.setStandardButtons(QMessageBox.Ok)
        msgbox.show()
