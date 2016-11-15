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

from PyQt5.QtCore import QT_VERSION_STR
from PyQt5.QtWidgets import QMessageBox

from sdbcore.version import Version
from sdbcore import Logger

class AboutMessageBox(object):
    def __init__(self, parent):
        about_txt = ("sdb (%s)" % Version().sdb_version(),
                     "Serialbox (%s)" % Version().serialbox_version(),
                     "numpy (%s)" % Version().numpy_version(),
                     "PyQt5 (%s)" % QT_VERSION_STR,
                     "",
                     "Copyright (c) 2016, Fabian Thuering",
                     "",
                     "All rights reserved.",
                     "",
                     "The program is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE "
                     "WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.",
                     "")

        Logger.info("showing about message box")
        msgbox = QMessageBox(parent)
        msgbox.setWindowTitle("About sdb")
        msgbox.setText("\n".join(about_txt))
        msgbox.setStandardButtons(QMessageBox.Cancel)
        msgbox.show()
