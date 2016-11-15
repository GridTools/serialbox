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

import os
import sys

sys.path.insert(1, os.path.join(os.path.dirname(os.path.realpath(__file__)), "../"))

from optparse import OptionParser
from PyQt5.QtWidgets import QApplication

from sdbcore.logger import Logger, Level
from sdbgui.mainwindow import MainWindow

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-v", "--verbose", dest="verbose", action="store_true",
                      help="enable verbose logging")

    (options, args) = parser.parse_args()

    if options.verbose:
        Logger.set_level(Level.info)
        Logger.enable_serialbox_logging()

    app = QApplication(sys.argv)
    mainWindow = MainWindow()
    sys.exit(app.exec_())
