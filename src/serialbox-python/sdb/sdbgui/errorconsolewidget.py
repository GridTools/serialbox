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

from PyQt5.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QLabel

from sdbgui.errorconsoleipythonwidget import SDB_HAS_IPYTHON, SDB_IPYTHON_IMPORT_ERROR


class ErrorConsoleWidget(QWidget):
    def __init__(self, parent, mainwindow):
        super().__init__(parent)

        self.__widget_mainwindow = mainwindow

        if SDB_HAS_IPYTHON:
            from sdbgui.errorconsoleipythonwidget import ErrorConsoleIPythonWidget
            self.__widget_ipython = ErrorConsoleIPythonWidget(parent=self)
            layout = QVBoxLayout()
            layout.addWidget(self.__widget_ipython)
            self.setLayout(layout)
        else:
            self.__widget_label_disclaimer = QLabel(parent=self)

            if SDB_IPYTHON_IMPORT_ERROR:
                self.__widget_label_disclaimer.setText(
                    "IPython is not available.\n%s" % SDB_IPYTHON_IMPORT_ERROR)
            else:
                self.__widget_label_disclaimer.setText("Embedded IPython console is disabled.")

            hbox = QHBoxLayout()
            hbox.addWidget(self.__widget_label_disclaimer)
            hbox.addStretch(1)

            vbox = QVBoxLayout()
            vbox.addLayout(hbox)
            vbox.addStretch(1)

            self.setLayout(vbox)

    def make_update(self, comparison_result):
        if SDB_HAS_IPYTHON:
            self.__widget_ipython.push_variables(
                {"input_serializer": comparison_result["input_serializer"],
                 "input_savepoint": comparison_result["input_savepoint"],
                 "input_field": comparison_result.input_field,
                 "reference_serializer": comparison_result["reference_serializer"],
                 "reference_savepoint": comparison_result["reference_savepoint"],
                 "reference_field": comparison_result.reference_field,
                 })
