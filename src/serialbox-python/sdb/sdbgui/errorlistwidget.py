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

from PyQt5.QtWidgets import QTableWidget, QHeaderView, QTableWidgetItem, QLineEdit, QHBoxLayout

from sdbcore.logger import Logger


class ErrorListWidget(QTableWidget):
    def __init__(self, parent):
        super().__init__(parent)

    def make_update(self, comparison_result):
        Logger.info("Updating ErrorListWidget")

        list_of_errors = comparison_result.list_of_errors

        self.setColumnCount(3)
        self.setRowCount(len(list_of_errors))
        self.setHorizontalHeaderLabels(
            ["Index", "Input (%s)" % comparison_result["input_field_name"],
             "Reference (%s)" % comparison_result["reference_field_name"]])

        self.setStyleSheet(
            '''
                QTableWidget::item:selected:active {
                    background: #FFFFFF;
                    border-style: solid;
                    border-color: #D4D8DD;
                    border-width: 2px;
                }
            ''')

        self.horizontalHeader().resizeSections(QHeaderView.Stretch)
        self.setEditTriggers(QTableWidget.NoEditTriggers)
        self.setSelectionMode(QTableWidget.NoSelection)

        for idx in range(len(list_of_errors)):
            error = list_of_errors[idx]
            self.setItem(idx, 0, QTableWidgetItem("%s" % str(error[0])))
            self.setItem(idx, 1, QTableWidgetItem("%s" % error[1]))
            self.setItem(idx, 2, QTableWidgetItem("%s" % error[2]))