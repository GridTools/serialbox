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
from PyQt5.QtWidgets import (QTableWidget, QHeaderView, QTableWidgetItem, QWidget, QVBoxLayout,
                             QHBoxLayout, QLineEdit, QLabel)

from sdbcore.logger import Logger


class ErrorListWidget(QWidget):
    def __init__(self, parent, mainwindow):
        super().__init__(parent)

        self.__widget_table = QTableWidget(self)
        self.__widget_mainwindow = mainwindow

        self.__widget_label_num_errors = QLabel("Find error by index: ", parent=self)
        self.__widget_label_num_errors.setStatusTip("Find an error by specifying the index")

        self.__widget_lineedit_num_errors = QLineEdit("", parent=self)
        self.__widget_lineedit_num_errors.textEdited[str].connect(self.find_error_by_index)

        vbox = QVBoxLayout()
        vbox.addWidget(self.__widget_table)

        hbox = QHBoxLayout()
        hbox.addWidget(self.__widget_label_num_errors)
        hbox.addWidget(self.__widget_lineedit_num_errors)
        hbox.addStretch(1)

        vbox.addLayout(hbox)
        self.setLayout(vbox)

    def make_update(self, comparison_result):
        Logger.info("Updating ErrorListWidget")

        list_of_errors = comparison_result.get_error_list()

        self.__widget_table.setColumnCount(4)
        self.__widget_table.setRowCount(len(list_of_errors))
        self.__widget_table.setHorizontalHeaderLabels(
            ["Index", "Input (%s)" % comparison_result["input_field_name"],
             "Reference (%s)" % comparison_result["reference_field_name"], "Error"])

        self.__widget_table.setStyleSheet(
            '''
                QTableWidget::item:selected:active {
                    background: transparent;
                    border-width: 0px;
                }
            ''')

        self.__widget_table.horizontalHeader().resizeSections(QHeaderView.Stretch)
        self.__widget_table.setEditTriggers(QTableWidget.NoEditTriggers)

        # Sort errors
        list_of_errors.sort(key=lambda x: x[0][-1])

        for idx in range(len(list_of_errors)):
            error = list_of_errors[idx]
            self.__widget_table.setItem(idx, 0, QTableWidgetItem(
                "%s" % str(error[0]) if len(error[0]) != 1 else str(error[0][0])))
            self.__widget_table.setItem(idx, 1, QTableWidgetItem("%s" % error[1]))
            self.__widget_table.setItem(idx, 2, QTableWidgetItem("%s" % error[2]))
            self.__widget_table.setItem(idx, 3, QTableWidgetItem("%s" % abs(error[2] - error[1])))

    def find_error_by_index(self, index):
        item = self.__widget_table.findItems(index, Qt.MatchExactly)

        if len(item) == 0:
            # Try with whitespaces after comma
            item = self.__widget_table.findItems(index.replace(",", ", "), Qt.MatchExactly)

        if item:
            self.__widget_table.selectRow(item[0].row())
