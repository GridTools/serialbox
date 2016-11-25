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

from PyQt5.QtWidgets import QTableWidget, QHeaderView

from sdbcore.logger import Logger


class ErrorListWidget(QTableWidget):
    def __init__(self, parent):
        super().__init__(parent)

    def make_update(self, result_data):
        Logger.info("Updating ErrorListWidget")

        self.setColumnCount(3)
        self.setHorizontalHeaderLabels(["Index", "Input", "Reference"])

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
