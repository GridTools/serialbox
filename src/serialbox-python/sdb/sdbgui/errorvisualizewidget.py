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
                             QHBoxLayout, QLineEdit, QLabel, QComboBox)

from sdbcore.logger import Logger
from .errormatplotlibwidget import ErrorMatplotlibWidget

class ErrorVisualizeWidget(QWidget):
    def __init__(self, parent, mainwindow):
        super().__init__(parent)

        self.__widget_matplotlib = ErrorMatplotlibWidget(self)

        self.__widget_label_layer = QLabel("Layer: ", parent=self)
        self.__widget_combobox_layer = QComboBox(self)

        self.__widget_mainwindow = mainwindow

        vbox = QVBoxLayout()
        vbox.addWidget(self.__widget_matplotlib)

        hbox = QHBoxLayout()

        hbox.addStretch(1)
        hbox.addWidget(self.__widget_label_layer)
        hbox.addWidget(self.__widget_combobox_layer)

        vbox.addLayout(hbox)
        self.setLayout(vbox)

    def make_update(self, comparison_result):
        Logger.info("Updating ErrorVisualizeWidget")

        error_positions = comparison_result.get_error_positions()
        self.__widget_matplotlib.draw_layer(error_positions, 1)
