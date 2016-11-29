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

from PyQt5.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QTabWidget, QSizePolicy, QLabel, QProgressDialog

from sdbcore.logger import Logger
from .errorlistwidget import ErrorListWidget
from .errorvisualizewidget import ErrorVisualizeWidget
from .errorwindowheaderwidget import ErrorWindowHeaderWidget
from .tabstate import TabState


class ErrorWindow(QWidget):
    def __init__(self, mainwindow):
        super().__init__()

        # Data
        self.__result_data = None
        self.__result_data_is_dirty = True

        # Widgets
        self.__widget_mainwindow = mainwindow

        self.__widget_input_header = ErrorWindowHeaderWidget("Input")
        self.__widget_reference_header = ErrorWindowHeaderWidget("Reference")

        self.__widget_error_tab = QTabWidget(self)
        self.__widget_error_tab.addTab(ErrorListWidget(self, mainwindow), "List")
        self.__widget_error_tab.addTab(ErrorVisualizeWidget(self, mainwindow), "Visualize")
        self.__widget_error_tab.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        hbox_header = QHBoxLayout()
        hbox_header.addWidget(self.__widget_input_header)
        hbox_header.addWidget(self.__widget_reference_header)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox_header)
        vbox.addWidget(self.__widget_error_tab)

        self.setLayout(vbox)

    def set_result_data(self, result_data):
        if self.__result_data != result_data:
            self.__result_data = result_data
            self.__result_data_is_dirty = True

    def make_continue(self):
        pass

    def make_back(self):
        self.__widget_mainwindow.switch_to_tab(TabState.Result)

    def make_update(self):
        if self.__result_data_is_dirty:
            Logger.info("Updating ErrorTab tab")

            self.__widget_input_header.make_update(self.__result_data)
            self.__widget_reference_header.make_update(self.__result_data)

            for idx in range(self.__widget_error_tab.count()):
                self.__widget_error_tab.widget(idx).make_update(self.__result_data)

            self.__result_data_is_dirty = False

    @property
    def widget_mainwindow(self):
        return self.__widget_mainwindow
