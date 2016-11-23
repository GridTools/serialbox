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

from PyQt5.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout

from .resulttablewidget import ResultTableWidget
from .tabstate import TabState


class ResultWindow(QWidget):
    def __init__(self, mainwindow, stencil_field_mapper):
        super().__init__()

        # Data
        self.__stencil_field_mapper = stencil_field_mapper

        # Widgets
        self.__widget_mainwindow = mainwindow
        self.__widget_result_table = ResultTableWidget(self.__stencil_field_mapper)

        hbox = QHBoxLayout()
        hbox.addWidget(self.__widget_result_table)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        self.setLayout(vbox)

    def make_continue(self):
        pass

    def make_back(self):
        self.__widget_mainwindow.switch_to_tab(TabState.Stencil)

    def make_update(self):
        self.__widget_result_table.make_update()
