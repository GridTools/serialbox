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

from sdbgui.resulttablewidget import ResultTableWidget
from sdbgui.tabstate import TabState
from sdbgui.tabwindow import TabWindow


class ResultWindow(QWidget, TabWindow):
    def __init__(self, mainwindow, stencilwindow, stencil_field_mapper):
        super().__init__()

        # Data
        self.__stencil_field_mapper = stencil_field_mapper

        # Widgets
        self.__widget_mainwindow = mainwindow
        self.__widget_stencilwindow = stencilwindow

        self.__widget_result_table = ResultTableWidget(self, self.__stencil_field_mapper)

        hbox = QHBoxLayout()
        hbox.addWidget(self.__widget_result_table)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        self.setLayout(vbox)

    def make_continue(self):
        self.try_switch_to_error_tab()

    def make_back(self):
        self.__widget_mainwindow.switch_to_tab(TabState.Stencil)

    def make_update(self):
        if not self.__widget_stencilwindow.update_comparison_result():
            self.make_back()
        self.__widget_result_table.make_update()

    def try_switch_to_error_tab(self):
        return self.__widget_result_table.try_switch_to_error_tab()

    @property
    def widget_mainwindow(self):
        return self.__widget_mainwindow
