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

from PyQt5.QtWidgets import QScrollArea, QHBoxLayout, QLabel, QGridLayout, QVBoxLayout, QTableWidget


class ResultTableWidget(QTableWidget):
    def __init__(self, stencil_field_mapper):
        super().__init__()
        self.__stencil_field_mapper = stencil_field_mapper

    def make_update(self):
        comparison_result = self.__stencil_field_mapper.comparison_result

        self.__widget_title = QLabel("<b>%s</b>" % comparison_result["stencil_name"])

        self.setRowCount(15)
        self.setColumnCount(6)
        self.__widget_table.setCellWidget(0, 0, self.__widget_title)

        # self.__grid_layout = QGridLayout()
        # self.__grid_layout.setSpacing(5)
        # print(self.__grid_layout.columnStretch(0))
        # print(self.__grid_layout.columnStretch(1))
        #
        # self.__grid_layout.addWidget(self.__widget_title, 0, 0)
        # self.__next_row = 1
        #
        #
        # self.add_stage("qwe")

        hbox = QHBoxLayout()
        hbox.addWidget(self.__widget_table)
        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        vbox.addStretch(1)

        self.setLayout(vbox)
