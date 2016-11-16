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

from PyQt5.QtWidgets import QWidget, QComboBox, QLabel, QGridLayout, QListWidget

from sdbcore.logger import Logger


class StencilWidget(QWidget):
    def __init__(self, serializer_data):
        super().__init__()
        Logger.info("Initialize stencil widget: %s" % serializer_data.name)

        # Data
        self.__serializer_data = serializer_data

        # Widget
        self.__name_label = QLabel("<b>%s</b>" % self.__serializer_data.name)
        self.__stencil_label = QLabel("Stencil")
        self.__stencil_list = QComboBox()
        self.__stencil_list.currentIndexChanged.connect(self.update_available_fields)

        self.__field_label = QLabel("Fields")
        self.__field_list = QListWidget()

        # Layout
        grid_layout = QGridLayout()
        grid_layout.setSpacing(5)
        grid_layout.addWidget(self.__name_label, 0, 0)
        grid_layout.addWidget(self.__stencil_label, 1, 0)
        grid_layout.addWidget(self.__stencil_list, 1, 1, 1, 3)
        grid_layout.addWidget(self.__field_label, 2, 0)
        grid_layout.addWidget(self.__field_list, 2, 1, 3, 3)
        self.setLayout(grid_layout)

    def make_update(self):
        Logger.info("Updating stencil widget of %s" % self.__serializer_data.name)
        self.update_avialable_stencils()
        self.update_available_fields()

    def update_avialable_stencils(self):
        self.__stencil_list.clear()
        for stencil in self.__serializer_data.stencils:
            self.__stencil_list.addItem(stencil)

    def update_available_fields(self):
        self.__field_list.clear()
        for field in self.__serializer_data.get_fields_of_stencil(
                self.__stencil_list.currentText()):
            self.__field_list.addItem(field)
