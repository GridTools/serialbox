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

from PyQt5.QtWidgets import QWidget, QLabel, QGridLayout

from sdbcore.logger import Logger
from sdbgui.stencilfieldlistwidget import StencilFieldListWidget
from sdbgui.stencillistwidget import StencilListWidget


class StencilWidget(QWidget):
    def __init__(self, stencilwindow, stencil_data, widget_fieldmetainfo):
        super().__init__()

        # Data
        data = stencil_data
        self.__stencil_data = data
        self.__name = self.__stencil_data.name

        # Widgets
        self.__widget_stencilwindow = stencilwindow

        self.__widget_label_name = QLabel("<b>%s</b>" % self.__name)
        self.__widget_label_stencil = QLabel("Stencil")
        self.__widget_label_stencil.setStatusTip("Stencil to compare")
        self.__widget_stencil_list = StencilListWidget(self, self.__stencil_data)
        self.__widget_stencil_list.currentIndexChanged[int].connect(self.set_stencil_index)

        self.__widget_label_field = QLabel("Fields")
        self.__widget_label_field.setStatusTip("Fields of the stencil to compare")

        self.__widget_listview_field_list = StencilFieldListWidget(self, self.__stencil_data,
                                                                   widget_fieldmetainfo)

        # Layout
        grid_layout = QGridLayout()
        grid_layout.setSpacing(5)
        grid_layout.addWidget(self.__widget_label_name, 0, 0)
        grid_layout.addWidget(self.__widget_label_stencil, 1, 0)
        grid_layout.addWidget(self.__widget_stencil_list, 1, 1, 1, 3)
        grid_layout.addWidget(self.__widget_label_field, 2, 0)
        grid_layout.addWidget(self.__widget_listview_field_list, 2, 1, 3, 3)
        self.setLayout(grid_layout)

    def set_stencil_index(self, idx):
        self.__stencil_data.set_selected_stencil(idx)
        self.update_available_fields()
        self.__widget_stencilwindow.match_fields()

    def make_update(self):
        Logger.info("Updating StencilWidget of '%s'" % self.__name)
        self.update_avialable_stencils()
        self.update_available_fields()

    def update_avialable_stencils(self):
        Logger.info("Updating available stencils of '%s'" % self.__name)
        self.__stencil_data.update_stencil_list()

    def update_available_fields(self):
        Logger.info("Updating ListView of StencilWidget '%s' to match stencil '%s'" % (
            self.__name, self.__stencil_data.selected_stencil))
        self.__stencil_data.update_field_list()

    @property
    def fields(self):
        return self.__widget_listview_field_list.fields
