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

from PyQt5.QtWidgets import QWidget, QLabel, QHBoxLayout, QGridLayout


class StencilFieldMetainfoWidget(QWidget):
    def __init__(self, parent):
        super().__init__(parent)

        # Data
        self.__current_fieldname = None
        self.__current_field_metainfo = None

        # Widgets
        self.__widget_label_fieldmetainfo_name = QLabel()
        self.__widget_label_fieldmetainfo_dimensions = QLabel()

        hbox = QHBoxLayout()

        grid_layout = QGridLayout()
        grid_layout.setSpacing(10)
        grid_layout.addWidget(self.__widget_label_fieldmetainfo_name, 1, 0, 1, 5)
        grid_layout.addWidget(self.__widget_label_fieldmetainfo_dimensions, 1, 5, 1, 3)

        hbox.addLayout(grid_layout)
        hbox.addStretch(1)

        self.setLayout(hbox)

    def set_field(self, serializer, field):
        if serializer.has_field(field):
            self.__current_field_metainfo = serializer.get_field_metainfo(field)
            self.__current_fieldname = field
        else:
            self.__current_fieldname = None
            self.__current_field_metainfo = None

        self.update_field()

    def update_field(self):
        if self.__current_fieldname:
            self.__widget_label_fieldmetainfo_name.setText("<b>%s</b>" % self.__current_fieldname)
            self.__widget_label_fieldmetainfo_dimensions.setText(
                "%s" % self.__current_field_metainfo.dims)
        else:
            self.__widget_label_fieldmetainfo.setText("")
