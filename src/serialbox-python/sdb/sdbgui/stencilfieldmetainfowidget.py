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

from PyQt5.QtGui import QIcon
from PyQt5.QtWidgets import QWidget, QLabel, QPushButton, QHBoxLayout, QGridLayout

from .fieldmetainfobox import FieldMetainfoBox


class StencilFieldMetainfoWidget(QWidget):
    def __init__(self):
        super().__init__()

        # Data
        self.__current_fieldname = None
        self.__current_field_metainfo = None

        # Widgets
        self.__widget_label_fieldmetainfo_name = QLabel()
        self.__widget_label_fieldmetainfo_dimensions = QLabel()

        self.__widget_button_metainfo = QPushButton()
        self.__widget_button_metainfo.setIcon(QIcon("sdbgui/images/show_all.png"))
        self.__widget_button_metainfo.setEnabled(False)
        self.__widget_button_metainfo.clicked.connect(self.show_field_metainfo_box)

        hbox = QHBoxLayout()

        grid_layout = QGridLayout()
        grid_layout.setSpacing(10)
        grid_layout.addWidget(self.__widget_button_metainfo, 1, 0)
        grid_layout.addWidget(self.__widget_label_fieldmetainfo_name, 1, 1, 1, 5)
        grid_layout.addWidget(self.__widget_label_fieldmetainfo_dimensions, 1, 6, 1, 3)

        hbox.addLayout(grid_layout)
        hbox.addStretch(1)

        self.setLayout(hbox)

    def set_field(self, serializer, field):
        if serializer.has_field(field):
            self.__current_field_metainfo = serializer.get_field_metainfo(field)
            self.__current_fieldname = field
            self.__widget_button_metainfo.setEnabled(True)
        else:
            self.__current_fieldname = None
            self.__current_field_metainfo = None
            self.__widget_button_metainfo.setEnabled(False)

        self.update()

    def update(self):
        if self.__current_fieldname:
            self.__widget_label_fieldmetainfo_name.setText("<b>%s</b>" % self.__current_fieldname)
            self.__widget_label_fieldmetainfo_dimensions.setText(
                "%s" % self.__current_field_metainfo.dims)

            self.__widget_button_metainfo.setEnabled(True)
            self.__widget_button_metainfo.setStatusTip(
                "Field meta-information of %s" % self.__current_fieldname)

        else:
            self.__widget_label_fieldmetainfo.setText("")
            self.__widget_button_metainfo.setStatusTip("")
            self.__widget_button_metainfo.setEnabled(False)

    def show_field_metainfo_box(self):
        self.__widget_field_metainfo_box = FieldMetainfoBox(self, self.__current_fieldname,
                                                            self.__current_field_metainfo)
