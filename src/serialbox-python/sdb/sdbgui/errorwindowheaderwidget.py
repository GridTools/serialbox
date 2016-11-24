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
from PyQt5.QtWidgets import QWidget, QLabel, QPushButton, QHBoxLayout, QVBoxLayout

from sdbcore import Logger
from .fieldmetainfobox import FieldMetainfoBox


class ErrorWindowHeaderWidget(QWidget):
    def __init__(self, name):
        super().__init__()

        # Data
        self.__name = name

        # Widgets
        self.__widget_label_name = QLabel()
        self.__widget_label_stencil = QLabel()
        self.__widget_label_stage = QLabel()

        self.__widget_button_field_metainfo = QPushButton("")
        self.__widget_button_field_metainfo.setIcon(QIcon("sdbgui/images/show_all.png"))
        self.__widget_button_field_metainfo.clicked.connect(self.show_field_metainfo_box)

        # Layout
        hbox_bottom = QHBoxLayout()
        hbox_bottom.addWidget(self.__widget_button_field_metainfo)
        hbox_bottom.addStretch(1)

        vbox = QVBoxLayout()
        vbox.addWidget(self.__widget_label_name)
        vbox.addWidget(self.__widget_label_stencil)
        vbox.addWidget(self.__widget_label_stage)
        vbox.addLayout(hbox_bottom)
        self.setLayout(vbox)

    def make_update(self, result_data):
        Logger.info("Updating ErrorWindowHeaderWidget of '%s'" % self.__name)

        id = self.__name.lower()
        self.__widget_label_name.setText("<b>%s</b>" % self.__name)
        self.__widget_label_stencil.setText("Stencil: %s" % result_data[id + "_stencil"])
        self.__widget_label_stage.setText("Stage: %s" % result_data[id + "_stage"])
        self.__widget_button_field_metainfo.setText(result_data[id + "_field_name"])

        self.__current_field_name = result_data[id + "_field_name"]
        self.__current_field_metainfo = result_data[id + "_serializer"].get_field_metainfo(
            self.__current_field_name)

    def show_field_metainfo_box(self):
        self.__widget_field_metainfo_box = FieldMetainfoBox(self,
                                                            self.__current_field_name,
                                                            self.__current_field_metainfo)
