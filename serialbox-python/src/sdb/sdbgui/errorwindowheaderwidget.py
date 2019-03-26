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

from PyQt5.QtWidgets import QWidget, QLabel, QVBoxLayout

from sdbcore.logger import Logger


class ErrorWindowHeaderWidget(QWidget):
    def __init__(self, name):
        super().__init__()

        # Data
        self.__name = name

        # Widgets
        self.__widget_label_name = QLabel()
        self.__widget_label_stencil = QLabel()
        self.__widget_label_stage = QLabel()

        vbox = QVBoxLayout()
        vbox.addWidget(self.__widget_label_name)
        vbox.addWidget(self.__widget_label_stencil)
        vbox.addWidget(self.__widget_label_stage)
        self.setLayout(vbox)

    def make_update(self, result_data):
        Logger.info("Updating ErrorWindowHeaderWidget of '%s'" % self.__name)

        id = self.__name.lower()
        self.__widget_label_name.setText(
            "<b>%s  (%s)</b>" % (self.__name, result_data[id + "_field_name"]))
        self.__widget_label_stencil.setText("Stencil: %s" % result_data[id + "_stencil"])
        self.__widget_label_stage.setText("Stage: %s" % result_data[id + "_stage"])

        self.__current_field_name = result_data[id + "_field_name"]
        self.__current_field_metainfo = result_data[id + "_serializer"].get_field_metainfo(
            self.__current_field_name)
