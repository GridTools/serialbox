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

from PyQt5.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout, QPushButton, QLabel

from sdbcore import Logger
from .errormessagebox import ErrorMessageBox
from .setupwidget import SetupWidget
from .tabstate import TabState


class SetupWindow(QWidget):
    def __init__(self, mainwindow, input_serializer_data, reference_serializer_data):
        super().__init__()

        # Data
        self.__widget_mainwindow = mainwindow
        self.__input_serializer_data = input_serializer_data
        self.__reference_serializer_data = reference_serializer_data

        # Widgets
        self.__widget_setup_input = SetupWidget(self, self.__input_serializer_data)
        self.__widget_setup_reference = SetupWidget(self, self.__reference_serializer_data)

        self.__widget_button_continue = QPushButton("Continue")
        self.__widget_button_continue.clicked.connect(self.make_continue)

        self.__widget_button_back = QPushButton("Back")
        self.__widget_button_back.setEnabled(False)

        self.__widget_label_explain = QLabel("Explain the process here!")

        hbox = QHBoxLayout()
        hbox.addWidget(self.__widget_setup_input)
        hbox.addWidget(self.__widget_setup_reference)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        vbox.addStretch(1)
        vbox.addWidget(self.__widget_label_explain)
        vbox.addStretch(1)

        hbox_button = QHBoxLayout()
        hbox_button.addStretch(1)
        hbox_button.addWidget(self.__widget_button_back)
        hbox_button.addWidget(self.__widget_button_continue)

        vbox.addLayout(hbox_button)

        self.setLayout(vbox)

    def make_continue(self):
        Logger.info("Setup Serializers if needed")
        try:
            self.__input_serializer_data.make_serializer()
            self.__reference_serializer_data.make_serializer()
        except RuntimeError as e:
            ErrorMessageBox(self, "%s" % e)

        if self.__input_serializer_data.is_valid() and self.__reference_serializer_data.is_valid():
            self.__widget_mainwindow.set_tab_highest_valid_state(TabState.Stencil)
            self.__widget_mainwindow.switch_to_tab(TabState.Stencil)
        else:
            self.__widget_mainwindow.set_tab_highest_valid_state(TabState.Setup)

    def make_back(self):
        pass

    def make_update(self):
        self.__widget_setup_input.check_if_directory_is_valid()
        self.__widget_setup_reference.check_if_directory_is_valid()

    @property
    def widget_mainwindow(self):
        return self.__widget_mainwindow