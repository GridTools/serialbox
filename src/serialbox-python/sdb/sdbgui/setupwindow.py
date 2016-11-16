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
    def __init__(self, parent, input_serializer_data, reference_serializer_data):
        super().__init__()

        # Data
        self.__parent = parent
        self.__input_serializer_data = input_serializer_data
        self.__reference_serializer_data = reference_serializer_data

        # Widgets
        self.__input_widget = SetupWidget(self.__input_serializer_data)
        self.__reference_widget = SetupWidget(self.__reference_serializer_data)

        # Buttons
        self.__continue_button = QPushButton("Continue")
        self.__continue_button.clicked.connect(self.make_continue)

        self.__back_button = QPushButton("Back")
        self.__back_button.setEnabled(False)

        self.__explain_wdiget = QLabel("Explain the process here!")

        hbox = QHBoxLayout()
        hbox.addWidget(self.__input_widget)
        hbox.addWidget(self.__reference_widget)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        vbox.addStretch(1)
        vbox.addWidget(self.__explain_wdiget)
        vbox.addStretch(1)

        hbox_button = QHBoxLayout()
        hbox_button.addStretch(1)
        hbox_button.addWidget(self.__back_button)
        hbox_button.addWidget(self.__continue_button)

        vbox.addLayout(hbox_button)

        self.setLayout(vbox)

    def make_continue(self):
        Logger.info("Initializing serializers")
        try:
            self.__input_serializer_data.make_serializer()
            self.__reference_serializer_data.make_serializer()
        except RuntimeError as e:
            ErrorMessageBox(self, "%s" % e)

        if self.__input_serializer_data.is_valid() and self.__reference_serializer_data.is_valid():
            self.__parent.set_valid_tab_state(TabState.Stencil)
            self.__parent.switch_to_tab(TabState.Stencil)
        else:
            self.__parent.set_valid_tab_state(TabState.Setup)

    def make_back(self):
        pass

    def make_update(self):
        pass