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

from PyQt5.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout, QPushButton

from sdbcore.logger import Logger
from .stencilwidget import StencilWidget
from .tabstate import TabState

class StencilWindow(QWidget):
    def __init__(self, parent, input_serializer_data, reference_serializer_data):
        super().__init__()

        # Data
        self.__parent = parent
        self.__input_serializer_data = input_serializer_data
        self.__reference_serializer_data = reference_serializer_data

        self.__input_widget = StencilWidget(self.__input_serializer_data)
        self.__reference_widget = StencilWidget(self.__reference_serializer_data)

        # Buttons
        self.__continue_button = QPushButton("Continue")
        self.__continue_button.clicked.connect(self.make_continue)

        self.__back_button = QPushButton("Back")
        self.__back_button.clicked.connect(self.make_back)

        hbox = QHBoxLayout()
        hbox.addWidget(self.__input_widget)
        hbox.addWidget(self.__reference_widget)

        hbox_button = QHBoxLayout()
        hbox_button.addStretch(1)
        hbox_button.addWidget(self.__back_button)
        hbox_button.addWidget(self.__continue_button)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        vbox.addStretch(1)
        vbox.addLayout(hbox_button)

        self.setLayout(vbox)

    def make_continue(self):
        print("continue")

    def make_back(self):
        self.__parent.switch_to_tab(TabState.Setup)

    def make_update(self):
        Logger.info("Updating stencil tab")
        self.__input_widget.make_update()
        self.__reference_widget.make_update()
