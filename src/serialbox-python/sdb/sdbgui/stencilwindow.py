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
from .stencilfieldmetainfowidget import StencilFieldMetainfoWidget
from .stencilwidget import StencilWidget
from .tabstate import TabState


class StencilWindow(QWidget):
    def __init__(self, mainwindow, input_stencil_data, reference_stencil_data):
        super().__init__()

        # Data
        self.__input_stencil_data = input_stencil_data
        self.__reference_stencil_data = reference_stencil_data

        # Widget
        self.__widget_mainwindow = mainwindow

        self.__widget_fieldmetainfo = StencilFieldMetainfoWidget()

        self.__widget_stencil_input = StencilWidget(self.__input_stencil_data,
                                                    self.__widget_fieldmetainfo)
        self.__widget_stencil_reference = StencilWidget(self.__reference_stencil_data,
                                                        self.__widget_fieldmetainfo)

        self.__widget_button_continue = QPushButton("Continue")
        self.__widget_button_continue.clicked.connect(self.make_continue)

        self.__widget_button_back = QPushButton("Back")
        self.__widget_button_back.clicked.connect(self.make_back)

        hbox_widgets = QHBoxLayout()
        hbox_widgets.addWidget(self.__widget_stencil_input)
        hbox_widgets.addWidget(self.__widget_stencil_reference)

        hbox_button = QHBoxLayout()
        hbox_button.addStretch(1)
        hbox_button.addWidget(self.__widget_button_back)
        hbox_button.addWidget(self.__widget_button_continue)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox_widgets)
        vbox.addStretch(1)
        vbox.addWidget(self.__widget_fieldmetainfo)
        vbox.addLayout(hbox_button)

        self.setLayout(vbox)

    def make_continue(self):
        print("continue")

    def make_back(self):
        self.__widget_mainwindow.switch_to_tab(TabState.Setup)

    def make_update(self):
        Logger.info("Updating Stencil tab")
        self.__widget_stencil_input.make_update()
        self.__widget_stencil_reference.make_update()
