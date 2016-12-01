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

from PyQt5.QtCore import Qt, QSize
from PyQt5.QtGui import QIcon
from PyQt5.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout, QPushButton, QLabel, QListWidget

from sdbcore.logger import Logger
from .setupwidget import SetupWidget
from .tabstate import TabState
from .tabwindow import TabWindow


class SmallListWidget(QListWidget):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def sizeHint(self):
        s = QSize()
        s.setHeight(super().sizeHint().height())
        s.setWidth(self.sizeHintForColumn(0))
        return s

class SetupWindow(QWidget, TabWindow):
    def __init__(self, mainwindow, input_serializer_data, reference_serializer_data):
        super().__init__()

        # Data
        self.__widget_mainwindow = mainwindow
        self.__input_serializer_data = input_serializer_data
        self.__reference_serializer_data = reference_serializer_data

        # Widgets
        self.__widget_setup_input = SetupWidget(self, self.__input_serializer_data)
        self.__widget_setup_reference = SetupWidget(self, self.__reference_serializer_data)

        self.__widget_button_next = QPushButton("Next", parent=self)
        self.__widget_button_next.clicked.connect(self.make_continue)
        self.__widget_button_next.setIcon(QIcon("sdbgui/images/run.png"))
        self.__widget_button_next.setStatusTip("Load Input and Refrence Serializers")

        self.__widget_label_recently_used = QLabel("Recently used Serializers: ", parent=self)
        self.__widget_list_recently_used = QListWidget(self)

        paths = mainwindow.session_manager.get_recently_used_serializer_paths()
        if paths:

            list = self.__widget_list_recently_used
            for path in mainwindow.session_manager.get_recently_used_serializer_paths():
                list.addItem(path)

            list.setDragEnabled(True)
            list.setDragDropMode(QListWidget.DragOnly)
            list.setFixedHeight(list.sizeHintForRow(0) * 5 + 2 * list.frameWidth())
        else:
            self.__widget_list_recently_used.setEnabled(False)
            self.__widget_label_recently_used.setEnabled(False)

        hbox = QHBoxLayout()
        hbox.addWidget(self.__widget_setup_input)
        hbox.addWidget(self.__widget_setup_reference)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        vbox.addStretch(1)
        vbox.addWidget(self.__widget_label_recently_used)
        vbox.addWidget(self.__widget_list_recently_used)

        hbox_button = QHBoxLayout()
        hbox_button.addStretch(1)
        hbox_button.addWidget(self.__widget_button_next)

        vbox.addLayout(hbox_button)

        self.setLayout(vbox)

    def make_continue(self):
        try:
            Logger.info("Setup Serializers")
            self.__input_serializer_data.make_serializer()
            self.__reference_serializer_data.make_serializer()
        except RuntimeError as e:
            self.__widget_mainwindow.popup_error_box("%s" % e)

        if self.__input_serializer_data.is_valid() and self.__reference_serializer_data.is_valid():

            # Register directories in the session manager
            self.__widget_mainwindow.session_manager.update_recently_used_serializer_paths(
                self.__input_serializer_data.serializer.directory)
            self.__widget_mainwindow.session_manager.update_recently_used_serializer_paths(
                self.__reference_serializer_data.serializer.directory)

            # Switch to the next tab
            self.__widget_mainwindow.set_tab_highest_valid_state(TabState.Stencil)
            self.__widget_mainwindow.switch_to_tab(TabState.Stencil)
        else:
            self.__widget_mainwindow.set_tab_highest_valid_state(TabState.Setup)

    def make_back(self):
        pass

    def make_update(self):
        Logger.info("Updating Serializer tab")
        self.__widget_setup_input.check_if_directory_is_valid()
        self.__widget_setup_reference.check_if_directory_is_valid()

    @property
    def widget_mainwindow(self):
        return self.__widget_mainwindow
