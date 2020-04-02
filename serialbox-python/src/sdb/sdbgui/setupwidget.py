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

from os import getcwd, listdir, path
from sys import platform as sys_platform

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import (QWidget, QGridLayout, QLabel, QPushButton, QFileDialog,
                             QComboBox)

from sdbcore.logger import Logger
from sdbcore.serializerdatalistener import SerializerDataDirectoryAndPrefixListener
from sdbgui.droppablelineeditwidget import DroppableLineEditWidget
from sdbgui.icon import Icon
from sdbgui.pixmap import Pixmap
from sdbgui.tabstate import TabState


class SetupWidget(QWidget, SerializerDataDirectoryAndPrefixListener):
    def __init__(self, setupwindow, serializer_data):
        super().__init__()

        # Data
        self.__serializer_data = serializer_data
        self.__name = serializer_data.name

        # Register as SerializerData listener
        self.__serializer_data.register_as_serializer_data_directory_and_prefix_listener(self)

        #  Widgets
        self.__widget_setupwindow = setupwindow

        self.__widget_label_name = QLabel("<b>%s</b>" % self.__name, parent=self)

        self.__widget_label_status_icon = QLabel(self)

        self.__widget_label_directory_name = QLabel('Directory', parent=self)
        self.__widget_label_directory_name.setToolTip("Directory of the %s" % self.__name)
        self.__widget_label_directory_name.setStatusTip("Directory of the %s" % self.__name)

        self.__widget_edit_directory = DroppableLineEditWidget(self.__serializer_data.directory,
                                                               parent=self)
        self.__widget_edit_directory.setClearButtonEnabled(True)
        self.__widget_edit_directory.setDragEnabled(True)
        self.__widget_edit_directory.textChanged[str].connect(self.widget_edit_directory_changed)

        self.__widget_button_directory_file_dialog = QPushButton(self)
        self.__widget_button_directory_file_dialog.setIcon(Icon("fileopen.png"))
        self.__widget_button_directory_file_dialog.setToolTip("Set Serializer directory")
        self.__widget_button_directory_file_dialog.clicked.connect(self.open_file_dialog)

        self.__widget_label_prefix_name = QLabel('Prefix', parent=self)
        self.__widget_label_prefix_name.setToolTip("Prefix of the %s" % self.__name)
        self.__widget_label_prefix_name.setStatusTip("Prefix of the %s" % self.__name)

        self.__widget_edit_prefix = QComboBox(self)
        self.__widget_edit_prefix.setEditable(True)
        self.__widget_edit_prefix.setEditText(self.__serializer_data.prefix)
        self.__widget_edit_prefix.editTextChanged[str].connect(self.widget_edit_prefix_changed)

        # Layouting
        grid_layout = QGridLayout()
        grid_layout.setSpacing(5)

        grid_layout.addWidget(self.__widget_label_name, 1, 0)
        grid_layout.addWidget(self.__widget_label_status_icon, 1, 1)

        grid_layout.addWidget(self.__widget_label_directory_name, 2, 0)
        grid_layout.addWidget(self.__widget_edit_directory, 2, 1)
        grid_layout.addWidget(self.__widget_button_directory_file_dialog, 2, 2)

        grid_layout.addWidget(self.__widget_label_prefix_name, 3, 0)
        grid_layout.addWidget(self.__widget_edit_prefix, 3, 1)

        self.setLayout(grid_layout)
        self.check_if_directory_is_valid()
        self.check_if_prefix_is_valid()

    def prefix_changed(self, prefix):
        self.__widget_edit_prefix.setEditText(prefix)

    def directory_changed(self, directory):
        self.__widget_edit_directory.setText(directory)

    @property
    def directory(self):
        return self.__serializer_data.directory

    @property
    def prefix(self):
        return self.__serializer_data.prefix

    def check_if_directory_is_valid(self):
        """Check if the directory exists and fill the self.__prefix_edit with suggestions for the
        prefix.
        """
        is_valid = path.isdir(self.directory)

        if is_valid:
            files = [f for f in listdir(self.directory) if
                     path.isfile(path.join(self.directory, f))]

            # If the prefix is empty or changed (i.e not the same as currently displayed), we
            # check and edit the text of the widget
            if self.prefix == "" or self.prefix != self.__widget_edit_prefix.currentText():
                is_valid = False
                for f in files:
                    if path.splitext(f)[1] == ".json" and f.startswith("MetaData-"):
                        self.__widget_edit_prefix.addItem(
                            path.splitext(f)[0].replace("MetaData-", ""))
                        is_valid = True
            # In case the prefix has not changed and is not empty, we just check
            else:
                self.check_if_prefix_is_valid()
                return

        if is_valid:
            self.show_valid_icon()
        else:
            self.show_invalid_icon()

    def check_if_prefix_is_valid(self):
        dir_is_valid = path.isdir(self.directory)
        if dir_is_valid:
            files = [f for f in listdir(self.directory) if
                     path.isfile(path.join(self.directory, f))]

            if "MetaData-%s.json" % self.prefix in files:
                self.show_valid_icon()
                return

        self.show_invalid_icon()

    def open_file_dialog(self):
        Logger.info("Open file dialog")

        open_dir = self.directory
        if not self.directory:
            open_dir = getcwd()

        dialog = QFileDialog(self, 'Open %s' % self.__name)
        dialog.setFileMode(QFileDialog.Directory)
        dialog.setViewMode(QFileDialog.Detail)
        dialog.setDirectory(open_dir)
        if dialog.exec_():
            self.__widget_edit_directory.setText(dialog.selectedFiles()[0])

    def widget_edit_directory_changed(self, dir):
        self.__serializer_data.directory = dir
        self.check_if_directory_is_valid()

        Logger.info("Setting directory of %s to: %s" % (self.__name, self.directory))

    def widget_edit_prefix_changed(self, prefix):
        self.__serializer_data.prefix = prefix
        self.check_if_prefix_is_valid()

        Logger.info("Setting prefix of %s to: %s" % (self.__name, self.prefix))

    def show_invalid_icon(self):
        self.__widget_label_status_icon.setPixmap(Pixmap("error.png"))
        self.__widget_label_status_icon.setStatusTip("Directory does not contain a Serializer")

        # It's the safest way to just invalidate all other tabs
        self.__widget_setupwindow.widget_mainwindow.set_tab_highest_valid_state(TabState.Setup)

    def show_valid_icon(self):
        image = Pixmap("success.png")

        if sys_platform == 'win32':
            image = image.scaled(12, 12, Qt.KeepAspectRatio)

        self.__widget_label_status_icon.setPixmap(image)
        self.__widget_label_status_icon.setStatusTip("")
