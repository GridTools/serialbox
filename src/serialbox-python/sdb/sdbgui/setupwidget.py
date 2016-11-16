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

from PyQt5.QtGui import QIcon, QPixmap
from PyQt5.QtWidgets import (QWidget, QGridLayout, QLabel, QLineEdit, QPushButton, QFileDialog,
                             QComboBox)

from sdbcore import Logger


class SetupWidget(QWidget):
    def __init__(self, serializer_data):
        super().__init__()

        self.__serializer_data = serializer_data

        self.__name = serializer_data.name
        Logger.info("Initialize setup widget: %s" % self.__name)

        # Create Widgets
        self.__name_label = QLabel("<b>%s</b>" % self.__name)
        self.__name_icon = QLabel()

        self.__directory_label = QLabel('Directory')
        self.__directory_label.setToolTip("Directory of the %s" % self.__name)

        self.__directory_edit = QLineEdit(self.__serializer_data.directory)
        self.__directory_edit.setDragEnabled(True)
        self.__directory_edit.textChanged[str].connect(self.__directory_edit_changed)

        self.__directory_file_dialog = QPushButton()
        self.__directory_file_dialog.setIcon(QIcon("sdbgui/images/fileopen.png"))
        self.__directory_file_dialog.setToolTip("Set Serializer directory")
        self.__directory_file_dialog.clicked.connect(self.__open_file_dialog)

        self.__prefix_label = QLabel('Prefix')
        self.__prefix_label.setToolTip("Prefix of the %s" % self.__name)

        self.__prefix_edit = QComboBox()
        self.__prefix_edit.setEditText(self.__serializer_data.prefix)
        self.__prefix_edit.setEditable(True)
        self.__prefix_edit.editTextChanged[str].connect(self.__prefix_edit_changed)

        # Layouting
        grid_layout = QGridLayout()
        grid_layout.setSpacing(5)

        grid_layout.addWidget(self.__name_label, 1, 0)
        grid_layout.addWidget(self.__name_icon, 1, 1)

        grid_layout.addWidget(self.__directory_label, 2, 0)
        grid_layout.addWidget(self.__directory_edit, 2, 1)
        grid_layout.addWidget(self.__directory_file_dialog, 2, 2)

        grid_layout.addWidget(self.__prefix_label, 3, 0)
        grid_layout.addWidget(self.__prefix_edit, 3, 1)

        self.setLayout(grid_layout)
        self.__check_if_directory_is_valid()
        self.__check_if_prefix_is_valid()

    @property
    def directory(self):
        return self.__serializer_data.directory

    @property
    def prefix(self):
        return self.__serializer_data.prefix

    def __check_if_directory_is_valid(self):
        """Check if the directory exists and fill the self.__prefix_edit with suggestions for the
        prefix.
        """
        dir_is_valid = path.exists(self.directory)

        if dir_is_valid:
            files = [f for f in listdir(self.directory) if
                     path.isfile(path.join(self.directory, f))]

            self.__prefix_edit.clear()

            dir_is_valid = False
            for f in files:
                if path.splitext(f)[1] == ".json" and f.startswith("MetaData-"):
                    self.__prefix_edit.addItem(path.splitext(f)[0].replace("MetaData-", ""))
                    dir_is_valid = True

        if dir_is_valid:
            self.__show_valid_icon()
        else:
            self.__show_invalid_icon()

    def __check_if_prefix_is_valid(self):
        dir_is_valid = path.exists(self.directory)
        if dir_is_valid:
            files = [f for f in listdir(self.directory) if
                     path.isfile(path.join(self.directory, f))]

            if "MetaData-%s.json" % self.prefix in files:
                self.__show_valid_icon()
                return

        self.__show_invalid_icon()

    def __open_file_dialog(self):
        Logger.info("Open file dialog")

        open_dir = self.directory
        if self.directory:
            open_dir = getcwd()

        fname = QFileDialog.getExistingDirectory(self, 'Open %s' % self.__name, open_dir)
        self.__directory_edit.setText(fname)

    def __directory_edit_changed(self, dir):
        self.__serializer_data.directory = dir
        self.__check_if_directory_is_valid()

        Logger.info("Setting directory of %s to: %s" % (self.__name, self.directory))

    def __prefix_edit_changed(self, prefix):
        self.__serializer_data.prefix = prefix
        self.__check_if_prefix_is_valid()

        Logger.info("Setting prefix of %s to: %s" % (self.__name, self.prefix))

    def __show_invalid_icon(self):
        self.__name_icon.setPixmap(QPixmap("sdbgui/images/error.png"))
        self.__name_icon.setStatusTip("Directory does not contain a Serializer")

    def __show_valid_icon(self):
        self.__name_icon.setPixmap(QPixmap("sdbgui/images/success.png"))
        self.__name_icon.setStatusTip("")
