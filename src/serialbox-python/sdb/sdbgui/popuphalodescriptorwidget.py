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

from PyQt5.QtCore import Qt
from PyQt5.QtGui import QStandardItemModel, QStandardItem
from PyQt5.QtWidgets import (QLabel, QVBoxLayout, QHBoxLayout, QHeaderView, QSizePolicy, QTableView,
                             QPushButton)

from sdbcore.logger import Logger
from sdbgui.icon import Icon
from sdbgui.popupwidget import PopupWidget


class PopupHaloDescriptorWidget(PopupWidget):
    def __init__(self, parent):
        super().__init__(parent, 0.25, 0.5)

        self.setWindowTitle("Halo descriptor")

        self.__widget_label_title = QLabel("Set the halos for each dimension.", parent=self)

        self.__halo_model = QStandardItemModel(0, 2)
        self.__halo_model.setHorizontalHeaderLabels(["Minus", "Plus"])
        self.__halo_model.horizontalHeaderItem(0).setToolTip("Negative halo extent")
        self.__halo_model.horizontalHeaderItem(1).setToolTip("Positive halo extent")

        self.__widget_table = QTableView(self)
        self.__widget_table.setModel(self.__halo_model)
        self.__widget_table.setStyleSheet(
            '''
                QTableWidget::item:selected:active {
                    background: transparent;
                    border-width: 0px;
                }
            ''')

        self.__widget_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)

        # Assume 3 dimension by default
        for i in range(3):
            self.add_row()

        self.__widget_button_add = QPushButton(self)
        self.__widget_button_add.setIcon(Icon("edit_add.png"))
        self.__widget_button_add.clicked.connect(self.add_row)
        self.__widget_button_add.setToolTip("Add a dimension")

        self.__widget_button_remove = QPushButton(self)
        self.__widget_button_remove.setIcon(Icon("edit_remove.png"))
        self.__widget_button_remove.clicked.connect(self.remove_row)
        self.__widget_button_remove.setToolTip("Remove last dimension")

        self.__widget_button_ok = QPushButton("Done", parent=self)
        self.__widget_button_ok.clicked.connect(self.done)

        hbox_bottom = QHBoxLayout()
        hbox_bottom.addWidget(self.__widget_button_add)
        hbox_bottom.addWidget(self.__widget_button_remove)
        hbox_bottom.addStretch(1)
        hbox_bottom.addWidget(self.__widget_button_ok)

        vbox = QVBoxLayout()
        vbox.addWidget(self.__widget_label_title)
        vbox.addWidget(self.__widget_table)
        vbox.addLayout(hbox_bottom)

        self.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Fixed)
        self.setLayout(vbox)
        self.show()

    def add_row(self):
        Logger.info("Adding row")

        minus = QStandardItem()
        minus.setTextAlignment(Qt.AlignCenter)
        minus.setText("0")

        plus = QStandardItem()
        plus.setTextAlignment(Qt.AlignCenter)
        plus.setText("0")

        self.__halo_model.appendRow([minus, plus])

    def remove_row(self):
        Logger.info("Removing row")
        self.__halo_model.removeRow(self.__halo_model.rowCount() - 1)

    def done(self):
        self.close()
