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

from PyQt5.QtCore import QItemSelection
from PyQt5.QtGui import QStandardItemModel, QStandardItem, QIcon
from PyQt5.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QLabel, QListView
from numpy import any

from sdbcore.logger import Logger
from .errorvisualizematplotlibwidget import ErrorVisualizeMatplotlibWidget, SDB_HAS_MATPLOTLIB


class ErrorVisualizeWidget(QWidget):
    def __init__(self, parent, mainwindow):
        super().__init__(parent)

        self.__widget_mainwindow = mainwindow
        self.__widget_matplotlib = ErrorVisualizeMatplotlibWidget(self)
        self.__current_layer = 0

        self.__widget_label_wrong_dimension = QLabel("", parent=self)

        self.__widget_label_layer = QLabel("<b>Layer</b>: ", parent=self)
        self.__widget_list_layer = QListView(self)
        self.__widget_list_layer.setModel(QStandardItemModel())
        self.__widget_list_layer.clicked.connect(self.set_layer)
        self.__widget_list_layer.selectionModel().selectionChanged.connect(self.set_layer)

        if not SDB_HAS_MATPLOTLIB:
            self.__widget_label_layer.setEnabled(False)
            self.__widget_list_layer.setEnabled(False)

        hbox_top = QHBoxLayout()
        hbox_top.addWidget(self.__widget_matplotlib)

        vbox_top = QVBoxLayout()
        vbox_top.addWidget(self.__widget_label_layer)
        vbox_top.addWidget(self.__widget_list_layer)
        hbox_top.addLayout(vbox_top)

        hbox_bottom = QHBoxLayout()
        hbox_bottom.addWidget(self.__widget_label_wrong_dimension)
        hbox_bottom.addStretch(1)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox_top)
        vbox.addLayout(hbox_bottom)
        self.setLayout(vbox)

    def make_update(self, comparison_result):
        Logger.info("Updating ErrorVisualizeWidget")

        if not SDB_HAS_MATPLOTLIB:
            return

        self.__error_positions = comparison_result.get_error_positions()

        if self.__error_positions.ndim in (2, 3):
            self.__current_layer = 0
            self.__widget_matplotlib.draw_layer(self.__error_positions, self.__current_layer)

            self.__widget_list_layer.model().clear()
            if self.__error_positions.ndim == 3:
                self.__widget_label_layer.setEnabled(True)
                self.__widget_list_layer.setEnabled(True)

                for k in range(self.__error_positions.shape[2]):
                    item = QStandardItem("  %i" % k)

                    if any(self.__error_positions[:, :, k]):
                        item.setIcon(QIcon("sdbgui/images/failure-small.png"))
                    else:
                        item.setIcon(QIcon("sdbgui/images/success.png"))

                    item.setEditable(False)
                    self.__widget_list_layer.model().insertRow(k, item)
            else:
                self.__widget_label_layer.setEnabled(False)
                self.__widget_list_layer.setEnabled(False)

            self.__widget_label_wrong_dimension.setText("")
        else:
            self.__widget_matplotlib.draw_nothing()

            self.__widget_label_layer.setEnabled(False)
            self.__widget_list_layer.model().clear()
            self.__widget_list_layer.setEnabled(False)

            self.__widget_label_wrong_dimension.setText(
                "Visualization is only supported for 2 and 3-dimensional fields.")

    def set_layer(self, layer):
        if isinstance(layer, QItemSelection):
            layer = layer.indexes()[0]

        layer_idx = layer.row()

        self.__current_layer = layer_idx
        self.__widget_matplotlib.draw_layer(self.__error_positions, self.__current_layer)
