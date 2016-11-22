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

from PyQt5.QtCore import Qt, QItemSelection
from PyQt5.QtGui import QStandardItem, QStandardItemModel
from PyQt5.QtWidgets import QListView

from sdbcore.logger import Logger


class StencilFieldListWidget(QListView):
    def __init__(self, stencil_data, widget_fieldmetainfo):
        super().__init__()

        # Data
        self.__stencil_data = stencil_data
        self.__stencil_data.register_as_field_list_listener(self)

        self.__model = QStandardItemModel()
        self.setModel(self.__model)

        self.setDragEnabled(True)
        self.setDefaultDropAction(Qt.MoveAction)
        self.setDragDropOverwriteMode(False)
        self.setDragDropMode(self.InternalMove)

        # Widgets
        self.__widget_fieldmetainfo = widget_fieldmetainfo

        # Signals
        self.clicked.connect(self.update_field_metainfo)
        self.selectionModel().selectionChanged.connect(self.update_field_metainfo)

    def update_field_metainfo(self, model_idx):
        if isinstance(model_idx, QItemSelection):
            model_idx = model_idx.indexes()[0]
        self.__widget_fieldmetainfo.set_field(self.__stencil_data.serializer, model_idx.data())

    @classmethod
    def create_item(self, data):
        item = QStandardItem(data)
        item.setData(data)
        item.setEditable(False)

        # Don't allow overwrite of items
        item.setFlags(item.flags() & ~Qt.ItemIsDropEnabled)

        item.setCheckable(True)
        item.setCheckState(Qt.Checked)
        item.checkState()
        return item

    def remove_all_items(self):
        Logger.info(
            "Removing all items of StencilFieldListWidget of '%s'" % self.__stencil_data.name)
        self.__model.clear()

    def add_item(self, data, idx=None):
        Logger.info(
            "Adding data '%s' to StencilFieldListWidget of '%s'" % (data, self.__stencil_data.name))

        item = self.create_item(data)
        if idx:
            self.__model.insertRow(idx, item)
        else:
            self.__model.appendRow(item)

    def remove_item(self, item):
        Logger.info(
            "Removing item '%s' of StencilFieldListWidget of '%s'" % (
                item, self.__stencil_data.name))

        idx = self.__model.findItems(item)
        self.__model.removeRow(idx)

    def set_enable_item(self, idx, enable):
        item = self.__model.item(idx)
        Logger.info("Setting enable status of item '%s' of StencilFieldListWidget of '%s' to %s" % (
            item.data(), self.__stencil_data.name, enable))

        if enable:
            item.setCheckState(Qt.Checked)
        else:
            item.setCheckState(Qt.Unchecked)

    def move_item(self, from_idx, to_idx):
        data = self.__model.item(from_idx, 0).data()
        Logger.info(
            "Moving item '%s' from '%i' to '%i' of StencilFieldListWidget of '%s'" % (
                data, from_idx, to_idx, self.__stencil_data.name))

        self.__model.removeRow(from_idx)
        item = self.create_item(data)
        self.__model.insertRow(to_idx, item)

    def num_items(self):
        return self.__model.rowCount()

    @property
    def fields(self):
        fields = []
        for idx in self.__model.rowCount():
            field_item = self.__model.item(idx, 0)
            if field_item.checkState() == Qt.Checked:
                fields += [field_item.data()]
