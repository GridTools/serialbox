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

        # Widgets
        self.__widget_fieldmetainfo = widget_fieldmetainfo

        # Signals
        self.clicked.connect(self.update_field_metainfo)
        self.selectionModel().selectionChanged.connect(self.update_field_metainfo)

    def update_field_metainfo(self, model_idx):
        if isinstance(model_idx, QItemSelection):
            model_idx = model_idx.indexes()[0]
        self.__widget_fieldmetainfo.set_field(self.__stencil_data.serializer, model_idx.data())

    def remove_all_items(self):
        Logger.info(
            "Removing all items of StencilFieldListWidget of '%s'" % self.__stencil_data.name)
        self.__model.clear()

    def add_item(self, item):
        Logger.info(
            "Adding item '%s' to StencilFieldListWidget of '%s'" % (item, self.__stencil_data.name))

        item_to_add = QStandardItem(item)
        item_to_add.setCheckable(True)
        item_to_add.setEditable(False)
        item_to_add.setCheckState(Qt.Checked)
        item_to_add.checkState()
        self.__model.appendRow(item_to_add)

    def remove_item(self, item):
        Logger.info(
            "Removing item '%s' of StencilFieldListWidget of '%s'" % (
                item, self.__stencil_data.name))

        idx = self.__model.findItems(item)
        self.__model.removeRow(idx)

    @property
    def fields(self):
        fields = []
        for idx in self.__model.rowCount():
            field_item = self.__model.item(idx, 0)
            if field_item.checkState() == Qt.Checked:
                fields += [field_item.data()]
