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
from PyQt5.QtGui import QStandardItem, QStandardItemModel, QCursor
from PyQt5.QtWidgets import QListView, QMenu, QAction, QInputDialog, QLineEdit

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

    def contextMenuEvent(self, event):
        if self.selectionModel().selection().indexes():
            for i in self.selectionModel().selection().indexes():
                self.row = i.row()

            self.menu = QMenu(self)
            renameAction = QAction('Rename', self)
            renameAction.triggered.connect(self.rename_entry)
            self.menu.addAction(renameAction)
            self.menu.popup(QCursor.pos())

    def rename_entry(self):
        item = self.__model.item(self.row, 0)
        name, ok = QInputDialog.getText(self, "Rename", "Renaming %s" % item.text(),
                                        QLineEdit.Normal, item.text())
        if ok:
            item.setText(name)

    def update_field_metainfo(self, model_idx):
        if isinstance(model_idx, QItemSelection):
            model_idx = model_idx.indexes()[0]

        self.__widget_fieldmetainfo.set_field(self.__stencil_data.serializer,
                                              self.__model.item(model_idx.row(), 0).data())

    @classmethod
    def create_item(self, name, data=None):
        item = QStandardItem(name)
        item.setData(data if data is not None else name)
        item.setEditable(False)

        # Don't allow overwrite of items
        item.setFlags(item.flags() & ~Qt.ItemIsDropEnabled)

        item.setCheckable(True)
        item.setCheckState(Qt.Checked)
        return item

    def remove_all_items(self):
        Logger.info(
            "Removing all items of StencilFieldListWidget of '%s'" % self.__stencil_data.name)
        self.__model.clear()

    def add_item(self, name, idx=None):
        Logger.info(
            "Adding item with name '%s' to StencilFieldListWidget of '%s'" % (
                name, self.__stencil_data.name))

        item = self.create_item(name)
        if idx:
            self.__model.insertRow(idx, item)
        else:
            self.__model.appendRow(item)

    def remove_item(self, name):
        item = self.__model.findItems(name)
        Logger.info(
            "Removing item with name '%s' of StencilFieldListWidget of '%s'" % (
                item.text(), self.__stencil_data.name))

        self.__model.removeRow(item)

    def set_enable_item(self, idx, enable):
        if isinstance(idx, str):
            item = self.__model.findItems(idx)[0]
        else:
            item = self.__model.item(idx)

        Logger.info(
            "Setting enable status of item with name '%s' of StencilFieldListWidget of '%s' to %s" % (
                item.text(), self.__stencil_data.name, enable))

        if enable:
            item.setCheckState(Qt.Checked)
        else:
            item.setCheckState(Qt.Unchecked)

    def move_item(self, field, idx):
        item = self.__model.findItems(field)[0]
        name = item.text()
        data = item.data()
        check_state = item.checkState()
        from_idx = self.__model.indexFromItem(item).row()

        Logger.info(
            "Moving item with name '%s' to index '%i' of StencilFieldListWidget of '%s'" % (
                name, idx, self.__stencil_data.name))

        self.__model.removeRow(from_idx)
        item = self.create_item(name, data)
        item.setCheckState(check_state)
        self.__model.insertRow(idx, item)

    def num_items(self):
        return self.__model.rowCount()

    @property
    def fields(self):
        fields = []
        for idx in range(self.__model.rowCount()):
            field_item = self.__model.item(idx, 0)
            if field_item.checkState() == Qt.Checked:
                fields += [field_item.data()]
        return fields
