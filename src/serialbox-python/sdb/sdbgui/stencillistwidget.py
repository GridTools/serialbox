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

from PyQt5.QtWidgets import QComboBox

from sdbcore.logger import Logger


class StencilListWidget(QComboBox):
    def __init__(self, stencil_data):
        super().__init__()
        self.__stencil_data = stencil_data
        self.__stencil_data.register_as_stencil_list_listener(self)

    def remove_all_items(self):
        Logger.info(
            "Removing all items of StencilListWidget of '%s'" % self.__stencil_data.name)
        self.clear()

    def add_item(self, item):
        Logger.info(
            "Adding item '%s' to StencilFieldListWidget of '%s'" % (item, self.__stencil_data.name))
        self.addItem(item)

    def remove_item(self, item):
        Logger.info(
            "Removing item '%s' of StencilFieldListWidget of '%s'" % (
                item, self.__stencil_data.name))
        self.removeItem(self.findText(item))

    @property
    def stencil(self):
        return self.itemText(self.currentIndex())
