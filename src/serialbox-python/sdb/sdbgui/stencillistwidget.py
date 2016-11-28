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
from sdbcore.stencildatalistener import StencilDataStencilListListener


class StencilListWidget(QComboBox, StencilDataStencilListListener):
    def __init__(self, parent, stencil_data):
        super().__init__(parent)
        self.__stencil_data = stencil_data
        self.__stencil_data.register_as_stencil_list_listener(self)

    def remove_all_stencils(self):
        Logger.info(
            "Removing all stencils of StencilListWidget of '%s'" % self.__stencil_data.name)
        self.clear()

    def add_stencil(self, stencil):
        Logger.info(
            "Adding stencil '%s' to StencilFieldListWidget of '%s'" % (
            stencil, self.__stencil_data.name))
        self.addItem(stencil)

    def remove_stencil(self, stencil):
        Logger.info(
            "Removing stencil '%s' of StencilFieldListWidget of '%s'" % (
                stencil, self.__stencil_data.name))
        self.removeItem(self.findText(stencil))

    @property
    def stencil(self):
        return self.itemText(self.currentIndex())
