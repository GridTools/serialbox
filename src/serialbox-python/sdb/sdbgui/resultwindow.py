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

from PyQt5.QtWidgets import QWidget

from .tabstate import TabState


class ResultWindow(QWidget):
    def __init__(self, mainwindow, stencil_field_mapper):
        super().__init__()

        self.__stencil_field_mapper = stencil_field_mapper
        self.__widget_mainwindow = mainwindow

    def make_continue(self):
        pass

    def make_back(self):
        self.__widget_mainwindow.switch_to_tab(TabState.Stencil)

    def make_update(self):
        pass
