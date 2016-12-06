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

from PyQt5.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout, QPushButton

from sdbcore.logger import Logger
from sdbgui.icon import Icon
from sdbgui.popuphalodescriptorwidget import PopupHaloDescriptorWidget
from sdbgui.stencilfieldmetainfowidget import StencilFieldMetainfoWidget
from sdbgui.stencilthresholdsetterwidget import StencilThresholdSetterWidget
from sdbgui.stencilwidget import StencilWidget
from sdbgui.tabstate import TabState
from sdbgui.tabwindow import TabWindow


class StencilWindow(QWidget, TabWindow):
    def __init__(self, mainwindow, stencil_field_mapper, input_stencil_data,
                 reference_stencil_data):
        super().__init__()

        # Data
        self.__input_stencil_data = input_stencil_data
        self.__reference_stencil_data = reference_stencil_data
        self.__stencil_field_mapper = stencil_field_mapper

        # Widget
        self.__widget_mainwindow = mainwindow

        self.__widget_fieldmetainfo = StencilFieldMetainfoWidget(self)
        self.__widget_threshold_setter = StencilThresholdSetterWidget(self,
                                                                      self.__stencil_field_mapper)

        self.__widget_stencil_input = StencilWidget(self, self.__input_stencil_data,
                                                    self.__widget_fieldmetainfo)
        self.__widget_stencil_reference = StencilWidget(self, self.__reference_stencil_data,
                                                        self.__widget_fieldmetainfo)

        self.__widget_button_halo = QPushButton("Halo", parent=self)
        self.__widget_button_halo.setIcon(Icon("show_all.png"))
        self.__widget_button_halo.setStatusTip(
            "Set halo size of the fields. The indentation of the halos is skipped during"
            " comparison of the fields.")
        self.__widget_button_halo.clicked.connect(self.popup_halo_descriptor)

        self.__widget_button_next = QPushButton("Next")
        self.__widget_button_next.clicked.connect(self.make_continue)
        self.__widget_button_next.setIcon(Icon("run.png"))
        self.__widget_button_next.setStatusTip(
            "Compare selected fields of the Input and Refrence stencil")

        hbox_widgets = QHBoxLayout()
        hbox_widgets.addWidget(self.__widget_stencil_input)
        hbox_widgets.addWidget(self.__widget_stencil_reference)

        hbox_button = QHBoxLayout()
        hbox_button.addStretch(1)
        hbox_button.addWidget(self.__widget_button_next)

        hbox_middle = QHBoxLayout()
        hbox_middle.addWidget(self.__widget_fieldmetainfo)
        hbox_middle.addStretch(1)
        hbox_middle.addWidget(self.__widget_button_halo)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox_widgets)
        vbox.addStretch(1)
        vbox.addLayout(hbox_middle)
        vbox.addWidget(self.__widget_threshold_setter)
        vbox.addLayout(hbox_button)
        self.setLayout(vbox)

    def initial_field_match(self):
        self.__stencil_field_mapper.initial_field_match()

    def update_comparison_result(self):
        try:
            self.__stencil_field_mapper.compare_fields(self.__widget_stencil_input.fields,
                                                       self.__widget_stencil_reference.fields)
        except RuntimeError as e:
            self.__widget_mainwindow.popup_error_box(str(e))
            return False
        return True

    def popup_halo_descriptor(self):
        self.__widget_popup_halo_descriptor = PopupHaloDescriptorWidget(self.__widget_mainwindow)

    def make_continue(self):
        if self.update_comparison_result():
            self.__widget_mainwindow.set_tab_highest_valid_state(TabState.Result)
            self.__widget_mainwindow.switch_to_tab(TabState.Result)

    def make_back(self):
        self.__widget_mainwindow.switch_to_tab(TabState.Setup)

    def make_update(self):
        Logger.info("Updating Stencil tab")
        self.__widget_stencil_input.make_update()
        self.__widget_stencil_reference.make_update()

    @property
    def widget_mainwindow(self):
        return self.__widget_mainwindow
