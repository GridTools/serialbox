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

from PyQt5.QtGui import QIcon
from PyQt5.QtWidgets import QMainWindow, QDesktopWidget, QAction, QTabWidget

from sdbcore.logger import Logger
from sdbcore.stencildata import StencilData
from sdbcore.stencilfieldmapper import StencilFieldMapper
from sdbcore.version import Version
from .aboutwidget import AboutWidget
from .configuration import Configuration
from .errormessagebox import ErrorMessageBox
from .resultwindow import ResultWindow
from .setupwindow import SetupWindow
from .stencilwindow import StencilWindow
from .tabstate import TabState


class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        Logger.info("Setup main window")

        # Data
        self.__configuration = Configuration()
        self.__configuration.load_from_file()

        self.__input_serializer_data = self.__configuration.make_serializer_data("Input Serializer")
        self.__input_stencil_data = StencilData(self.__input_serializer_data)

        self.__reference_serializer_data = self.__configuration.make_serializer_data(
            "Reference Serializer")
        self.__reference_stencil_data = StencilData(self.__reference_serializer_data)

        self.__stencil_field_mapper = StencilFieldMapper(self.__input_stencil_data,
                                                         self.__reference_stencil_data)

        # Setup GUI
        self.setWindowTitle('sdb - stencil debugger (%s)' % Version().sdb_version())
        self.resize(960, 480)

        self.setWindowIcon(QIcon("sdbgui/images/logo-small.png"))
        self.init_menu_tool_bar()

        # Setup tabs
        self.__tab_highest_valid_state = TabState.Setup
        self.__widget_tab = QTabWidget()
        self.__widget_tab.addTab(
            SetupWindow(self, self.__input_serializer_data, self.__reference_serializer_data),
            "Setup")
        self.__widget_tab.addTab(
            StencilWindow(self, self.__stencil_field_mapper, self.__input_stencil_data,
                          self.__reference_stencil_data),
            "Stencil")
        self.__widget_tab.addTab(
            ResultWindow(self, self.__stencil_field_mapper),
            "Result")

        self.__widget_tab.currentChanged.connect(self.switch_to_tab)

        self.__widget_tab.setTabEnabled(TabState.Setup.value, True)
        self.__widget_tab.setTabEnabled(TabState.Stencil.value, False)
        self.__widget_tab.setTabEnabled(TabState.Result.value, False)

        self.__widget_tab.setTabToolTip(TabState.Setup.value,
                                        "Setup Input and Refrence Serializer")
        self.__widget_tab.setTabToolTip(TabState.Stencil.value,
                                        "Set the stencil to compare and define the mapping of the fields")
        self.__widget_tab.setTabToolTip(TabState.Result.value,
                                        "View to comparison result")

        self.__tab_old_state = TabState.Setup
        self.set_tab_highest_valid_state(TabState.Setup)
        self.switch_to_tab(TabState.Setup)

        self.setCentralWidget(self.__widget_tab)

        Logger.info("Starting main loop")
        self.show()

    def init_menu_tool_bar(self):
        Logger.info("Setup menu toolbar")

        action_exit = QAction("Exit", self)
        action_exit.setShortcut("Ctrl+Q")
        action_exit.setStatusTip("Exit the application")
        action_exit.triggered.connect(self.close)

        action_about = QAction("&About", self)
        action_about.setStatusTip("Show the application's About box")
        action_about.triggered.connect(self.show_about)

        action_save_session = QAction(QIcon("sdbgui/images/filesave.png"), "&Save", self)
        action_save_session.setStatusTip("Save current session")
        action_save_session.setShortcut("Ctrl+S")
        action_save_session.triggered.connect(self.save_session)

        action_open_session = QAction(QIcon("sdbgui/images/fileopen.png"), "&Open", self)
        action_open_session.setShortcut("Ctrl+O")
        action_open_session.setStatusTip("Open session")
        action_open_session.triggered.connect(self.open_session)

        self.__action_continue = QAction(QIcon("sdbgui/images/next_cursor.png"), "Continue", self)
        self.__action_continue.setShortcut("Ctrl+R")
        self.__action_continue.setStatusTip("Continue to next tab")
        self.__action_continue.triggered.connect(self.switch_to_next_tab)
        self.__action_continue.setEnabled(True)

        self.__action_back = QAction(QIcon("sdbgui/images/prev_cursor.png"), "Back", self)
        self.__action_back.setShortcut("Ctrl+B")
        self.__action_back.setStatusTip("Back to previous tab")
        self.__action_back.triggered.connect(self.switch_to_previous_tab)
        self.__action_back.setEnabled(False)

        self.__action_reload = QAction(QIcon("sdbgui/images/step_in.png"), "Reload", self)
        self.__action_reload.setStatusTip("Reload Input and Reference Serializer")
        self.__action_reload.triggered.connect(self.reload_serializer)
        self.__action_reload.setEnabled(False)

        menubar = self.menuBar()
        menubar.setNativeMenuBar(False)
        self.statusBar()

        file_menu = menubar.addMenu('&File')
        file_menu.addAction(action_open_session)
        file_menu.addAction(action_save_session)
        file_menu.addAction(action_exit)

        edit_menu = menubar.addMenu('&Edit')
        edit_menu.addAction(self.__action_back)
        edit_menu.addAction(self.__action_continue)
        edit_menu.addAction(self.__action_reload)

        help_menu = menubar.addMenu('&Help')
        help_menu.addAction(action_about)

        toolbar = self.addToolBar("Toolbar")
        toolbar.addAction(action_open_session)
        toolbar.addAction(action_save_session)
        toolbar.addAction(self.__action_back)
        toolbar.addAction(self.__action_continue)
        toolbar.addAction(self.__action_reload)

    def switch_to_tab(self, tab):
        idx = tab.value if isinstance(tab, TabState) else tab
        if self.__tab_old_state == TabState(idx):
            return

        Logger.info("Switching to %s tab" % TabState(idx).name)
        self.__tab_old_state = TabState(idx)
        self.__widget_tab.setCurrentIndex(idx)
        self.__widget_tab.currentWidget().make_update()

        # First tab
        if idx == 0:
            self.__action_continue.setEnabled(True)
            self.__action_back.setEnabled(False)
        # Last tab
        elif idx == self.__widget_tab.count() - 1:
            self.__action_continue.setEnabled(False)
            self.__action_back.setEnabled(True)
        # Middle tab
        else:
            self.__action_continue.setEnabled(True)
            self.__action_back.setEnabled(True)

    def set_tab_highest_valid_state(self, state):
        """Set the state at which the data is valid i.e everything <= self.valid_tab_state is valid
        """
        self.__tab_highest_valid_state = state
        self.enable_tabs_according_to_tab_highest_valid_state()

    def enable_tabs_according_to_tab_highest_valid_state(self):
        """Enable/Disable tabs according to self.__tab_highest_valid_state
        """
        if self.__tab_highest_valid_state == TabState.Setup:
            self.__widget_tab.setTabEnabled(TabState.Setup.value, True)
            self.__widget_tab.setTabEnabled(TabState.Stencil.value, False)
            self.__widget_tab.setTabEnabled(TabState.Result.value, False)

        elif self.__tab_highest_valid_state == TabState.Stencil:
            self.__widget_tab.setTabEnabled(TabState.Setup.value, True)
            self.__widget_tab.setTabEnabled(TabState.Stencil.value, True)
            self.__widget_tab.setTabEnabled(TabState.Result.value, False)
            self.__widget_tab.widget(TabState.Stencil.value).match_fields()

            self.__action_reload.setEnabled(True)

        elif self.__tab_highest_valid_state == TabState.Result:
            self.__widget_tab.setTabEnabled(TabState.Setup.value, True)
            self.__widget_tab.setTabEnabled(TabState.Stencil.value, True)
            self.__widget_tab.setTabEnabled(TabState.Result.value, True)

    def switch_to_next_tab(self):
        self.__widget_tab.currentWidget().make_continue()

    def switch_to_previous_tab(self):
        self.__widget_tab.currentWidget().make_back()

    def center(self):
        qr = self.frameGeometry()
        cp = QDesktopWidget().availableGeometry().center()
        qr.moveCenter(cp)
        self.move(qr.topLeft())

    def show_about(self):
        self.about_widget = AboutWidget(self)

    def show_error(self, msg):
        ErrorMessageBox(self, msg)

    def save_session(self):
        Logger.info("Saveing session")

    def open_session(self):
        Logger.info("Opening session")

    def reload_serializer(self):
        Logger.info("Reloading serializers")
        try:
            self.__input_serializer_data.reload()
            self.__reference_serializer_data.reload()
        except RuntimeError as e:
            self.show_error(str(e))
            self.set_tab_highest_valid_state(TabState.Setup)
            self.switch_to_tab(TabState.Setup)
            self.__widget_tab.currentWidget().make_update()

    def closeEvent(self, event):
        self.__configuration.update_serializer_data(self.__input_serializer_data)
        self.__configuration.update_serializer_data(self.__reference_serializer_data)
        self.__configuration.store_to_file()
