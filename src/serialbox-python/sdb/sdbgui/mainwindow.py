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
from PyQt5.QtGui import QIcon
from PyQt5.QtWidgets import QMainWindow, QDesktopWidget, qApp, QAction, QTabWidget

from sdbcore.logger import Logger
from sdbcore.version import Version
from .aboutwidget import AboutWidget
from .configuration import Configuration
from .errormessagebox import ErrorMessageBox
from .setupwindow import SetupWindow
from .stencilwindow import StencilWindow
from .tabstate import TabState


class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        Logger.info("Initializing main window")

        # Members
        self.configuration = Configuration()
        self.configuration.load_from_file()

        self.input_data = self.configuration.make_serializer_data("Input Serializer")
        self.reference_data = self.configuration.make_serializer_data("Reference Serializer")

        # Initialize GUI
        self.setWindowTitle('sdb - stencil debugger (%s)' % Version().sdb_version())
        self.resize(960, 480)

        self.setWindowIcon(QIcon("sdbgui/images/favicon.ico"))
        self.init_menu_tool_bar()

        # Setup tabs
        self.valid_tab_state = TabState.Setup
        self.tab_widget = QTabWidget()
        self.tab_widget.addTab(SetupWindow(self, self.input_data, self.reference_data),
                               "Setup")
        self.tab_widget.addTab(StencilWindow(self, self.input_data, self.reference_data),
                               "Stencil")
        self.tab_widget.currentChanged.connect(self.switch_to_tab)

        self.tab_widget.setTabEnabled(TabState.Stencil.value, False)

        self.old_tab_state = TabState.Setup
        self.valid_tab_state = TabState.Setup
        self.switch_to_tab(TabState.Setup)

        self.setCentralWidget(self.tab_widget)

        Logger.info("Starting main loop")
        self.show()

    def init_menu_tool_bar(self):
        Logger.info("Initializing menu toolbar")

        exit_action = QAction("Exit", self)
        exit_action.setShortcut("Ctrl+Q")
        exit_action.setStatusTip("Exit the application")
        exit_action.triggered.connect(qApp.quit)

        about_action = QAction("&About", self)
        about_action.setStatusTip("Show the application's About box")
        about_action.triggered.connect(self.show_about)

        save_session_action = QAction(QIcon("sdbgui/images/filesave.png"), "&Save", self)
        save_session_action.setStatusTip("Save current session")
        save_session_action.setShortcut("Ctrl+S")
        save_session_action.triggered.connect(self.save_session)

        open_session_action = QAction(QIcon("sdbgui/images/fileopen.png"), "&Open", self)
        open_session_action.setShortcut("Ctrl+O")
        open_session_action.setStatusTip("Open session")
        open_session_action.triggered.connect(self.open_session)

        continue_action = QAction(QIcon("sdbgui/images/next_cursor.png"), "Continue", self)
        continue_action.setShortcut("Ctrl+R")
        continue_action.setStatusTip("Continue to next tab")
        continue_action.triggered.connect(self.switch_to_next_tab)

        back_action = QAction(QIcon("sdbgui/images/prev_cursor.png"), "Back", self)
        back_action.setShortcut("Ctrl+B")
        back_action.setStatusTip("Back to previous tab")
        back_action.triggered.connect(self.switch_to_previous_tab)

        menubar = self.menuBar()
        menubar.setNativeMenuBar(False)
        self.statusBar()

        file_menu = menubar.addMenu('&File')
        file_menu.addAction(open_session_action)
        file_menu.addAction(save_session_action)
        file_menu.addAction(exit_action)

        edit_menu = menubar.addMenu('&Edit')
        edit_menu.addAction(back_action)
        edit_menu.addAction(continue_action)

        help_menu = menubar.addMenu('&Help')
        help_menu.addAction(about_action)

        toolbar = self.addToolBar("Toolbar")
        toolbar.addAction(open_session_action)
        toolbar.addAction(save_session_action)
        toolbar.addAction(back_action)
        toolbar.addAction(continue_action)

    def switch_to_tab(self, tab):
        idx = tab.value if isinstance(tab, TabState) else tab
        if self.old_tab_state == TabState(idx):
            return

        Logger.info("Switching to tab: %s" % TabState(idx))
        self.old_tab_state = TabState(idx)
        self.tab_widget.setCurrentIndex(idx)
        self.tab_widget.currentWidget().make_update()

    def set_valid_tab_state(self, state):
        """Set the state at which the data is valid i.e everything <= self.valid_tab_state is valid
        """
        self.valid_tab_state = state
        self.enable_tabs_according_to_valid_tab_state()

    def enable_tabs_according_to_valid_tab_state(self):
        """Enable/Disable tabs according to self.valid_tab_state
        """
        if self.valid_tab_state == TabState.Setup:
            self.tab_widget.setTabEnabled(TabState.Setup.value, True)
            self.tab_widget.setTabEnabled(TabState.Stencil.value, False)

        elif self.valid_tab_state == TabState.Stencil:
            self.tab_widget.setTabEnabled(TabState.Setup.value, True)
            self.tab_widget.setTabEnabled(TabState.Stencil.value, True)

    def switch_to_next_tab(self):
        self.tab_widget.currentWidget().make_continue()

    def switch_to_previous_tab(self):
        self.tab_widget.currentWidget().make_back()

    def center(self):
        qr = self.frameGeometry()
        cp = QDesktopWidget().availableGeometry().center()
        qr.moveCenter(cp)
        self.move(qr.topLeft())

    def show_about(self):
        self.about_widget = AboutWidget(self)

    def show_error(self, msg):
        ErrorMessageBox(self, msg)

    def reload(self):
        Logger.info("Reload current window")

    def save_session(self):
        Logger.info("Save session")

    def open_session(self):
        Logger.info("Open session")

    def closeEvent(self, event):
        self.configuration.upade_serializer_data(self.input_data)
        self.configuration.upade_serializer_data(self.reference_data)
        self.configuration.sotre_to_file()
