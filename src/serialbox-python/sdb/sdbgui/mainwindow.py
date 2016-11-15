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
from PyQt5.QtGui import QIcon, QPixmap
from PyQt5.QtWidgets import QMainWindow, QDesktopWidget, qApp, QAction

from sdbcore.logger import Logger
from sdbcore.serializerdata import SerializerData
from sdbcore.version import Version
from .aboutmessagebox import AboutMessageBox
from .errormessagebox import ErrorMessageBox
from .setupwindow import SetupWindow
from .stencilwindow import StencilWindow


class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.init_ui()

    def init_ui(self):
        Logger.info("Initializing main window")

        self.setWindowTitle('sdb - stencil debugger (%s)' % Version().sdb_version())
        self.resize(960, 480)
        # self.move(250, 250)

        self.setWindowIcon(QIcon("sdbgui/images/favicon.ico"))

        self.__input_data = SerializerData("Input Serializer")
        self.__reference_data = SerializerData("Reference Serializer")

        self.init_menu_tool_bar()
        self.switch_to_setup_window()

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

        reload_action = QAction(QIcon("sdbgui/images/reload.png"), "&Reload", self)
        reload_action.setStatusTip("Reload the Serializers")
        reload_action.triggered.connect(self.reload)
        reload_action.setEnabled(False)

        save_icon = QPixmap("sdbgui/images/filesave.png")
        save_icon = save_icon.scaled(16, 16, Qt.KeepAspectRatio)
        save_session_action = QAction(QIcon(save_icon), "&Save..", self)
        save_session_action.setStatusTip("Save current session")
        save_session_action.setShortcut("Ctrl+S")
        save_session_action.triggered.connect(self.save_session)

        open_icon = QPixmap("sdbgui/images/fileopen.png")
        open_icon = open_icon.scaled(16, 16, Qt.KeepAspectRatio)
        open_session_action = QAction(QIcon(open_icon), "&Open..", self)
        open_session_action.setShortcut("Ctrl+O")
        open_session_action.setStatusTip("Open session")
        open_session_action.triggered.connect(self.open_session)

        menubar = self.menuBar()
        menubar.setNativeMenuBar(False)
        self.statusBar()

        file_menu = menubar.addMenu('&File')
        file_menu.addAction(open_session_action)
        file_menu.addAction(save_session_action)
        file_menu.addAction(reload_action)
        file_menu.addAction(exit_action)

        help_menu = menubar.addMenu('&Help')
        help_menu.addAction(about_action)

        toolbar = self.addToolBar("Toolbar")
        toolbar.addAction(open_session_action)
        toolbar.addAction(save_session_action)
        toolbar.addAction(reload_action)

    def switch_to_setup_window(self):
        Logger.info("Switching to setup window")

        setup_window = SetupWindow(self, self.__input_data, self.__reference_data)
        self.setCentralWidget(setup_window)
        setup_window.show()

    def switch_to_stencil_window(self):
        Logger.info("Switching to stencil window")

        stencil_window = StencilWindow(self, self.__input_data, self.__reference_data)
        self.setCentralWidget(stencil_window)
        stencil_window.show()

    def center(self):
        qr = self.frameGeometry()
        cp = QDesktopWidget().availableGeometry().center()
        qr.moveCenter(cp)
        self.move(qr.topLeft())

    def show_about(self):
        AboutMessageBox(self)

    def show_error(self, msg):
        ErrorMessageBox(self, msg)

    def reload(self):
        Logger.info("Reload current window")

    def save_session(self):
        Logger.info("Save session")

    def open_session(self):
        Logger.info("Open session")
