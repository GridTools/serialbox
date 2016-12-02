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

from os import getcwd
from time import time

from PyQt5.QtCore import QFileSystemWatcher, QUrl
from PyQt5.QtGui import QDesktopServices
from PyQt5.QtWidgets import (QMainWindow, QDesktopWidget, QAction, QTabWidget, QMessageBox,
                             QFileDialog)

from sdbcore.logger import Logger
from sdbcore.serializerdata import SerializerData
from sdbcore.stencildata import StencilData
from sdbcore.stencilfieldmapper import StencilFieldMapper
from sdbcore.version import Version
from sdbgui.errorwindow import ErrorWindow
from sdbgui.globalconfig import GlobalConfig
from sdbgui.icon import Icon
from sdbgui.popupaboutwidget import PopupAboutWidget
from sdbgui.resultwindow import ResultWindow
from sdbgui.sessionmanager import SessionManager
from sdbgui.setupwindow import SetupWindow
from sdbgui.stencilwindow import StencilWindow
from sdbgui.tabstate import TabState


class MainWindow(QMainWindow):
    OnlineHelpUrl = QUrl("https://thfabian.github.io/serialbox2/sdb.html")

    def __init__(self):
        super().__init__()
        Logger.info("Setup main window")

        self.__input_serializer_data = SerializerData("Input Serializer")
        self.__input_stencil_data = StencilData(self.__input_serializer_data)

        self.__reference_serializer_data = SerializerData("Reference Serializer")
        self.__reference_stencil_data = StencilData(self.__reference_serializer_data)

        self.__stencil_field_mapper = StencilFieldMapper(self.__input_stencil_data,
                                                         self.__reference_stencil_data,
                                                         GlobalConfig()["async"])

        self.__file_system_watcher = QFileSystemWatcher()
        self.__file_system_watcher.directoryChanged[str].connect(self.popup_reload_box)
        self.__file_system_watcher_last_modify = time()

        # Load from session?
        self.__session_manager = SessionManager()

        if GlobalConfig()["default_session"]:
            self.__session_manager.load_from_file()

        self.__session_manager.set_serializer_data(self.__input_serializer_data)
        self.__session_manager.set_serializer_data(self.__reference_serializer_data)

        # Setup GUI
        self.setWindowTitle('sdb - stencil debugger (%s)' % Version().sdb_version())
        self.resize(1200, 600)

        if GlobalConfig()["center_window"]:
            self.center()

        if GlobalConfig()["move_window"]:
            self.move(GlobalConfig()["move_window"])

        self.setWindowIcon(Icon("logo-small.png"))
        self.init_menu_tool_bar()

        # Tabs
        self.__tab_highest_valid_state = TabState.Setup
        self.__widget_tab = QTabWidget(self)

        # Setup tab
        self.__widget_tab.addTab(
            SetupWindow(self, self.__input_serializer_data, self.__reference_serializer_data),
            "Setup")

        # Stencil tab
        self.__widget_tab.addTab(
            StencilWindow(self, self.__stencil_field_mapper, self.__input_stencil_data,
                          self.__reference_stencil_data),
            "Stencil")

        # Result tab
        self.__widget_tab.addTab(
            ResultWindow(self, self.__widget_tab.widget(TabState.Stencil.value),
                         self.__stencil_field_mapper),
            "Result")

        # Error tab
        self.__widget_tab.addTab(ErrorWindow(self), "Error")

        self.__widget_tab.currentChanged.connect(self.switch_to_tab)

        self.__widget_tab.setTabEnabled(TabState.Setup.value, True)
        self.__widget_tab.setTabEnabled(TabState.Stencil.value, False)
        self.__widget_tab.setTabEnabled(TabState.Result.value, False)
        self.__widget_tab.setTabEnabled(TabState.Error.value, False)

        self.__widget_tab.setTabToolTip(TabState.Setup.value,
                                        "Setup Input and Refrence Serializer")
        self.__widget_tab.setTabToolTip(TabState.Stencil.value,
                                        "Set the stencil to compare and define the mapping of the fields")
        self.__widget_tab.setTabToolTip(TabState.Result.value,
                                        "View to comparison result")
        self.__widget_tab.setTabToolTip(TabState.Error.value,
                                        "Detailed error desscription of the current field")

        self.__tab_current_state = TabState.Setup
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
        action_about.triggered.connect(self.popup_about_box)

        action_save_session = QAction(Icon("filesave.png"), "&Save", self)
        action_save_session.setStatusTip("Save current session")
        action_save_session.setShortcut("Ctrl+S")
        action_save_session.triggered.connect(self.save_session)

        action_open_session = QAction(Icon("fileopen.png"), "&Open", self)
        action_open_session.setShortcut("Ctrl+O")
        action_open_session.setStatusTip("Open session")
        action_open_session.triggered.connect(self.open_session)

        action_help = QAction(Icon("help.png"), "&Online Help", self)
        action_help.setStatusTip("Online Help")
        action_help.setToolTip("Online Help")
        action_help.triggered.connect(self.go_to_online_help)

        self.__action_continue = QAction(Icon("next_cursor.png"), "Continue", self)
        self.__action_continue.setStatusTip("Continue to next tab")
        self.__action_continue.triggered.connect(self.switch_to_next_tab)
        self.__action_continue.setEnabled(True)

        self.__action_back = QAction(Icon("prev_cursor.png"), "Back", self)
        self.__action_back.setStatusTip("Back to previous tab")
        self.__action_back.triggered.connect(self.switch_to_previous_tab)
        self.__action_back.setEnabled(False)

        self.__action_reload = QAction(Icon("step_in.png"), "Reload", self)
        self.__action_reload.setStatusTip("Reload Input and Reference Serializer")
        self.__action_reload.setShortcut("Ctrl+R")
        self.__action_reload.triggered.connect(self.reload_serializer)
        self.__action_reload.setEnabled(False)

        self.__action_try_switch_to_error_tab = QAction(Icon("visualize.png"),
                                                        "Detailed error description", self)
        self.__action_try_switch_to_error_tab.setStatusTip(
            "Detailed error desscription of the current field")
        self.__action_try_switch_to_error_tab.triggered.connect(self.try_switch_to_error_tab)
        self.__action_try_switch_to_error_tab.setEnabled(False)

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
        help_menu.addAction(action_help)

        toolbar = self.addToolBar("Toolbar")
        toolbar.addAction(action_help)
        toolbar.addAction(action_open_session)
        toolbar.addAction(action_save_session)
        toolbar.addAction(self.__action_back)
        toolbar.addAction(self.__action_continue)
        toolbar.addAction(self.__action_reload)
        toolbar.addAction(self.__action_try_switch_to_error_tab)

    def center(self):
        qr = self.frameGeometry()
        cp = QDesktopWidget().availableGeometry().center()
        qr.moveCenter(cp)
        self.move(qr.topLeft())

    def closeEvent(self, event):
        self.__session_manager.update_serializer_data(self.__input_serializer_data)
        self.__session_manager.update_serializer_data(self.__reference_serializer_data)

        if GlobalConfig()["default_session"]:
            self.__session_manager.store_to_file()

    # ===----------------------------------------------------------------------------------------===
    #   TabWidgets
    # ==-----------------------------------------------------------------------------------------===

    def tab_widget(self, idx):
        return self.__widget_tab.widget(idx if not isinstance(idx, TabState) else idx.value)

    def switch_to_tab(self, tab):
        idx = tab.value if isinstance(tab, TabState) else tab
        if self.__tab_current_state == TabState(idx):
            return

        Logger.info("Switching to %s tab" % TabState(idx).name)
        self.__tab_current_state = TabState(idx)

        self.__widget_tab.setCurrentIndex(idx)
        self.tab_widget(idx).make_update()

        self.__action_try_switch_to_error_tab.setEnabled(TabState(idx) == TabState.Result)

        # Error tab is always disabled if not in "Error"
        self.__widget_tab.setTabEnabled(TabState.Error.value, TabState(idx) == TabState.Error)

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
            self.__widget_tab.setTabEnabled(TabState.Error.value, False)

            self.__action_try_switch_to_error_tab.setEnabled(False)

            watched_directories = self.__file_system_watcher.directories()
            if watched_directories:
                self.__file_system_watcher.removePaths(self.__file_system_watcher.directories())

        elif self.__tab_highest_valid_state == TabState.Stencil:

            self.__file_system_watcher.addPath(self.__input_serializer_data.serializer.directory)
            self.__file_system_watcher.addPath(self.__reference_stencil_data.serializer.directory)

            self.__widget_tab.setTabEnabled(TabState.Setup.value, True)
            self.__widget_tab.setTabEnabled(TabState.Stencil.value, True)
            self.__widget_tab.setTabEnabled(TabState.Result.value, False)
            self.__widget_tab.setTabEnabled(TabState.Error.value, False)

            self.__widget_tab.widget(TabState.Stencil.value).match_fields()

            self.__action_reload.setEnabled(True)
            self.__action_try_switch_to_error_tab.setEnabled(False)

        elif self.__tab_highest_valid_state == TabState.Result:
            self.__widget_tab.setTabEnabled(TabState.Setup.value, True)
            self.__widget_tab.setTabEnabled(TabState.Stencil.value, True)
            self.__widget_tab.setTabEnabled(TabState.Result.value, True)
            self.__widget_tab.setTabEnabled(TabState.Error.value, True)

            self.__action_try_switch_to_error_tab.setEnabled(True)

        elif self.__tab_highest_valid_state == TabState.Error:
            self.__widget_tab.setTabEnabled(TabState.Setup.value, True)
            self.__widget_tab.setTabEnabled(TabState.Stencil.value, True)
            self.__widget_tab.setTabEnabled(TabState.Result.value, True)
            self.__action_try_switch_to_error_tab.setEnabled(False)

    def switch_to_next_tab(self):
        self.__widget_tab.currentWidget().make_continue()

    def switch_to_previous_tab(self):
        self.__widget_tab.currentWidget().make_back()

    def try_switch_to_error_tab(self):
        if self.__widget_tab.widget(TabState.Result.value).try_switch_to_error_tab():
            self.__widget_tab.setTabEnabled(TabState.Error.value, True)

    def error_window_set_result_data(self, result_data):
        self.__widget_tab.widget(TabState.Error.value).set_result_data(result_data)

    # ===----------------------------------------------------------------------------------------===
    #   PopupWidgets
    # ==-----------------------------------------------------------------------------------------===

    def popup_about_box(self):
        self.__about_widget = PopupAboutWidget(self)

    def popup_error_box(self, msg):
        Logger.error(
            msg.replace("<b>", "").replace("</b>", "").replace("<br />", ":").replace("<br/>", ":"))

        msg_box = QMessageBox()
        msg_box.setWindowTitle("Error")
        msg_box.setIcon(QMessageBox.Critical)
        msg_box.setText(msg)
        msg_box.setStandardButtons(QMessageBox.Ok)
        reply = msg_box.exec_()  # Blocking

    def popup_reload_box(self, path):
        self.__file_system_watcher.blockSignals(True)
        reply = QMessageBox.question(self, "Reload serializer?",
                                     "The path \"%s\" has changed.\nDo want to reload the serializers?" % path,
                                     QMessageBox.Yes | QMessageBox.No,
                                     QMessageBox.Yes)
        if reply == QMessageBox.Yes:
            self.reload_serializer()

        self.__file_system_watcher.blockSignals(False)

    # ===----------------------------------------------------------------------------------------===
    #   Session manager
    # ==-----------------------------------------------------------------------------------------===

    def save_session(self):
        Logger.info("Try saving current session")

        dialog = QFileDialog(self, "Save current session")
        dialog.setAcceptMode(QFileDialog.AcceptSave)
        dialog.setDefaultSuffix("json")
        dialog.setDirectory(getcwd())

        if not dialog.exec_():
            Logger.info("Abort saving current session")
            return

        filename = dialog.selectedFiles()
        self.__session_manager.update_serializer_data(self.__input_serializer_data)
        self.__session_manager.update_serializer_data(self.__reference_serializer_data)

        ret, msglist = self.__session_manager.store_to_file(filename[0])
        if not ret:
            self.popup_error_box(
                "Failed to save configuration file: %s\n%s " % (filename[0], msglist[0]))

    def open_session(self):
        Logger.info("Try opening session")
        filename = QFileDialog.getOpenFileName(self, "Open Session", getcwd(),
                                               "JSON configuration (*.json)")[0]

        if filename is None or filename is "":
            Logger.info("Abort opening session")
            return

        ret, msglist = self.__session_manager.load_from_file(filename)
        if not ret:
            self.popup_error_box(
                "Failed to load configuration file: %s\n%s " % (filename, msglist[0]))
        else:
            Logger.info("Successfully opened session")
            self.__session_manager.set_serializer_data(self.__input_serializer_data)
            self.__session_manager.set_serializer_data(self.__reference_serializer_data)
            self.switch_to_tab(TabState.Setup)

    @property
    def session_manager(self):
        return self.__session_manager

    # ===----------------------------------------------------------------------------------------===
    #   Reload Serializer
    # ==-----------------------------------------------------------------------------------------===

    def reload_serializer(self):
        Logger.info("Reloading serializers")
        try:
            self.__input_serializer_data.reload()
            self.__reference_serializer_data.reload()

            if self.__widget_tab.currentIndex() == TabState.Error.value:
                self.switch_to_tab(TabState.Result)

            self.__widget_tab.currentWidget().make_update()

        except RuntimeError as e:
            self.popup_error_box(str(e))
            self.set_tab_highest_valid_state(TabState.Setup)
            self.switch_to_tab(TabState.Setup)
            self.__widget_tab.currentWidget().make_update()

    # ===----------------------------------------------------------------------------------------===
    #   Online help
    # ==-----------------------------------------------------------------------------------------===

    def go_to_online_help(self):
        Logger.info("Opening online help")
        QDesktopServices.openUrl(MainWindow.OnlineHelpUrl)
