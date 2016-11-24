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

# TODO: ubuntu 16.04 needs: zmq, ipython3-qtconsole and python3-pyqt5.qtsvg

from IPython.qt.console.rich_ipython_widget import RichIPythonWidget
from IPython.qt.inprocess import QtInProcessKernelManager


class QIPythonWidget(RichIPythonWidget):
    def __init__(self, customBanner=None, *args, **kwargs):
        super(QIPythonWidget, self).__init__(*args, **kwargs)

        if customBanner != None: self.banner = customBanner

        self.kernel_manager = kernel_manager = QtInProcessKernelManager()
        kernel_manager.start_kernel()
        kernel_manager.kernel.gui = 'qt'
        self.kernel_client = kernel_client = self._kernel_manager.client()
        kernel_client.start_channels()

        def stop():
            kernel_client.stop_channels()
            kernel_manager.shutdown_kernel()

        self.exit_requested.connect(stop)

    def pushVariables(self, variableDict):
        self.kernel_manager.kernel.shell.push(variableDict)

    def clearTerminal(self):
        self._control.clear()

    def printText(self, text):
        self._append_plain_text(text)

    def executeCommand(self, command):
        self._execute(command, False)
