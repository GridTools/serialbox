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

from .globalconfig import GlobalConfig

SDB_HAS_IPYTHON = False
SDB_IPYTHON_IMPORT_ERROR = None

if GlobalConfig()["ipython"]:
    try:
        from os import environ

        environ['QT_API'] = 'pyqt5'

        from IPython.qt.console.rich_ipython_widget import RichIPythonWidget
        from IPython.qt.inprocess import QtInProcessKernelManager
        from sdbcore.logger import Logger

        # IPython won't work if this is not correctly installed and the error message will be
        # misleading
        from PyQt5 import QtSvg

        SDB_HAS_IPYTHON = True

    except ImportError as e:
        from sdbcore.logger import Logger

        SDB_IPYTHON_IMPORT_ERROR = e
        Logger.warning("cannot import ipython: %s" % e)
else:
    SDB_HAS_IPYTHON = False

if SDB_HAS_IPYTHON:
    from sdbcore.version import Version


    class ErrorConsoleIPythonWidget(RichIPythonWidget):
        """Embedded IPython console
        """

        def __init__(self, *args, **kwargs):
            super(ErrorConsoleIPythonWidget, self).__init__(*args, **kwargs)

            self.kernel_manager = kernel_manager = QtInProcessKernelManager()
            kernel_manager.start_kernel()
            kernel_manager.kernel.gui = 'qt'
            self.kernel_client = kernel_client = self._kernel_manager.client()
            kernel_client.start_channels()

            self._append_plain_text("\nsdb (%s) -- Stencil Debugger." % Version().sdb_version())
            self._append_plain_text(
                "\nThe input and refrence fields, savepoints and serializers have been loaded. "
                "\nUse the 'whos' command for more information.")

        def push_variables(self, variables):
            """ Push variables to the IPython console

            :param variables: dictionary of variables to push to the console
            :type variables: dict
            """
            self.kernel_manager.kernel.shell.push(variables)

        def clear_terminal(self):
            """ Clear the terminal """
            self._control.clear()

        def print_text(self, text):
            """ Prints some plain text to the console """
            self._append_plain_text(text)

        def execute_command(self, command):
            """ Execute a command in the frame of the console widget """
            self._execute(command, False)
