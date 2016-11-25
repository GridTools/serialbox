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

from __future__ import print_function

from optparse import OptionParser, OptionGroup, SUPPRESS_HELP
from os import path
from platform import python_version
from sys import exit, stderr
from sys import path as sys_path, argv

sys_path.insert(1, path.join(path.dirname(path.realpath(__file__)), "../"))


class BooleanOptionGroup(OptionGroup):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def add_boolean_option(self, dest, help, default):
        self.add_option('--%s' % dest, dest=dest.replace("-", "_"), action='store_true',
                        help=help + " [default: %s]" % default, default=True if default else False)
        self.add_option('--no-%s' % dest, dest=dest.replace("-", "_"), action='store_false',
                        help=SUPPRESS_HELP, default=default)


def main():
    # ===----------------------------------------------------------------------------------------===
    #   Check Python Version
    # ==-----------------------------------------------------------------------------------------===
    from sys import version_info

    if version_info < (3, 4):
        print("sdb: error: sdb requires atleast python 3.4 (detected %s)" % python_version(),
              file=stderr)
        exit(1)

    # ===----------------------------------------------------------------------------------------===
    #   Parse command-line
    # ==-----------------------------------------------------------------------------------------===
    from sdbcore.version import Version
    from sdbcore.error import fatal_error
    from sdbcore.logger import Logger, Level

    parser = OptionParser(version="sdb %s" % Version().sdb_version())
    parser.add_option("-v", "--verbose", dest="verbose", action="store_true",
                      help="enable verbose logging")

    group = BooleanOptionGroup(parser, "Configuration Options",
                               "All options take a negative form, for example --foo sets the option"
                               " foo to True, while --no-foo sets it to False.")
    group.add_boolean_option("ipython", "Embed IPython console in the error description",
                             "True of IPython is available, False otherwise")
    group.add_boolean_option("center-window", "Center main window at launch", True)
    group.add_boolean_option("default-session",
                             "Load (save) default session at startup (shutdown)", True)
    group.add_boolean_option("async", "Use asynchronous reading API of Serialbox", True)
    parser.add_option_group(group)

    parser.add_option("--move-window", metavar="X:Y", dest="move_window",
                      help="Move main window at launch by X in the horizontal and Y in the vertical")

    (options, args) = parser.parse_args()

    if options.verbose:
        Logger.set_level(Level.info)

    # ===----------------------------------------------------------------------------------------===
    #   Check if Serialbox is available
    # ===----------------------------------------------------------------------------------------===
    try:
        from serialbox import __version__
        Logger.enable_serialbox_logging()
        Logger.info("Using Serialbox: %s" % __version__)
    except ImportError as e:
        fatal_error("serialbox not found: %s" % e)

    # ===----------------------------------------------------------------------------------------===
    #   Check if PyQt5 is available
    # ===----------------------------------------------------------------------------------------===
    try:
        from PyQt5.QtCore import QT_VERSION_STR
        Logger.info("Using PyQt5: %s" % QT_VERSION_STR)
    except ImportError as e:
        fatal_error("sdb: error: PyQt5 not found: %s" % e)

    # ===----------------------------------------------------------------------------------------===
    #   Check if IPython is available
    # ===----------------------------------------------------------------------------------------===

    # ===----------------------------------------------------------------------------------------===
    #  Set global configuration option
    # ===----------------------------------------------------------------------------------------===
    from sdbgui.globalconfig import GlobalConfig
    from PyQt5.QtCore import QPoint

    GlobalConfig()["ipython"] = options.ipython
    GlobalConfig()["default_session"] = options.default_session
    GlobalConfig()["center_window"] = options.center_window
    GlobalConfig()["async"] = options.async

    if options.move_window:
        try:
            point = options.move_window.split(":")
            GlobalConfig()["move_window"] = QPoint(int(point[0]), int(point[1]))
            GlobalConfig()["center_window"] = False
        except (IndexError, ValueError) as e:
            fatal_error("invalid value of option \"--move-window\": %s" % e)
    else:
        GlobalConfig()["move_window"] = None

    # ===----------------------------------------------------------------------------------------===
    #  Launch Gui
    # ===----------------------------------------------------------------------------------------===
    from PyQt5.QtWidgets import QApplication
    from sdbgui.mainwindow import MainWindow

    app = QApplication(argv)
    mainWindow = MainWindow()
    exit(app.exec_())


if __name__ == '__main__':
    main()
