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

from PyQt5.QtCore import QT_VERSION_STR, Qt
from PyQt5.QtWidgets import QWidget, QLabel, QVBoxLayout, QHBoxLayout, QPushButton, QSizePolicy

from sdbcore.logger import Logger
from sdbcore.version import Version
from sdbgui.pixmap import Pixmap


class PopupAboutWidget(QWidget):
    def __init__(self, parent):
        super().__init__()
        Logger.info("Showing about message box")

        self.setWindowTitle("About sdb")

        geometry = parent.geometry()
        geometry.setY(geometry.y() + int(0.10 * parent.geometry().height()))
        geometry.setX(geometry.x() + int(0.10 * parent.geometry().width()))
        geometry.setWidth(0.80 * parent.geometry().width())
        geometry.setHeight(0.80 * parent.geometry().height())
        self.setGeometry(geometry)

        image = Pixmap("logo.png")
        image_scaled = image.scaled(geometry.height(), geometry.width(), Qt.KeepAspectRatio)

        self.__widget_label_image = QLabel()
        self.__widget_label_image.setPixmap(image_scaled)

        about_txt = ("",
                     "sdb (%s)" % Version().sdb_version(),
                     "Serialbox (%s)" % Version().serialbox_version(),
                     "numpy (%s)" % Version().numpy_version(),
                     "matplotlib (%s)" % Version().matplotlib_version(),
                     "PyQt5 (%s)" % QT_VERSION_STR,
                     "IPython (%s)" % Version().ipython_version(),
                     "",
                     "Copyright (c) 2016-2017, Fabian Thuering",
                     "",
                     "All rights reserved.",
                     "",
                     "The program is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE "
                     "WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.",
                     "")

        self.__widget_label_about_txt = QLabel()
        self.__widget_label_about_txt.setText("\n".join(about_txt))
        self.__widget_label_about_txt.setWordWrap(True)

        hbox = QHBoxLayout()
        hbox.addWidget(self.__widget_label_image)
        hbox.addStretch(1)

        hbox_button = QHBoxLayout()
        hbox_button.addStretch(1)
        cancel_button = QPushButton("Cancel")
        cancel_button.clicked.connect(self.close)
        hbox_button.addWidget(cancel_button)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        vbox.addWidget(self.__widget_label_about_txt)
        vbox.addLayout(hbox_button)

        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        self.setLayout(vbox)
        self.show()

    def keyPressEvent(self, QKeyEvent):
        if QKeyEvent.key() == Qt.Key_Escape:
            Logger.info("Closing about message box")
            self.close()
