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
from PyQt5.QtGui import QPixmap
from PyQt5.QtWidgets import QWidget, QLabel, QVBoxLayout, QHBoxLayout, QPushButton, QSizePolicy

from sdbcore.logger import Logger
from sdbcore.version import Version


class AboutWidget(QWidget):
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

        image = QPixmap("sdbgui/images/logo.png")
        image_scaled = image.scaled(geometry.height(), geometry.width(), Qt.KeepAspectRatio)

        image_label = QLabel()
        image_label.setPixmap(image_scaled)

        about_txt = ("",
                     "sdb (%s)" % Version().sdb_version(),
                     "Serialbox (%s)" % Version().serialbox_version(),
                     "numpy (%s)" % Version().numpy_version(),
                     "PyQt5 (%s)" % QT_VERSION_STR,
                     "",
                     "Copyright (c) 2016, Fabian Thuering",
                     "",
                     "All rights reserved.",
                     "",
                     "The program is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE "
                     "WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.",
                     "")

        about_txt_label = QLabel()
        about_txt_label.setText("\n".join(about_txt))
        about_txt_label.setWordWrap(True)

        hbox = QHBoxLayout()
        hbox.addWidget(image_label)
        hbox.addStretch(1)

        hbox_button = QHBoxLayout()
        hbox_button.addStretch(1)
        cancel_button = QPushButton("Cancel")
        cancel_button.clicked.connect(self.close)
        hbox_button.addWidget(cancel_button)

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)
        vbox.addWidget(about_txt_label)
        vbox.addLayout(hbox_button)

        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        self.setLayout(vbox)
        self.show()

    def keyPressEvent(self, QKeyEvent):
        if QKeyEvent.key() == Qt.Key_Escape:
            self.close()
