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

from PyQt5.QtCore import QDataStream, QIODevice
from PyQt5.QtWidgets import QLineEdit


class DroppableLineEditWidget(QLineEdit):
    """QLineEdit widget which accepts drop/drop events from QListWidgets as well as mime-data
       convertible to URLs.
    """

    SupportedFormats = ["application/x-qabstractitemmodeldatalist", "text/uri-list"]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def dropEvent(self, event):
        formats = event.mimeData().formats()

        if "application/x-qabstractitemmodeldatalist" in formats:
            encoded = event.mimeData().data("application/x-qabstractitemmodeldatalist")
            stream = QDataStream(encoded, QIODevice.ReadOnly)
            while not stream.atEnd():
                row = stream.readInt()
                col = stream.readInt()
                data = stream.readQVariantMap()
                self.setText(data[""])
        elif "text/uri-list" in formats:
            urls = event.mimeData().urls()
            if urls:
                self.setText(urls[0].toLocalFile())

    def dragEnterEvent(self, event):
        if any(f in event.mimeData().formats() for f in self.SupportedFormats):
            event.acceptProposedAction()
