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

from PyQt5.QtWidgets import QMessageBox, QLabel, QHBoxLayout, QGridLayout, QVBoxLayout


from sdbcore.logger import Logger


class FieldMetainfoBox(QMessageBox):
    def __init__(self, parent, field_name, field_metainfo):
        super().__init__(parent)

        Logger.info("Showing FieldMetainfoBox of field '%s'" % field_name)

        self.__field_name = field_name
        self.__field_metainfo = field_metainfo

        self.setWindowTitle("Field meta-information")

        format_header = lambda key, value: "<b>%s</b> : %s<br/>" % (key, value)
        format_metainfo = lambda key, value: "<li><b>%s</b> : %s</li>" % (key, value)

        text = format_header("Field", self.__field_name)
        text += format_header("Dimension", self.__field_metainfo.dims)
        text += format_header("Type", self.__field_metainfo.type)

        metainfo = self.__field_metainfo.metainfo.to_dict()
        if metainfo:
            text += "<b>Metainfo:</b><ul>"
            for key, value in metainfo.items():
                text += format_metainfo(key, value)
            text += "</ul>"

        self.setText(text)
        self.show()
