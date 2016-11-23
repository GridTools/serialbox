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

from random import random

from PyQt5.QtCore import QSize
from PyQt5.QtGui import QPixmap, QMovie
from PyQt5.QtWidgets import QLabel, QVBoxLayout, QTableWidget, QWidget, QHBoxLayout, QHeaderView


def make_unqiue_and_preserve_order(seq):
    seen = set()
    seen_add = seen.add
    return [x for x in seq if not (x in seen or seen_add(x))]


class ResultTableWidget(QWidget):
    def __init__(self, stencil_field_mapper):
        super().__init__()

        # Data
        self.__stencil_field_mapper = stencil_field_mapper
        self.__draw_success_icons = True
        self.__draw_failure_icons = True

        # Widgets
        self.__widget_table = QTableWidget()
        self.__widget_title = QLabel("")
        self.__widget_menu = QLabel("check")
        self.__widget_result = QLabel("")
        self.__widget_result_icon = QLabel("")

        vbox = QVBoxLayout()

        hbox_top = QHBoxLayout()
        hbox_top.addWidget(self.__widget_title)
        hbox_top.addStretch(1)
        hbox_top.addWidget(self.__widget_menu)
        vbox.addLayout(hbox_top)

        vbox.addWidget(self.__widget_table)

        hbox_bottom = QHBoxLayout()
        hbox_bottom.addWidget(self.__widget_result)
        hbox_bottom.addWidget(self.__widget_result_icon)
        hbox_bottom.addStretch(1)

        vbox.addLayout(hbox_bottom)

        self.setLayout(vbox)

    def make_update(self):
        comparison_result = self.__stencil_field_mapper.comparison_result

        self.__widget_title.setText("<b>%s</b>" % comparison_result["stencil_name"])

        # Compute stages and fields
        stages = []
        fields = []
        num_errors = 0

        for result in comparison_result["result"]:
            stages += [result["stage"] + " [" + result["intent"] + "]"]

            if result["input_field_name"] == result["reference_field_name"]:
                fields += [result["input_field_name"]]
            else:
                fields += [
                    result["input_field_name"] + "  (" + result["reference_field_name"] + ")"]

            num_errors += not result["match"]

        stages = make_unqiue_and_preserve_order(stages)
        fields = make_unqiue_and_preserve_order(fields)

        # Setup headers of table
        self.__widget_table.setRowCount(len(fields))
        self.__widget_table.setColumnCount(len(stages))

        self.__widget_table.setHorizontalHeaderLabels(stages)
        self.__widget_table.setVerticalHeaderLabels(fields)
        self.__widget_table.setStyleSheet(
            '''
                QTableWidget::item:selected:active {
                    background: #FFFFFF;
                    border-style: solid;
                    border-color: #D4D8DD;
                    border-width: 2px;
                }
            ''')

        self.__widget_table.horizontalHeader().resizeSections(QHeaderView.Stretch)
        self.__widget_table.setEditTriggers(QTableWidget.NoEditTriggers)

        # Populate entries
        for result in comparison_result["result"]:
            stage_idx = stages.index(result["stage"] + " [" + result["intent"] + "]")

            field_idx = fields.index(
                [f for f in fields if f.startswith(result["input_field_name"])][0])

            cell = QWidget()

            #TODO: move this in a seperate CellWidget and attach the info of match
            icon = QLabel()
            if result["match"]:
                if self.__draw_success_icons:
                    icon.setPixmap(QPixmap("sdbgui/images/success.png"))
            elif self.__draw_failure_icons:
                    icon.setPixmap(QPixmap("sdbgui/images/failure.png"))

            layout = QHBoxLayout(cell)
            layout.addStretch(1)
            layout.addWidget(icon)
            layout.addStretch(1)
            cell.setLayout(layout)
            self.__widget_table.setCellWidget(field_idx, stage_idx, cell)

        # Set bottom widgets
        if num_errors != 0:
            self.__widget_result_icon.clear()
            self.__widget_result.setText(
                "<b>%s error%s detected</b>" % (num_errors, "s" if num_errors > 1 else ""))
            self.__widget_result.setStyleSheet("QLabel {color: #B72424}")
        else:
            if random() < 0.2:
                rnd = random()
                if rnd < 0.33:
                    movie = QMovie("sdbgui/images/dance_1.gif")
                    movie.setScaledSize(QSize(21, 25))
                elif rnd < 0.66:
                    movie = QMovie("sdbgui/images/dance_2.gif")
                    movie.setScaledSize(QSize(42, 25))
                else:
                    movie = QMovie("sdbgui/images/dance_3.gif")
                    movie.setScaledSize(QSize(20, 25))

                self.__widget_result_icon.setMovie(movie)
                movie.start()
            else:
                self.__widget_result_icon.clear()

            self.__widget_result.setText("<b>No errors detected! Hurray!</b>")
            self.__widget_result.setStyleSheet("QLabel {color: #478E40}")
