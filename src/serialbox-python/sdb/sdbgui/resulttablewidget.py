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

from PyQt5.QtCore import QSize, Qt, QPoint
from PyQt5.QtGui import QMovie, QIcon
from PyQt5.QtWidgets import (QLabel, QVBoxLayout, QTableWidget, QWidget, QHBoxLayout, QHeaderView,
                             QCheckBox, QTableWidgetItem)

from .resulttablecellwidget import ResultTableCellWidget
from .tabstate import TabState


def make_unqiue_and_preserve_order(seq):
    seen = set()
    seen_add = seen.add
    return [x for x in seq if not (x in seen or seen_add(x))]


def make_stage_name(result):
    # TODO: Handle the case when input_stage != reference_stage
    return result["input_stage"] + " [" + result["intent"] + "]"


class ResultTableWidget(QWidget):
    def __init__(self, resultwindow, stencil_field_mapper):
        super().__init__(resultwindow)

        # Data
        self.__stencil_field_mapper = stencil_field_mapper
        self.__draw_success_icons = True
        self.__draw_failure_icons = True

        self.__table_data = None
        self.__current_cell_row = None
        self.__current_cell_col = None

        # Widgets
        self.__widget_resultwindow = resultwindow
        self.__widget_table = QTableWidget(self)
        self.__widget_label_title = QLabel("")

        self.__widget_checkbox_draw_success = QCheckBox()
        self.__widget_checkbox_draw_success.setIcon(QIcon("sdbgui/images/success.png"))
        self.__widget_checkbox_draw_success.setChecked(True)
        self.__widget_checkbox_draw_success.stateChanged[int].connect(self.set_draw_success)
        self.__widget_checkbox_draw_success.setStatusTip("Show success icons")

        self.__widget_checkbox_draw_failure = QCheckBox()
        self.__widget_checkbox_draw_failure.setIcon(QIcon("sdbgui/images/failure-small.png"))
        self.__widget_checkbox_draw_failure.setChecked(True)
        self.__widget_checkbox_draw_failure.stateChanged[int].connect(self.set_draw_failure)
        self.__widget_checkbox_draw_failure.setStatusTip("Show failure icons")

        self.__widget_label_result = QLabel("")
        self.__widget_label_result_icon = QLabel("")

        self.__widget_label_loading = QLabel("")

        vbox = QVBoxLayout()

        hbox_top = QHBoxLayout()
        hbox_top.addWidget(self.__widget_label_title)
        hbox_top.addStretch(1)
        hbox_top.addWidget(self.__widget_checkbox_draw_success)
        hbox_top.addWidget(self.__widget_checkbox_draw_failure)
        vbox.addLayout(hbox_top)

        vbox.addWidget(self.__widget_table)

        hbox_bottom = QHBoxLayout()
        hbox_bottom.addWidget(self.__widget_label_result)
        hbox_bottom.addWidget(self.__widget_label_result_icon)
        hbox_bottom.addStretch(1)
        hbox_bottom.addWidget(self.__widget_label_loading)

        vbox.addLayout(hbox_bottom)

        self.setLayout(vbox)

    def make_update(self):
        comparison_result_list = self.__stencil_field_mapper.comparison_result_list

        self.__widget_label_title.setText(
            "<b>%s</b>" % comparison_result_list.shared_stencil_name())

        # Compute stages and fields
        stages = []
        fields = []
        fields_tooltip = []
        num_errors = 0
        first_error_cell = None

        for result in comparison_result_list.results:
            stages += [make_stage_name(result)]

            input_field = result["input_field_name"]
            reference_field = result["reference_field_name"]

            if input_field == reference_field:
                fields += input_field
                fields_tooltip += ["Field: \"%s\"" % input_field]
            else:
                fields += ["%s  (%s)" % (input_field, reference_field)]
                fields_tooltip += [
                    "Input field: \"%s\", Reference field: \"%s\"" % (input_field, reference_field)]

            num_errors += not result["match"]

        stages = make_unqiue_and_preserve_order(stages)
        fields = make_unqiue_and_preserve_order(fields)
        fields_tooltip = make_unqiue_and_preserve_order(fields_tooltip)

        # Setup headers of table
        rows = len(fields)
        cols = len(stages)
        self.__widget_table.setRowCount(rows)
        self.__widget_table.setColumnCount(cols)
        self.__table_data = [([None] * cols) for row in range(rows)]

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

        for i in range(self.__widget_table.rowCount()):
            item = self.__widget_table.verticalHeaderItem(i)
            item.setToolTip(fields_tooltip[i])

        self.__widget_table.horizontalHeader().resizeSections(QHeaderView.Stretch)
        self.__widget_table.setEditTriggers(QTableWidget.NoEditTriggers)

        self.__widget_table.setContextMenuPolicy(Qt.CustomContextMenu)
        self.__widget_table.cellClicked[int, int].connect(self.cell_left_clicked)
        self.__widget_table.customContextMenuRequested[QPoint].connect(self.cell_right_clicked)

        # Populate table
        for result in comparison_result_list.results:
            stage_idx = stages.index(make_stage_name(result))
            field_idx = fields.index(
                [f for f in fields if f.startswith(result["input_field_name"])][0])

            # Widget
            cell = ResultTableCellWidget(result["match"])
            cell.set_icon(self.__draw_success_icons, self.__draw_failure_icons)
            self.__widget_table.setCellWidget(field_idx, stage_idx, cell)

            # Save the first error for selection
            if not result["match"] and not first_error_cell:
                first_error_cell = [field_idx, stage_idx]
                first_error_cell = [field_idx, stage_idx]

            # Item
            cell_item = QTableWidgetItem("")
            self.__widget_table.setItem(field_idx, stage_idx, cell_item)

            # Data
            self.__table_data[field_idx][stage_idx] = result

        # Set bottom widgets
        if num_errors != 0:
            self.__widget_label_result_icon.clear()
            self.__widget_label_result.setText(
                "<b>%s error%s detected</b>" % (num_errors, "s" if num_errors > 1 else ""))
            self.__widget_label_result.setStyleSheet("QLabel {color: #B72424}")

            # Emulate "left" click on first error
            self.__widget_table.setCurrentCell(first_error_cell[0], first_error_cell[1])
            self.cell_left_clicked(first_error_cell[0], first_error_cell[1])

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

                self.__widget_label_result_icon.setMovie(movie)
                movie.start()
            else:
                self.__widget_label_result_icon.clear()

            self.__widget_label_result.setText("<b>No errors detected! Hurray!</b>")
            self.__widget_label_result.setStyleSheet("QLabel {color: #478E40}")

    def set_draw_success(self, state):
        self.__draw_success_icons = True if state == Qt.Checked else False
        self.update_icons()

    def set_draw_failure(self, state):
        self.__draw_failure_icons = True if state == Qt.Checked else False
        self.update_icons()

    def update_icons(self):
        for i in range(self.__widget_table.rowCount()):
            for j in range(self.__widget_table.columnCount()):
                if self.__widget_table.cellWidget(i, j):
                    self.__widget_table.cellWidget(i, j).set_icon(self.__draw_success_icons,
                                                                  self.__draw_failure_icons)

    def set_current_cell(self, item):
        if item:
            self.__current_cell_row = item.row()
            self.__current_cell_col = item.column()
        else:
            self.__current_cell_row = None
            self.__current_cell_col = None

    def cell_right_clicked(self, point):
        self.set_current_cell(self.__widget_table.itemAt(point))
        self.try_switch_to_error_tab()

    def cell_left_clicked(self, row, column):
        self.set_current_cell(self.__widget_table.item(row, column))

    def try_switch_to_error_tab(self):
        if self.__current_cell_row is not None and self.__current_cell_col is not None:
            result_data = self.__table_data[self.__current_cell_row][self.__current_cell_col]
            if not result_data["match"]:
                mainwindow = self.__widget_resultwindow.widget_mainwindow
                mainwindow.error_window_set_result_data(result_data)
                mainwindow.switch_to_tab(TabState.Error)
