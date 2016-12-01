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
                             QCheckBox, QTableWidgetItem, QComboBox)

from sdbcore.logger import Logger
from .resulttablecellwidget import ResultTableCellWidget
from .tabstate import TabState


def make_unqiue_and_preserve_order(seq):
    seen = set()
    seen_add = seen.add
    return [x for x in seq if not (x in seen or seen_add(x))]


def make_shared_name(input_field, reference_field):
    return "%s  (%s)" % (input_field, reference_field)


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
        self.__last_cell_row = None
        self.__last_cell_row = None

        self.__current_invocation_count = 0

        # Widgets
        self.__widget_resultwindow = resultwindow
        self.__widget_table = QTableWidget(self)
        self.__currently_processing_custom_context_menu_request = False

        self.__widget_label_title = QLabel("", parent=self)

        self.__widget_label_invocation_count = QLabel("Invocation count: ", parent=self)
        self.__widget_label_invocation_count.setStatusTip("Select the invocation of the stencil")
        self.__widget_combobox_invocation_count = QComboBox(self)
        self.__widget_combobox_invocation_count.currentIndexChanged.connect(
            self.set_invocation_count)

        self.__widget_checkbox_draw_success = QCheckBox(self)
        self.__widget_checkbox_draw_success.setIcon(QIcon("sdbgui/images/success.png"))
        self.__widget_checkbox_draw_success.setChecked(True)
        self.__widget_checkbox_draw_success.stateChanged[int].connect(self.set_draw_success)
        self.__widget_checkbox_draw_success.setStatusTip("Show success icons")

        self.__widget_checkbox_draw_failure = QCheckBox(self)
        self.__widget_checkbox_draw_failure.setIcon(QIcon("sdbgui/images/failure-small.png"))
        self.__widget_checkbox_draw_failure.setChecked(True)
        self.__widget_checkbox_draw_failure.stateChanged[int].connect(self.set_draw_failure)
        self.__widget_checkbox_draw_failure.setStatusTip("Show failure icons")

        self.__widget_label_result = QLabel("", parent=self)
        self.__widget_label_result_icon = QLabel("", parent=self)

        self.__widget_label_loading = QLabel("", parent=self)

        vbox = QVBoxLayout()

        hbox_top = QHBoxLayout()
        hbox_top.addWidget(self.__widget_label_title)
        hbox_top.addStretch(1)
        hbox_top.addWidget(self.__widget_checkbox_draw_success)
        hbox_top.addWidget(self.__widget_checkbox_draw_failure)
        vbox.addLayout(hbox_top)

        hbox_middle = QHBoxLayout()
        hbox_middle.addWidget(self.__widget_label_invocation_count)
        hbox_middle.addWidget(self.__widget_combobox_invocation_count)
        hbox_middle.addStretch(1)
        vbox.addLayout(hbox_middle)

        vbox.addWidget(self.__widget_table)

        hbox_bottom = QHBoxLayout()
        hbox_bottom.addWidget(self.__widget_label_result)
        hbox_bottom.addWidget(self.__widget_label_result_icon)
        hbox_bottom.addStretch(1)
        hbox_bottom.addWidget(self.__widget_label_loading)

        vbox.addLayout(hbox_bottom)

        self.setLayout(vbox)

    def make_update(self):
        Logger.info("Updating ResultTableWidget")

        self.__comparison_result_list = self.__stencil_field_mapper.comparison_result_list

        self.__widget_label_title.setText(
            "<b>%s</b>" % self.__comparison_result_list.shared_stencil_name())

        # Set current invocation count
        if self.__current_invocation_count >= self.__comparison_result_list.invocation_count():
            self.__current_invocation_count = 0

        num_errors = 0

        # Set invocation count widgets and compute errors
        if self.__comparison_result_list.invocation_count() >= 1:
            self.__widget_combobox_invocation_count.clear()
            for i in range(self.__comparison_result_list.invocation_count()):
                self.__widget_combobox_invocation_count.addItem("%i" % i)

                for result in self.__comparison_result_list.results(i):
                    num_errors += not result["match"]
        else:
            # No comparison found, roll back
            self.__widget_resultwindow.widget_mainwindow.popup_error_box(
                "<b>No valid comparisons</b><br/>No valid comparison were "
                "computed for the selected stencil pair.")
            self.__widget_resultwindow.make_back()

        self.__widget_label_invocation_count.setEnabled(
            self.__comparison_result_list.invocation_count() > 1)
        self.__widget_combobox_invocation_count.setEnabled(
            self.__comparison_result_list.invocation_count() > 1)

        # Update the table
        self.update_table()

        # Set bottom message and display a funny gif ;)
        if num_errors != 0:
            self.__widget_label_result_icon.clear()
            self.__widget_label_result.setText(
                "<b>%s error%s detected</b>" % (num_errors, "s" if num_errors > 1 else ""))
            self.__widget_label_result.setStyleSheet("QLabel {color: #B72424}")
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

    def set_invocation_count(self, idx):
        if idx < 0:
            self.__current_invocation_count = 0
        else:
            self.__current_invocation_count = int(
                self.__widget_combobox_invocation_count.itemText(idx))
            self.update_table()

    def update_table(self):

        # Compute stages and fields
        stages = []
        fields = []
        fields_tooltip = []
        num_errors = 0
        first_error_cell = None

        for result in self.__comparison_result_list.results(self.__current_invocation_count):
            stages += [make_stage_name(result)]

            input_field = result["input_field_name"]
            reference_field = result["reference_field_name"]

            if input_field == reference_field:
                fields += [input_field]
                fields_tooltip += ["Field: \"%s\"" % input_field]
            else:
                fields += [make_shared_name(input_field, reference_field)]
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
        for result in self.__comparison_result_list.results(self.__current_invocation_count):
            stage_idx = stages.index(make_stage_name(result))

            input_field_name = result["input_field_name"]

            if input_field_name in fields:
                field_idx = fields.index(input_field_name)
            else:
                field_idx = fields.index(
                    make_shared_name(input_field_name, result["reference_field_name"]))

            # Widget
            cell = ResultTableCellWidget(result["match"])
            cell.set_icon(self.__draw_success_icons, self.__draw_failure_icons)
            self.__widget_table.setCellWidget(field_idx, stage_idx, cell)

            # Save the first error for selection
            if not result["match"] and not first_error_cell:
                first_error_cell = [field_idx, stage_idx]

            # Item
            cell_item = QTableWidgetItem("")
            self.__widget_table.setItem(field_idx, stage_idx, cell_item)

            # Data
            self.__table_data[field_idx][stage_idx] = result

        # Emulate "left" click on first error
        if num_errors != 0:
            self.__widget_table.setCurrentCell(first_error_cell[0], first_error_cell[1])
            self.cell_left_clicked(first_error_cell[0], first_error_cell[1])

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
        Logger.info("Attempting to swtich to Error tab")
        cur_row = self.__current_cell_row
        cur_col = self.__current_cell_col

        if cur_row is not None and cur_col is not None:

            result_data = self.__table_data[cur_row][cur_col]

            if not result_data["match"]:

                mainwindow = self.__widget_resultwindow.widget_mainwindow

                # Check if dimensions match, if not display an error message and abort
                if not result_data.shapes_match():

                    # We only display the error message that the dimensions mismatch once. If we
                    # don't do this it will popup two error message.. don't ask me why :(
                    if self.__last_cell_row == cur_row and self.__last_cell_col == cur_col:
                        return False

                    self.__last_cell_row = cur_row
                    self.__last_cell_col = cur_col

                    errmsg = "<b>Dimension mismatch</b><br/>"
                    errmsg += "Input '%s': %s<br/>" % (
                        result_data["input_field_name"], result_data.input_shape)
                    errmsg += "Reference '%s': %s" % (
                        result_data["reference_field_name"], result_data.reference_shape)

                    mainwindow.popup_error_box(errmsg)
                    return False
                else:
                    mainwindow.error_window_set_result_data(result_data)
                    mainwindow.switch_to_tab(TabState.Error)
                    self.__last_cell_row = self.__last_cell_col = None
                    return True

        return False
