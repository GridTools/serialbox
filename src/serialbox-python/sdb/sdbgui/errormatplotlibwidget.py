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

from PyQt5.QtCore import Qt, QSize
from PyQt5.QtGui import QPainter, QColor, QPen
from PyQt5.QtWidgets import (QSizePolicy, QTableWidget, QHeaderView, QTableWidgetItem, QWidget, QVBoxLayout,
                             QHBoxLayout, QLineEdit, QLabel, QFrame)

import matplotlib
matplotlib.use('Qt5Agg')

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure
from matplotlib.colors import ListedColormap

from numpy import arange, linspace

from sdbcore.logger import Logger

class ErrorMatplotlibWidget(FigureCanvas):
    def __init__(self, parent):
        fig = Figure(figsize=(5, 5), dpi=100)
        fig.patch.set_alpha(0)

        self.axes = fig.add_subplot(111)
        self.axes.hold(False)

        FigureCanvas.__init__(self, fig)
        self.setParent(parent)

        FigureCanvas.setSizePolicy(self, QSizePolicy.Preferred, QSizePolicy.Preferred)
        FigureCanvas.updateGeometry(self)

    def draw_layer(self, error_positions, layer):
        data = error_positions[:,:, layer]

        cmap = ListedColormap(['green', 'red'])
        self.axes.imshow(data, interpolation='nearest', origin='lower', cmap=cmap)

        self.axes.set_xticks(arange(-.5, data.shape[1] - 1, 1), minor=True)
        self.axes.set_yticks(arange(-.5, data.shape[0] - 1, 1), minor=True)

        self.axes.set_xticklabels([])
        self.axes.set_yticklabels([])

        self.axes.grid(True, which='minor')
        self.draw()