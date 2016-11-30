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

from PyQt5.QtWidgets import QSizePolicy, QWidget, QLabel, QVBoxLayout

from sdbcore.logger import Logger

SDB_HAS_MATPLOTLIB = False

try:
    import matplotlib

    matplotlib.use('Qt5Agg')
    SDB_HAS_MATPLOTLIB = True
except ImportError as e:
    Logger.warning("cannot import matplotlib: %s" % e)

if SDB_HAS_MATPLOTLIB:

    from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
    from matplotlib.figure import Figure
    from matplotlib.colors import ListedColormap

    from numpy import arange, zeros


    class ErrorVisualizeMatplotlibWidget(FigureCanvas):
        def __init__(self, parent):
            fig = Figure(figsize=(5, 5), dpi=100)
            fig.patch.set_alpha(0)

            self.axes = fig.add_subplot(111)
            self.axes.hold(False)

            FigureCanvas.__init__(self, fig)
            self.setParent(parent)

            FigureCanvas.setSizePolicy(self, QSizePolicy.Expanding, QSizePolicy.Expanding)
            FigureCanvas.updateGeometry(self)

        def draw_layer(self, error_positions, layer):
            Logger.info("Drawing error positions at layer: %s" % layer)

            data = error_positions[:, :, layer] if error_positions.ndim == 3 else error_positions

            cmap = ListedColormap(['green', 'red'])
            self.axes.imshow(data, interpolation='nearest', origin='lower', cmap=cmap)

            self.axes.set_xticks(arange(-.5, data.shape[1] - 1, 1), minor=True)
            self.axes.set_yticks(arange(-.5, data.shape[0] - 1, 1), minor=True)

            self.axes.set_xlabel("j (%s)" % data.shape[1])
            self.axes.set_ylabel("i (%s)" % data.shape[0])

            self.axes.set_xticklabels([])
            self.axes.set_yticklabels([])

            self.axes.grid(True, which='minor')
            self.draw()

        def draw_nothing(self):
            Logger.info("Drawing nothing")

            cmap = ListedColormap(['white'])
            data = zeros((1,1))
            self.axes.imshow(data, interpolation='nearest', origin='lower', cmap=cmap)

            self.axes.set_xticklabels([])
            self.axes.set_yticklabels([])
            self.draw()
else:

    class ErrorVisualizeMatplotlibWidget(QWidget):
        def __init__(self, parent):
            super().__init__(parent)

            self.__widget_label = QLabel("matplotlib with 'Qt5Agg' backend is not available.")
            layout = QVBoxLayout()
            layout.addWidget(self.__widget_label)
            layout.addStretch(1)
            self.setLayout(layout)

        def draw_layer(self, error_positions, layer):
            pass
