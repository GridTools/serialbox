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
##
## this file contains debugging utility.
##
##===------------------------------------------------------------------------------------------===##

#
# Check if numpy is available
#
try:
    import matplotlib

    del matplotlib
except ImportError:
    raise Exception("Serialbox visualizer requires matplotlib")

import numpy as np
from matplotlib import pyplot as plt
from matplotlib.colors import SymLogNorm, Normalize
from matplotlib.widgets import Slider, CheckButtons


class Visualizer:
    def __init__(self, field, fieldname, halospec=None):
        """Visualization a field in a matplotlib window

        Each k level is represented as a simple slice in the window.

        :param field: Field to display
        :type field: numpy.array
        :param fieldname: Name of the field to display
        :type fieldname: str
        :param halospec: 2x2 array with the definition of the halo size (e.g ``[[3, 3], [3, 3]]``)
        :type halospec: array[array]
        """
        self.__field = field
        self.__fieldname = fieldname

        # Register halo information
        if halospec is None:
            halospec = [[3, 3], [3, 3]]

        self.__istart = halospec[0][0]
        self.__iend = field.shape[0] - halospec[0][1]
        self.__jstart = halospec[1][0]
        self.__jend = field.shape[1] - halospec[1][1]
        self.__plotHalo = True
        self.__plotLogLog = False

        self.__curklevel = 0

        self.__figure = plt.figure()

        # Slider
        slideraxes = plt.axes([0.15, 0.02, 0.5, 0.03], axisbg='lightgoldenrodyellow')
        self.__slider = Slider(slideraxes, 'K level', 0, field.shape[2] - 1, valinit=0)
        self.__slider.valfmt = '%2d'
        self.__slider.set_val(0)
        self.__slider.on_changed(self.update_slider)

        # CheckButton
        self.__cbaxes = plt.axes([0.8, -.04, 0.12, 0.15])
        self.__cbaxes.set_axis_off()
        self.__cb = CheckButtons(self.__cbaxes, ('Halo', 'Logscale'),
                                 (self.__plotHalo, self.__plotLogLog))
        self.__cb.on_clicked(self.update_button)

        # Initial plot
        self.__fieldaxes = self.__figure.add_axes([0.1, 0.15, 0.9, 0.75])
        self.__collection = plt.pcolor(self._get_field(), axes=self.__fieldaxes)
        self.__colorbar = plt.colorbar()
        self.__fieldaxes.set_xlim(right=self._get_field().shape[1])
        self.__fieldaxes.set_ylim(top=self._get_field().shape[0])

        plt.xlabel('i')
        plt.ylabel('j')
        self.__title = plt.title('%s - Level 0' % (fieldname,))
        plt.show(block=True)

    def update_slider(self, val):
        if val == self.__curklevel:
            return
        self.__curklevel = round(val)
        self.__title.set_text('%s - Level %d' % (self.__fieldname, self.__curklevel))

        # Draw new field level
        field = self._get_field()
        size = field.shape[0] * field.shape[1]
        array = field.reshape(size)
        self.__collection.set_array(array)

        self.__colorbar.set_clim(vmin=field.min(), vmax=field.max())
        self.__collection.set_clim(vmin=field.min(), vmax=field.max())
        self.__colorbar.update_normal(self.__collection)
        self.__figure.canvas.draw_idle()

    def update_button(self, label):
        if label == 'Halo':
            self.__plotHalo = not self.__plotHalo
        if label == 'Logscale':
            self.__plotLogLog = not self.__plotLogLog
        self.update_plot()

    def update_plot(self):
        # Redraw field
        self.__collection.remove()
        field = self._get_field()

        if self.__plotLogLog:
            minvalue = field.min()
            norm = SymLogNorm(linthresh=1e-10)
            self.__collection = plt.pcolor(field, axes=self.__fieldaxes,
                                           norm=norm)
            self.__colorbar.set_clim(vmin=minvalue, vmax=field.max())
        else:
            self.__collection = plt.pcolor(field, axes=self.__fieldaxes)
            self.__colorbar.set_clim(vmin=field.min(), vmax=field.max())
            self.__colorbar.set_norm(norm=Normalize(vmin=field.min(), vmax=field.max()))

        self.__fieldaxes.set_xlim(right=field.shape[1])
        self.__fieldaxes.set_ylim(top=field.shape[0])

        self.__colorbar.update_normal(self.__collection)
        self.__figure.canvas.draw_idle()

    def _get_field(self):
        if self.__plotHalo:
            return np.rot90(self.__field[:, :, int(self.__curklevel)])
        else:
            return np.rot90(self.__field[self.__istart:self.__iend, self.__jstart:self.__jend,
                            int(self.__curklevel)])
