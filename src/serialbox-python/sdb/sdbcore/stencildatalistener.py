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


class StencilDataDataListener(object):
    """Listen any changes of the data the StencilData object.
    """

    def data_changed(self):
        """Data has changed.
        """
        raise NotImplementedError()


class StencilDataStencilListListener(object):
    """Listen to changes in the stencil list of the StencilData object.
    """

    def remove_all_stencils(self):
        raise NotImplementedError()

    def add_stencil(self, stencil):
        raise NotImplementedError()

    def remove_stencil(self, stencil):
        raise NotImplementedError()
