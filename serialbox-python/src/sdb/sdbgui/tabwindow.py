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

class TabWindow(object):
    """Represent a tab in the mainwindow.
    """

    def make_update(self):
        """Called upon switching to this tab.
        """
        raise NotImplementedError

    def make_back(self):
        """Switch to the previous tab.
        """
        raise NotImplementedError

    def make_continue(self):
        """Switch to the next tab.
        """
        raise NotImplementedError
