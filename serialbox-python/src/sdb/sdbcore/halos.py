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

class Halos(object):
    def __init__(self, halo_list):
        self.__halo_list = halo_list

    def is_null(self):
        if self.__halo_list:
            for halo_pair in self.__halo_list:
                if halo_pair[0] is not 0 or halo_pair[1] is not None:
                    return False
        return True

    def get_slice(self):
        slices = []
        for halo_pair in self.__halo_list:
            slices += [slice(halo_pair[0], None if halo_pair[1] is None else -halo_pair[1], None)]
        return tuple(slices)