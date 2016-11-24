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

import os
import sys

sys.path.insert(1, os.path.join(os.path.dirname(os.path.realpath(__file__)),
                                "../../../src/serialbox-python"))

import serialbox as ser
import numpy as np


class StencilUVWT(object):
    def __init__(self, perturb=None):

        self.name = "StencilUVWT"
        self.invocation_count = 0
        self.perturb = 0 if perturb is None else perturb

        dir = "./" + self.name + ("" if perturb is None else "-error")
        self.serializer = ser.Serializer(ser.OpenModeKind.Write, dir, "stencil")
        self.serializer.global_metainfo.insert("stencils", ["StencilUVWT"])

        self.name = "StencilUVWT"
        self.invocation_count = 0

        np.random.seed(0)
        self.u = np.random.rand(32, 34, 80)
        self.v = np.random.rand(32, 34, 80)
        self.w = np.random.rand(32, 34, 80)

        self.t = np.random.rand(80)

    def run(self):
        self.stage_1()
        self.stage_2()

        self.invocation_count += 1

    def stage_1(self):
        self.serialize("in", 0, "stage_1", {"u": self.u, "v": self.v, "w": self.w})

        self.u += 1
        self.v += 2
        self.w += 3

        self.serialize("out", 0, "stage_1", {"u": self.u, "v": self.v, "w": self.w})

    def stage_2(self):
        self.serialize("in", 1, "stage_2", {"u": self.u, "v": self.v, "w": self.w, "t": self.t})

        self.u += 1
        self.v += 2
        self.w += 3 + self.perturb
        self.t += 4

        self.serialize("out", 1, "stage_2", {"u": self.u, "v": self.v, "w": self.w, "t": self.t})

    def serialize(self, intent, stage_id, stage_name, fields):
        sp = ser.Savepoint(self.name + "__" + intent)
        sp.metainfo.insert("stage_id", stage_id)
        sp.metainfo.insert("stage_name", stage_name)
        sp.metainfo.insert("invocation_count", self.invocation_count)

        for name, field in fields.items():
            self.serializer.write(name, sp, field)


if __name__ == '__main__':
    #ser.Logging().enable()

    s_uvwt = StencilUVWT()
    s_uvwt.run()

    s_uvwt_error = StencilUVWT(1)
    s_uvwt_error.run()
