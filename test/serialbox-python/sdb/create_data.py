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

u = np.random.rand(4, 4, 2)
v = np.random.rand(4, 4, 2)
w = np.random.rand(4, 4, 2)
t = np.random.rand(2)

u1 = np.copy(u)
v1 = np.copy(v)
w1 = np.copy(w)
t1 = np.copy(t)
u1[2,2,1] = -1

# ===--------------------------------------------------------------------------------------------===
#   Fields: u,v,w,t 
#   Stencils: Coriolis, Diffusion
# ==---------------------------------------------------------------------------------------------===
serializer = ser.Serializer(ser.OpenModeKind.Write, "./test-uvwt", "stencil")
serializer.global_metainfo.insert("stencils", ["Coriolis", "Diffusion"])

sp_in = ser.Savepoint("Coriolis__in")
sp_in.metainfo.insert("stage_id", 0)
sp_in.metainfo.insert("stage_name", "CoriolisStage")
sp_in.metainfo.insert("invocation_count", 0)
serializer.write("u", sp_in, u)
#serializer.write("v", sp_in, v)
#serializer.write("w", sp_in, w)
#serializer.write("t", sp_in, t)

sp_out = ser.Savepoint("Coriolis__out")
sp_out.metainfo.insert("stage_id", 0)
sp_out.metainfo.insert("stage_name", "CoriolisStage")
sp_out.metainfo.insert("invocation_count", 0)
serializer.write("u", sp_out, u)
#serializer.write("v", sp_out, v)
#serializer.write("w", sp_out, w)
#serializer.write("t", sp_out, t)

# ===--------------------------------------------------------------------------------------------===
#   Fields: u,v,w,t 
#   Stencils: Coriolis, Diffusion
# ==---------------------------------------------------------------------------------------------===
serializer = ser.Serializer(ser.OpenModeKind.Write, "./test-uvwt-error", "stencil")
serializer.global_metainfo.insert("stencils", ["Coriolis", "Diffusion"])

sp_in = ser.Savepoint("Coriolis__in")
sp_in.metainfo.insert("stage_id", 0)
sp_in.metainfo.insert("stage_name", "CoriolisStage")
sp_in.metainfo.insert("invocation_count", 0)
serializer.write("u", sp_in, u)
serializer.write("v", sp_in, v)
serializer.write("w", sp_in, w)
serializer.write("t", sp_in, t)

sp_out = ser.Savepoint("Coriolis__out")
sp_out.metainfo.insert("stage_id", 0)
sp_out.metainfo.insert("stage_name", "CoriolisStage")
sp_out.metainfo.insert("invocation_count", 0)
serializer.write("u", sp_out, u1)
serializer.write("v", sp_out, v1)
serializer.write("w", sp_out, w1)
serializer.write("t", sp_out, t1)

