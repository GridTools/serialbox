#!/bin/bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
##
## Run Python Unittest.
##
## DO NOT MODIFY THIS FILE IT IS AUTO GENERATED!
##
##===------------------------------------------------------------------------------------------===##

export PYTHONPATH=$PYTHONPATH:${SERIALOBX_PYTHON_MODULE}

cd ${PYTHON_TEST_DIR}/serialbox
${PYTHON_EXECUTABLE} -m "nose"

