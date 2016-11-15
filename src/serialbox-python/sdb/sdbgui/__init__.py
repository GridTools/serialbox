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

from logging import info

from sdbcore.error import fatal_error
from sdbcore.logger import Logger

# Check if PyQt5 is available
try:
  from PyQt5.QtCore import QT_VERSION_STR
  Logger.info("importing PyQt5 (%s)" % QT_VERSION_STR)
except ImportError as e:
  fatal_error(e)
  
