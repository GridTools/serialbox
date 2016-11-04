#!/bin/bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

echo ''
echo 'Produce serialized data'
echo ''
./main_producer

echo ''
echo 'Consume serialized data without pertubation'
echo ''
./main_consumer

echo ''
echo 'Consume serialized data WITH pertubation'
echo ''
./main_consumer_perturb
