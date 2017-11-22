#!/bin/bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

ret=0

echo ''
echo 'Produce serialized data using netcdf'
echo ''
./fortran_producer_netcdf
ret=$((ret || $? ))

echo ''
echo 'Consume serialized data without pertubation using netcdf'
echo ''
./fortran_consumer_netcdf
ret=$((ret || $? ))

echo ''
echo 'Consume serialized data WITH pertubation using netcdf'
echo ''
./fortran_consumer_perturb_netcdf
ret=$((ret || $? ))

exit $ret
