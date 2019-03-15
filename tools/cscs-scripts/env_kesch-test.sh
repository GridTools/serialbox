#!/bin/bash
##===-----------------------------------------------------------*- bash -*-===##
##
##                           S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===----------------------------------------------------------------------===##

## Print help statement and exit.
print_help()
{
    printf "Usage: $0 [options]\n\n"
    printf "Options:\n"
    
    # --fc-compiler
    printf "  %-35s %s\n" \
           "-f, --fc-compiler FORTRAN_COMPILER" \
           "Fortran compiler to use (set to "
    printf "  %-35s %s\n" "" "environment variable FC)." 
   
    # --help
    printf "  %-35s %s\n" "-h, --help" "Print this help statement."
    printf "\n" 
    exit 0
}

## Convert to lower case and remove all whitespaces
to_lower_and_trim()
{
    if [ $# -ne 1 ]; then
        echo "$0: internal error." ; exit 1
    fi
    
    local to_lower=$(echo "$1" | tr '[:upper:]' '[:lower:]')
    local trim_whitespaces=${to_lower// /}
    echo "${trim_whitespaces}"
}

#------------------------------ Parse options ----------------------------------
ENV_ARGS=$(getopt -o f:h:: -l fc-compiler,help:: -n 'env_kesch' -- "$@");

if [ $? -ne 0 ]; then
  exit 1
fi

eval set -- "$ENV_ARGS"

while true; do 
    case "$1" in
        -h|--h*) print_help; exit 0;;
        -f|--fc-compiler) ARG_FC_COMPILER=$(to_lower_and_trim $2); shift 2;;
        --) shift; break ;;
        *) echo "$0: internal error." ; exit 1 ;;
    esac
done

# Fortran Compiler
if [ -z ${ARG_FC_COMPILER+x} ]; then
  echo "$0: error: fortran compiler is not set"
  exit 1
else
  FC_COMPILER=${ARG_FC_COMPILER}
fi

#------------------------------ Set environment --------------------------------

module purge
module load PE/18.12
module load craype-network-infiniband
module load craype-haswell
module load gcc/5.4.0-2.26
module load cmake


if [ "$FC_COMPILER" = "pgfortran" ]; then
  module load PrgEnv-pgi/18.10
  module load netcdf-fortran/4.4.4-pgi-18.10-gcc-5.4.0-2.26
  export NETCDF_ROOT=/apps/escha/UES/jenkins/RH7.5/pgi_PE18.10/easybuild/software/netcdf-fortran/4.4.4-pgi-18.10-gcc-5.4.0-2.26/
elif [ "$FC_COMPILER" = "ftn" ]; then
  module load CrayCCE/.18.12
  module load netCDF-Fortran/4.4.4-CrayCCE-18.12
  export GCC_X86_64=$EBROOTGCCCORE
  export NETCDF_ROOT=/apps/escha/UES/jenkins/RH7.5/cray_PE18.12/easybuild/software/netCDF-Fortran/4.4.4-CrayCCE-18.12/
else
  module load PrgEnv-gnu
  module load netcdf-fortran/4.4.4-gmvolf-17.02
  export NETCDF_ROOT=/apps/escha/UES/jenkins/RH7.5/gnu_PE17.02/easybuild/software/netcdf/4.4.1.1-gmvolf-17.02
fi

export CXX=$(which g++)
export CC=$(which gcc)
export FC=$(which $FC_COMPILER)

export Boost_NO_SYSTEM_PATHS=true
export Boost_NO_BOOST_CMAKE=true

export BOOST_ROOT=/project/c14/install/kesch/boost/boost_1_67_0/
export BOOST_INCLUDE={BOOST_ROOT}/include/

