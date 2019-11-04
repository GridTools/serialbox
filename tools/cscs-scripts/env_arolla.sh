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

module load craype-x86-skylake
module load craype-network-infiniband
module load slurm

if [ "$FC_COMPILER" = "pgfortran" ]; then
    echo "not defined"
    exit 1
    # module swap PrgEnv-cray PrgEnv-pgi
    # module load gcc
elif [ "$FC_COMPILER" = "ftn" ]; then
    echo "not defined"
    exit 1
    # module load gcc
elif [ "$FC_COMPILER" = "ifort" ]; then
    echo "not defined"
    exit 1
    # module swap PrgEnv-cray PrgEnv-intel
    # module load gcc/7.3.0
elif [ "$FC_COMPILER" = "pgfortran18.10" ]; then
    echo "not defined"
    exit 1
    # module use /project/c14/data-eniac/modulefiles
    # module swap PrgEnv-cray PrgEnv-pgi
    # module load eniac/pgi-18.10
    # module load gcc
    # FC_COMPILER="pgfortran"
else
    module load PrgEnv-gnu
    module swap gcc gcc/5.3.0
fi

module load boost/1.70.0-gmvolf-18.12-python2
module load netcdf/4.6.3-gmvolf-18.12
module load hdf5/1.10.5-gmvolf-18.12
export NETCDF_ROOT=${EBROOTNETCDF}

export CXX=$(which g++)
export CC=$(which gcc)
export FC=$(which $FC_COMPILER)

export Boost_NO_SYSTEM_PATHS=true
export Boost_NO_BOOST_CMAKE=true

# export BOOST_ROOT=/project/c14/install/daint/boost/boost_1_67_0
# export BOOST_INCLUDE={BOOST_ROOT}/include/
# export LD_LIBRARY_PATH=${BOOST_ROOT}/lib:$LD_LIBRARY_PATH
