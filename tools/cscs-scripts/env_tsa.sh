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
module load cmake/3.14.5

if [ "$FC_COMPILER" = "pgfortran" ]; then
    module load PrgEnv-pgi
    module load gcc
elif [ "$FC_COMPILER" = "ftn" ]; then
    echo "$FC_COMPILER not available on tsa"
    exit 1
elif [ "$FC_COMPILER" = "ifort" ]; then
    echo "$FC_COMPILER not available on tsa"
    exit 1
else
    module load PrgEnv-gnu
fi

export CXX=$(which g++)
export CC=$(which gcc)
export FC=$(which $FC_COMPILER)

export Boost_NO_SYSTEM_PATHS=true
export Boost_NO_BOOST_CMAKE=true

export BOOST_ROOT=/project/c14/install/tsa/boost/boost_1_67_0

