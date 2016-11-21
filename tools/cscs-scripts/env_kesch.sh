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
    
    printf "  %-35s %s\n" \
           "-f, --fc-compiler [gnu|cray|pgi]" \
           "Select Fortran compiler [default: gnu]." 
   
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
ENV_ARGS=$(getopt -o t:c: -l target:,compiler: -n 'env_kesch' -- "$@");

if [ $? -ne 0 ]; then
  exit 1
fi

eval set -- "$ENV_ARGS"

while true; do 
    case "$1" in
        -h|--h*) print_help; exit 0;;
        -t|--target) ARG_TARGET=$(to_lower_and_trim $2); shift 2;;
        -c|--compiler) ARG_COMPILER=$(to_lower_and_trim $2); shift 2;;
        --) shift; break ;;
        *) echo "$0: internal error." ; exit 1 ;;
    esac
done

# Target
if [ "${ARG_TARGET}" = "gpu" ]; then
    ENV_CUDA=true
else
    ENV_CUDA=false
fi

# Compiler
if [ "${ARG_COMPILER}" = "gcc-4.9" ] || [ "${ENV_CUDA}" = "true" ]; then
    ENV_COMPILER="gcc49"
else
    ENV_COMPILER="gcc53"
fi

#------------------------------ Set environment --------------------------------

module load CMake/3.3.2

if [ "$ENV_COMPILER" = "gcc49" ]; then
    module load PrgEnv-gnu
    export BOOST_ROOT=/scratch/cosuna/software/boost_1_59_0/
    export BOOST_INCLUDE=/scratch/cosuna/software/boost_1_59_0/include/
    export SERIALBOX_ROOT=/scratch/thfabian/serialbox/install/gcc49
else
    module load GCC/5.3.0-binutils-2.25
    export BOOST_ROOT=/scratch/cosuna/software/boost_1_59_0_gcc5.3/
    export BOOST_INCLUDE=/scratch/cosuna/software/boost_1_59_0_gcc5.3/include/
    export SERIALBOX_ROOT=/scratch/thfabian/serialbox/install/gcc53
fi

if [ "$ENV_CUDA" = "true" ]; then
    module load cudatoolkit/7.0.28
fi

export CXX=$(which g++)
export CC=$(which gcc)
export FC=$(which gfortran)

export Boost_NO_SYSTEM_PATHS=true
export Boost_NO_BOOST_CMAKE=true
export CUDATOOLKIT_HOME=${CUDA_PATH}
export CUDA_ARCH=sm_37

export GRIDTOOLS_ROOT=/scratch/thfabian/gridtools
