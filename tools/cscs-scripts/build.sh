#!/usr/bin/env bash

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
    
    # --build-type
    printf "  %-35s %s\n" \
           "-b, --build-type [release|debug]" \
           "Set build type [default: release]."  
    
    # --rerun-cmake
    printf "  %-35s %s\n" \
           "-r, --rerun-cmake" "Delete build directory and rerun cmake."  

    # --install           
    printf "  %-35s %s\n" \
           "-i, --install [path]" \
           "Install to Serialbox in 'path'"
    printf "  %-35s %s\n" "" "[default: install/]"
            
           
    # --fc-compiler
    printf "  %-35s %s\n" \
           "-f, --fc-compiler [gnu|cray|pgi]" \
           "Select Fortran compiler [default: gnu]." 

    # --run-tests
    printf "  %-35s %s\n" \
           "-t, --run-tests]" \
           "Run tests."
   
    # --help
    printf "  %-35s %s\n" "-h, --help" "Print this help statement."
    
    printf "\nThe unittests for gridtools and stella will be built " 
    printf "automatically they are checked out in external/ or " 
    printf "GRIDTOOLS_ROOT and/or STELLA_ROOT are set.\n"
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

## Get source directory
get_source_dir()
{
    local scriptfile=${BASH_SOURCE[0]}
    if [ -z "${scriptfile}" ]; then
        scriptfile=$0
    fi
    local scriptdir=$(dirname "${scriptfile}")
    local source_dir=$(cd ${scriptdir}/../../; pwd)
    echo $source_dir
}

## Download and update a git repository
git_download () {
    repo=$1
    branch=$2
    dir=$3
    echo "------------------------------------------------------"
    echo "Downloading ${repo} (branch: ${branch}) into ${dir}"
    if [ ! -d "${dir}" ]; then
        git clone ${repo} ${dir}
        if [ "${branch}" != "master" ]; then
            git -C $dir checkout -t origin/$branch
        fi
    fi
    git -C $dir pull
}

#------------------------------ Identify CSCS host -----------------------------
if [ "$(hostname | grep greina)" != "" ] ; then
    MYHOST="greina"
elif [[ "$(hostname)" == "keschcn-"* ]]; then
    MYHOST=kesch-test
    module load python/3.6.2-gmvolf-17.02
    module load git
elif [ "`hostname | grep kesch`" != "" -o "`hostname | grep escha`" != "" ] ; then
    MYHOST=kesch
    module load Python/3.5.0-gmvolf-15.11
    module load git
elif [ "`hostname | grep daint`" != "" ]; then
    MYHOST=daint
    module load cray-python/17.09.1
    module load git/2.13.1
else
    echo "build: host '$(hostname)' not known. Assuming environment is already setup."
fi

#------------------------------ Build dependencies -----------------------------
APEPI_REPO=https://github.com/MeteoSwiss-APN/apepi.git
APEPI_BRANCH=master
APEPI_DIR=$(pwd)/apepi

ENVIRONMENT_REPO=https://github.com/MeteoSwiss-APN/environment.git
ENVIRONMENT_BRANCH=master
ENVIRONMENT_DIR=$(pwd)/environment

# Download   Repository        Branch              Target Directory
git_download $APEPI_REPO       $APEPI_BRANCH       $APEPI_DIR
git_download $ENVIRONMENT_REPO $ENVIRONMENT_BRANCH $ENVIRONMENT_DIR

export PATH=${APEPI_DIR}/bin:${ENVIRONMENT_DIR}:$PATH

#------------------------------ Parse options ----------------------------------
ARGS=$(getopt                                                                  \
       -o b:i:f:r::h::t::                                                      \
       -l build-type:,fc-compiler:,install:,rerun-cmake::,run-tests::,help::   \
       -n 'build' -- "$@");

if [ $? -ne 0 ]; then
  exit 1
fi

eval set -- "$ARGS"

while true; do 
    case "$1" in
        -h|--h*) print_help; exit 0;;
        -b|--build-type) ARG_BUILD=$(to_lower_and_trim $2); shift 2;;
        -i|--install) ARG_INSTALL=$2; shift 2;;
        -f|--fc-compiler) ARG_FC_COMPILER=$(to_lower_and_trim $2); shift 2;;
        -t|--run-tests ) ARG_RUN_TESTS=true; shift 2;;
        -r|--rerun-cmake) 			
            case "$2" in
                "") ARG_RERUN=true; shift 2;;
                *) ARG_RERUN=$2; shift 2;;
            esac ;;
        --) shift; break ;;
        *) echo "$0: internal error." ; exit 1 ;;
    esac
done
 
# Build type
if [ "${ARG_BUILD}" = "debug" ]; then
    printf "%-20s: %s\n" "Build type" "Debug"
    BUILD_TYPE=Debug
else
    printf "%-20s: %s\n" "Build type" "Release"
    BUILD_TYPE=Release
fi

# Install
if [ ! -z "${ARG_INSTALL}" ]; then
    INSTALL_PREFIX="${ARG_INSTALL}"
    printf "%-20s: %s\n" "Install directory" "${INSTALL_PREFIX}"
fi

# Setup environment
ENV=$(getenv -e $ARG_FC_COMPILER)

# Rebuild
if [ "${ARG_RERUN}" = "true" ]; then
    printf "%-20s: %s\n" "Rebuilding" "ON"
    REBUILD=true
else
    printf "%-20s: %s\n" "Rebuilding" "OFF"
    REBUILD=false
fi

SERIALBOX_TESTING=ON
SERIALBOX_ENABLE_C=ON
SERIALBOX_ENABLE_FORTRAN=ON

#------------------------------ Check for external libraries -------------------

EXTERNAL_DIR=${CURRENT_PATH}/../../external

# Gridtools
if [ ! -z ${GRIDTOOLS_ROOT+x} ]; then
    SERIALBOX_TESTING_GRIDTOOLS=ON
elif [ -d "${EXTERNAL_DIR}/gridtools" ]; then
    SERIALBOX_TESTING_GRIDTOOLS=ON
    export GRIDTOOLS_ROOT=${EXTERNAL_DIR}/gridtools
else
    SERIALBOX_TESTING_GRIDTOOLS=OFF
fi

# STELLA
if [ ! -z ${STELLA_ROOT+x} ]; then
    SERIALBOX_TESTING_STELLA=ON
elif [ -d "${EXTERNAL_DIR}/stella" ]; then
    SERIALBOX_TESTING_STELLA=ON
    export GRIDTOOLS_ROOT=${EXTERNAL_DIR}/stella
else
    SERIALBOX_TESTING_STELLA=OFF
fi

# pFUnit
if [ ! -z ${PFUNIT_ROOT+x} ]; then
    SERIALBOX_TESTING_FORTRAN=ON
elif [ -d "${EXTERNAL_DIR}/pfunit" ]; then
    SERIALBOX_TESTING_FORTRAN=ON
else
    SERIALBOX_TESTING_FORTRAN=OFF
fi

#NetCDF
if [ ! -z ${NETCDF_ROOT+x} ]; then
    SERIALBOX_USE_NETCDF=ON
else
    SERIALBOX_USE_NETCDF=OFF
fi


#------------------------------ Build ------------------------------------------

SOURCE_DIR=$(get_source_dir)
BUILD_DIR=$(pwd)/build_gcc_${ARG_FC_COMPILER}

echo "Source dir: ${SOURCE_DIR}"
echo "Build dir:  ${BUILD_DIR}"

if [ ! -z "${INSTALL_PREFIX}" ]; then
    CMAKE_INSTALL_PREFIX=${INSTALL_PREFIX}
else
    CMAKE_INSTALL_PREFIX=${BUILD_DIR}/install
fi

args=""
if [ "${REBUILD}" = "true" ]; then
    args="${args} -z"
fi

# Run Cmake
cmakebuild -e ${ENV} -s ${SOURCE_DIR} -b ${BUILD_DIR}                          \
    -c Boost_NO_BOOST_CMAKE="true"                                             \
       CMAKE_INSTALL_PREFIX:STRING=${CMAKE_INSTALL_PREFIX}                     \
       CMAKE_BUILD_TYPE:STRING="$BUILD_TYPE"                                   \
       SERIALBOX_TESTING:BOOL=${SERIALBOX_TESTING}                             \
       SERIALBOX_ENABLE_C:BOOL=${SERIALBOX_ENABLE_C}                           \
       SERIALBOX_ENABLE_FORTRAN:BOOL=${SERIALBOX_ENABLE_FORTRAN}               \
       SERIALBOX_TESTING_GRIDTOOLS:BOOL=${SERIALBOX_TESTING_GRIDTOOLS}         \
       SERIALBOX_TESTING_STELLA:BOOL=${SERIALBOX_TESTING_STELLA}               \
       SERIALBOX_TESTING_FORTRAN:BOOL=${SERIALBOX_TESTING_FORTRAN}             \
       SERIALBOX_USE_NETCDF:BOOL=${SERIALBOX_USE_NETCDF}                       \
    -m all install -j 1 $args

ret=$?
if [ ${ret} -ne 0 ]; then
    exit ${ret}
fi

cp "${ENV}" "${CMAKE_INSTALL_PREFIX}/modules.env"

# Run tests
if [ "$ARG_RUN_TESTS" == "true" ]; then
    pushd $BUILD_DIR &>/dev/null
        chmod  +x run_tests.sh
        envrun $ENV ./run_tests.sh
        ret=$?
        exit $ret
    popd &>/dev/null
fi

