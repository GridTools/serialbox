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
           "-f, --fc-compiler [gnu|cray|pgi|intel]" \
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

#------------------------------ Identify CSCS host -----------------------------
if [ "$(hostname | grep greina)" != "" ] ; then
    MYHOST="greina"
elif [ "$(hostname | grep keschcn-0012)" != "" ]; then
    MYHOST="kesch-test"
elif [ "$(hostname | grep kesch)" != "" ] ; then
    MYHOST="kesch"
elif [ "$(hostname | grep daint)" != "" ] ; then
    MYHOST="daint"
elif [ "$(hostname | grep arolla)" != "" ] ; then
    MYHOST="tsa"
elif [ "$(hostname | grep tsa)" != "" ] ; then
    MYHOST="tsa"
elif [ "$(hostname | grep tave)" != "" ] ; then
    MYHOST="tave"
else
    echo "build: host '$(hostname)' not known. Assuming environment is already setup."
fi

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

# Fortran Compiler
if [ "${ARG_FC_COMPILER}" = "cray" ]; then
    printf "%-20s: %s\n" "Fortran compiler" "cray"
    FC_COMPILER="ftn"
elif [ "${ARG_FC_COMPILER}" = "pgi" ]; then
    printf "%-20s: %s\n" "Fortran compiler" "pgi"
    FC_COMPILER="pgfortran"
elif [ "${ARG_FC_COMPILER}" = "pgi18.10" ]; then
    printf "%-20s: %s\n" "Fortran compiler" "pgi"
    FC_COMPILER="pgfortran18.10"
elif [ "${ARG_FC_COMPILER}" = "intel" ]; then
    printf "%-20s: %s\n" "Fortran compiler" "intel"
    FC_COMPILER="ifort"
else
    printf "%-20s: %s\n" "Fortran compiler" "gnu"
    FC_COMPILER="gfortran"
fi

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

CURRENT_PATH=$(pwd)

#------------------------------ Load environment -------------------------------
if [ -n ${MYHOST} ]; then
    source ${CURRENT_PATH}/env_${MYHOST}.sh -f ${FC_COMPILER}
fi

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
if [ ! -z ${PFUNIT_INSTALL_DIR+x} ]; then
    SERIALBOX_TESTING_FORTRAN=ON
    SET_PFUNIT_PATH="-DpFUnit_DIR=${PFUNIT_INSTALL_DIR}"
    PFUNIT=${PFUNIT_INSTALL_DIR}
else
    SERIALBOX_TESTING_FORTRAN=OFF
fi

#NetCDF
SERIALBOX_USE_NETCDF=OFF
if [ "${FC_COMPILER}" = "gfortran" ] && [ "$MYHOST" != "tsa" ]; then
    SERIALBOX_USE_NETCDF=ON
fi


#------------------------------ Build ------------------------------------------

ROOT_DIR=${CURRENT_PATH}/../..
BUILD_DIR=${ROOT_DIR}/build_gcc_${ARG_FC_COMPILER}

# Create build directory
if [ -d "$BUILD_DIR" ]; then
    if [ "$REBUILD" = "true" ]; then
        rm -r ${BUILD_DIR}
    fi
fi

mkdir -p ${BUILD_DIR}

cd ${BUILD_DIR}

if [ ! -z "${INSTALL_PREFIX}" ]; then
    CMAKE_INSTALL_PREFIX=${INSTALL_PREFIX}
else
    CMAKE_INSTALL_PREFIX=${BUILD_DIR}/install
fi

# Run Cmake
cmake                                                                          \
 -DBoost_NO_BOOST_CMAKE="true"                                                 \
 -DCMAKE_INSTALL_PREFIX:STRING=${CMAKE_INSTALL_PREFIX}                         \
 -DCMAKE_BUILD_TYPE:STRING="$BUILD_TYPE"                                       \
 -DSERIALBOX_TESTING:BOOL=${SERIALBOX_TESTING}                                 \
 -DSERIALBOX_ENABLE_C:BOOL=${SERIALBOX_ENABLE_C}                               \
 -DSERIALBOX_ENABLE_FORTRAN:BOOL=${SERIALBOX_ENABLE_FORTRAN}                   \
 -DSERIALBOX_TESTING_GRIDTOOLS:BOOL=${SERIALBOX_TESTING_GRIDTOOLS}             \
 -DSERIALBOX_TESTING_STELLA:BOOL=${SERIALBOX_TESTING_STELLA}                   \
 -DSERIALBOX_TESTING_FORTRAN:BOOL=${SERIALBOX_TESTING_FORTRAN}                 \
 -DSERIALBOX_USE_NETCDF:BOOL=${SERIALBOX_USE_NETCDF}                           \
 -DSERIALBOX_ENABLE_EXPERIMENTAL_FILESYSTEM:BOOL=ON                            \
 -DSERIALBOX_ENABLE_FTG:BOOL=ON                                                \
 -DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=ON                                         \
 ${SET_PFUNIT_PATH}                                                            \
 ../

# make and install
nice make install -j8

ret=$?
if [ ${ret} -ne 0 ]; then
    exit ${ret}
fi


if [ "$ARG_RUN_TESTS" == "true" ]; then
    # Run tests
    chmod  +x run_tests.sh
    if [ "$MYHOST" == "tave" ]; then
        srun --account=c14 ./run_tests.sh
    else
        ./run_tests.sh
    fi
    ret=$?
    if [ ${ret} -ne 0 ]; then
        exit ${ret}
    fi
    
    # build the standalone example (tests installation)
    rel_path_to_example="examples/fortran/perturbation"
    mkdir -p ${BUILD_DIR}/${rel_path_to_example}
    cd ${BUILD_DIR}/${rel_path_to_example}
    cmake -DSerialbox_DIR=${CMAKE_INSTALL_PREFIX}/cmake          \
        -DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=ON              \
        ${ROOT_DIR}/${rel_path_to_example}
    nice make -j4
    if [ "$MYHOST" == "tave" ]; then
        srun --account=c14 ${ROOT_DIR}/${rel_path_to_example}/run.sh
    else
	    ${ROOT_DIR}/${rel_path_to_example}/run.sh
    fi
    ret=$?
    if [ ${ret} -ne 0 ]; then
        exit ${ret}
    fi
    
    if [ "$SERIALBOX_USE_NETCDF" == "ON" ]; then
        rel_path_to_example="examples/fortran/perturbation_netcdf"
        mkdir -p ${BUILD_DIR}/${rel_path_to_example}
        cd ${BUILD_DIR}/${rel_path_to_example}
        cmake -DSerialbox_DIR=${CMAKE_INSTALL_PREFIX}/cmake          \
            -DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=ON              \
            ${ROOT_DIR}/${rel_path_to_example}
        nice make -j4
        if [ "$MYHOST" == "tave" ]; then
	        srun --account=c14 ${ROOT_DIR}/${rel_path_to_example}/run.sh
	    else
		    ${ROOT_DIR}/${rel_path_to_example}/run.sh
	    fi
        ret=$?
        if [ ${ret} -ne 0 ]; then
            exit ${ret}
        fi
    fi
fi

