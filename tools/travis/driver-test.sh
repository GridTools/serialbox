#!/usr/bin/env bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

this_script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Load cache
source "$this_script_dir/install.sh"

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then 
  if ! brew ls --version cmake &>/dev/null; then brew install cmake; fi
  if ! brew ls --version boost &>/dev/null; then brew install boost; fi

  brew update
  brew install python3
  pip3 install numpy nose
      
  brew tap homebrew/science
  brew install netcdf
  export PATH="/usr/local/bin:${PATH}"

else # Linux 
  install_driver -i ${CACHE_DIR} -b cmake,boost,pfunit -c boost:filesystem,chrono,system,log

  if [ ! -z ${CLANG_VERSION+x} ]; then
    install_driver -i ${CACHE_DIR} -b clang
  fi

  export SERIALBOX_PYTHON3_BIN=`which python3`
  export SERIALBOX_PYTHON3_PIP=`which pip3`
  "$SERIALBOX_PYTHON3_PIP" install nose numpy
fi
  
export CXX=${CXX_COMPILER}
export CC=${C_COMPILER}

$CC --version
$CXX --version

local cmake_fortran_options=""

# Build Serialbox2
if [[ "${FC_COMPILER}" != "" ]]; then
  export SERIALBOX_ENABLE_FORTRAN=ON
  export SERIALBOX_TESTING_FORTRAN=ON
  export FC=${FC_COMPILER}
else
  export SERIALBOX_ENABLE_FORTRAN=OFF
  export SERIALBOX_TESTING_FORTRAN=OFF
fi


if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
  pushd $(pwd)
  mkdir -p build && cd build
  
  cmake ..                                                                                         \
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}                                                     \
        -DSERIALBOX_TESTING=ON                                                                     \
        -DSERIALBOX_ENABLE_FORTRAN=OFF                                                             \
      || fatal_error "failed to configure cmake"
  make -j2 install || fatal_error "failed to build"
  
  popd
else # Linux 
  pushd $(pwd)
  mkdir -p build && cd build
  cmake ..                                                                                         \
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}                                                     \
        -DPYTHON_EXECUTABLE="$SERIALBOX_PYTHON3_BIN"                                               \
        -DSERIALBOX_TESTING=ON                                                                     \
        -DSERIALBOX_ENABLE_FORTRAN=$SERIALBOX_ENABLE_FORTRAN                                       \
        -DSERIALBOX_TESTING_FORTRAN=$SERIALBOX_TESTING_FORTRAN                                     \
        -DBOOST_ROOT="$BOOST_ROOT"                                                                 \
        -DpFUnit_DIR="$PFUNIT_ROOT"                                                               \
      || fatal_error "failed to configure cmake"
  make -j2 install || fatal_error "failed to build"

  popd

  if [[ "${FC_COMPILER}" != "" ]]; then
    pushd $(pwd)
    cd examples/fortran/perturbation
    mkdir -p build && cd build
    
    cmake ..                                                                                       \
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}                                                     \
        -DPYTHON_EXECUTABLE="$SERIALBOX_PYTHON3_BIN"                                                \
        -DBOOST_ROOT="$BOOST_ROOT"                                                                 \
        || fatal_error "failed to configure cmake"
    make || fatal_error "failed to build"
    
    popd
  fi
fi

# Run Python, C and C++ (and possibly Fortran) unittests
pushd $(pwd)
cd build

ctest -C ${CONFIG} --output-on-failure --force-new-ctest-process                                   \
     || fatal_error "failed to run unittests"

popd

# Run stand-alone Fortran example
if [[ "${FC_COMPILER}" != "" ]]; then
  pushd $(pwd)
  cd examples/fortran/perturbation/build
  bash run.sh || fatal_error "failed to run stand-alone Fortran example"
  popd
fi
