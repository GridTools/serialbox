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

# Setup dependencies
source "$this_script_dir/install.sh"
install_driver -i ${CACHE_DIR} -b ${PACKAGES_TO_INSTALL}

export CXX=${CXX_COMPILER}
export CC=${C_COMPILER}

$CC --version
$CXX --version

# Build dawn
pushd "$(pwd)"

mkdir build && cd build
cmake .. -DCMAKE_CXX_COMPILER="$CXX"                                                               \
         -DCMAKE_C_COMPILER="$CC"                                                                  \
         -DCMAKE_BUILD_TYPE="$CONFIG"                                                              \
         -DProtobuf_DIR="$Protobuf_DIR"                                                            \
      || fatal_error "failed to configure"
make -j2 || fatal_error "failed to build"

# Run unittests
ctest -C ${CONFIG} --output-on-failure --force-new-ctest-process                                   \
     || fatal_error "failed to run tests"

popd
