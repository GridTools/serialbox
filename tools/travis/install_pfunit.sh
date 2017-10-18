#!/usr/bin/env bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# @brief Install the pFUnit 3
#
# @param $1   Install directory
# @param $2   pFUnit version triple (X.Y.Z)
function install_pfunit() {
  pushd $(pwd)
  local start_time=$(date +%s)

  if [[ $# -lt 2 ]]; then
    fatal_error "argument mistmatch: ${FUNCNAME[0]} <install_prefix> <version>"
  fi

  local install_dir=$1
  shift
  local pfunit_version=$1
  shift
  
  local pfunit_install_dir=$install_dir/pfunit-$pfunit_version

  echo "$FC_COMPILER"

  # No fortran compiler available
  if [ -z ${FC_COMPILER+x} ]; then
    NOTICE "${FUNCNAME[0]}: No Fortran compiler set (FC_COMPILER is empty). Skipping."
    return 0
  fi

  abort_and_cleanup() {
    rm -rf "$pfunit_install_dir" && mkdir -p "$pfunit_install_dir" 
    fatal_error "$1"
  }

  NOTICE "${FUNCNAME[0]}: Installing pFUnit $pfunit_version into \"$pfunit_install_dir\" ..."
  mkdir -p "${pfunit_install_dir}"

  if [[ ! -z "$(ls -A ${pfunit_install_dir})" ]]; then
    NOTICE "${FUNCNAME[0]}: Package already installed. Skipping."
  else
    local pfunit_url="https://github.com/laristra/pfunit/archive/${pfunit_version}.tar.gz"

    NOTICE "${FUNCNAME[0]}: Downloading pFUnit $pfunit_url ..."
    { wget --no-check-certificate -O - ${pfunit_url} |                                             \
      tar --strip-components=1 -xz -C ${pfunit_install_dir}; } ||                                  \
      abort_and_cleanup "Failed to download pFUnit from: $pfunit_url"
    NOTICE "${FUNCNAME[0]}: Successfully downloaded $pfunit_url"

    cd ${pfunit_install_dir}
    NOTICE "${FUNCNAME[0]}: Starting to build pFUnit ..."
    mkdir build && cd build
    export FC="$FC_COMPILER"
    cmake .. -DCMAKE_BUILD_TYPE=Release                                                            \
             -DCMAKE_INSTALL_PREFIX="${pfunit_install_dir}"                                        \
             -DINSTALL_PATH="${pfunit_install_dir}"                                                \
             -DCMAKE_Fortran_COMPILER="$FC_COMPILER"                                               \
          || abort_and_cleanup "Failed to configure pFUnit"
    make -j2 install || abort_and_cleanup "Failed to build pFUnit"
  fi
  
  local elapsed_time=$(expr $(date +%s) - $start_time)
  NOTICE $(printf "${FUNCNAME[0]}: Successfully installed pFUnit $pfunit_version (took %dm %ds)\n" \
           $(($elapsed_time%3600/60)) $(($elapsed_time%60)))

  export PFUNIT="${pfunit_install_dir}"
  export PFUNIT_ROOT="${pfunit_install_dir}"
  popd
}