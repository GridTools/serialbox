#!/usr/bin/env bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# @brief Install CMake
#
# @param $1   Install directory
# @param $2   CMake version triple (X.Y.Z)
function install_cmake() {
  pushd $(pwd)
  local start_time=$(date +%s)

  if [[ $# -lt 2 ]]; then
    fatal_error "argument mismatch: ${FUNCNAME[0]} <install_prefix> <version>"
  fi

  local install_dir=$1
  shift
  local cmake_version=$1
  shift
  
  local cmake_install_dir=$install_dir/cmake-$cmake_version
  local cmake_version_short=${cmake_version%.*}

  abort_and_cleanup() {
    rm -rf $cmake_install_dir && mkdir -p $cmake_install_dir 
    fatal_error "$1"
  }

  NOTICE "${FUNCNAME[0]}: Installing cmake $cmake_version into \"$cmake_install_dir\" ..."
  mkdir -p ${cmake_install_dir}
  if [[ ! -z "$(ls -A ${cmake_install_dir})" ]]; then
    NOTICE "${FUNCNAME[0]}: Package already installed. Skipping."
  else
    local cmake_url
    cmake_url=$(printf "https://cmake.org/files/v%s/cmake-%s-Linux-x86_64.tar.gz"                  \
                       ${cmake_version_short} ${cmake_version})

    NOTICE "${FUNCNAME[0]}: Downloading cmake $cmake_url ..."
    { wget --no-check-certificate -O - ${cmake_url} |                                              \
      tar --strip-components=1 -xz -C ${cmake_install_dir}; } ||                                   \
      abort_and_cleanup "Failed to download cmake from: $cmake_url"
    NOTICE "${FUNCNAME[0]}: Successfully downloaded $cmake_url"
  fi

  local elapsed_time=$(expr $(date +%s) - $start_time)
  NOTICE $(printf "${FUNCNAME[0]}: Successfully installed cmake $cmake_version (took %dm %ds)\n"   \
           $(($elapsed_time%3600/60)) $(($elapsed_time%60)))

  export PATH="${cmake_install_dir}/bin:${PATH}"
  popd
}