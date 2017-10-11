#!/usr/bin/env bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# @brief Install Clang
#
# @param $1   Install directory
# @param $2   Clang version triple (X.Y.Z)
function install_clang() {
  pushd $(pwd)
  local start_time=$(date +%s)

  if [[ $# -lt 2 ]]; then
    fatal_error "argument mistmatch: ${FUNCNAME[0]} <install_prefix> <version>"
  fi

  local install_dir=$1
  shift
  local clang_version=$1
  shift
  
  local clang_install_dir=$install_dir/clang-$clang_version

  abort_and_cleanup() {
    rm -rf "$clang_install_dir" && mkdir -p "$clang_install_dir" 
    fatal_error "$1"
  }

  # $1 >= $2
  version_gte() {
    if [ "$2" = "$(echo -e "$1\n$2" | sort -V | head -n1)" ]; then
      echo "1"
    else
      echo "0"
    fi
  }

  NOTICE "${FUNCNAME[0]}: Installing clang $clang_version into \"$clang_install_dir\" ..."
  mkdir -p "${clang_install_dir}"

  if [[ ! -z "$(ls -A ${clang_install_dir})" ]]; then
    NOTICE "${FUNCNAME[0]}: Package already installed. Skipping."
  else
    # If clang >= 5.0.0 the URL is different
    if [ $(version_gte $clang_version 5.0.0) = "1" ]; then
      local clang_url=$(printf                                                                     \
            "http://releases.llvm.org/%s/clang+llvm-%s-linux-x86_64-ubuntu14.04.tar.xz"            \
            ${clang_version} ${clang_version})
    else
      local clang_url=$(printf                                                                     \
            "http://llvm.org/releases/%s/clang+llvm-%s-x86_64-linux-gnu-ubuntu-14.04.tar.xz"       \
            ${clang_version} ${clang_version})
    fi

    NOTICE "${FUNCNAME[0]}: Downloading clang+llvm $clang_url ..."
    { wget --no-check-certificate -O - ${clang_url} |                                              \
      tar --strip-components=1 -xJ -C ${clang_install_dir}; }                                      \
      || abort_and_cleanup "Failed to download clang+llvm from: $clang_url"
    NOTICE "${FUNCNAME[0]}: Successfully downloaded $clang_url"
  fi
  
  local elapsed_time=$(expr $(date +%s) - $start_time)
  NOTICE $(printf "${FUNCNAME[0]}: Successfully installed clang $clang_version (took %dm %ds)\n"   \
           $(($elapsed_time%3600/60)) $(($elapsed_time%60)))

  export PATH="${clang_install_dir}/bin:${PATH}"
  export LD_LIBRARY_PATH="${clang_install_dir}/lib:${LD_LIBRARY_PATH}"
  export C_COMPILER="${clang_install_dir}/bin/clang"
  export CXX_COMPILER="${clang_install_dir}/bin/clang++"
  popd
}