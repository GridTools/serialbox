#!/usr/bin/env bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# @brief Install the Boost libraries
#
# @param $1   Install directory
# @param $2   Boost version triple (X.Y.Z)
# @param $3   Boost components to build (',' separated)
function install_boost() {
  pushd $(pwd)
  local start_time=$(date +%s)

  if [[ $# -lt 3 ]]; then
    fatal_error "argument mistmatch: ${FUNCNAME[0]} <install_prefix> <version> <components...>"
  fi

  local install_dir=$1
  shift
  local boost_version=$1
  shift
  local boost_components=$1

  local boost_install_dir=$install_dir/boost-$boost_version
  local boost_version_underscore=${boost_version//\./_}

  abort_and_cleanup() {
    rm -rf "$boost_install_dir" && mkdir -p "$boost_install_dir" 
    fatal_error "$1"
  }

  NOTICE "${FUNCNAME[0]}: Installing boost $boost_version into \"$boost_install_dir\" ..."
  mkdir -p "${boost_install_dir}"

  if [[ ! -z "$(ls -A ${boost_install_dir})" ]]; then
    NOTICE "${FUNCNAME[0]}: Package already installed. Skipping."
  else
    local boost_url=$(printf "http://sourceforge.net/projects/boost/files/boost/%s/boost_%s.tar.gz"\
                      ${boost_version} ${boost_version_underscore})

    NOTICE "${FUNCNAME[0]}: Downloading boost $boost_url ..."
    { wget --no-check-certificate -O - ${boost_url} |                                              \
      tar --strip-components=1 -xz -C ${boost_install_dir}; } ||                                   \
      abort_and_cleanup "Failed to download boost from: $boost_url"
    NOTICE "${FUNCNAME[0]}: Successfully downloaded $boost_url"

    cd ${boost_install_dir}

    if [ "$(echo $CXX | grep clang++ -c)" = "1" ]; then
      local toolset="clang"
      local toolset_version=$($CXX --version | grep version |                                      \
                              sed "s/.*version \([0-9]*\.[0-9]*\.[0-9]*\).*/\1/")
    else
      local toolset="gcc"
      local toolset_version=$($CXX -dumpversion)
    fi

    NOTICE "${FUNCNAME[0]}: Building boost with toolset $toolset : $toolset_version ..."
    echo "using ${toolset} : ${toolset_version} : ${CXX} ;" > user-config.jam
    
    NOTICE "${FUNCNAME[0]}: Building components: $boost_components ..."
    IFS=',' read -r -a boost_components_split <<< "$boost_components"
    local boost_components_arg=""
    for component in $boost_components_split; do
      boost_components_arg="$boost_components_arg --with-$component"
    done

    NOTICE "${FUNCNAME[0]}: Starting to build boost ..."
    ./bootstrap.sh || abort_and_cleanup "Failed to configure boost"
    ./b2 -j2 --toolset=${toolset}-${toolset_version} --prefix=${boost_install_dir}                 \
             --user-config=${boost_install_dir}/user-config.jam                                    \
             ${boost_components_arg} install || abort_and_cleanup "Failed to build boost"
  fi
  
  local elapsed_time=$(expr $(date +%s) - $start_time)
  NOTICE $(printf "${FUNCNAME[0]}: Successfully installed boost $boost_version (took %dm %ds)\n"   \
           $(($elapsed_time%3600/60)) $(($elapsed_time%60)))

  export BOOST_ROOT="${boost_install_dir}"
  popd
}