#!/bin/bash
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Runs clang format in the given directory
# Arguments:
#   $1 - Path to the source tree
#   $2 - Path to the clang format binary
#   $3 - Apply fixes (will raise an error if false and not there where changes)
#   $ARGN - Files to run clang format on
#

SOURCE_DIR=$1
shift 1
CLANG_FORMAT=$1
shift 1
APPLY_FIXES=$1
shift 1

# clang format will only find its configuration if we are in the source tree or in a path relative
# to the source tree
if [ "$APPLY_FIXES" == "1" ]; then
  $CLANG_FORMAT -style=file -i $@
else
  NUM_CORRECTIONS=`$CLANG_FORMAT -style=file -output-replacements-xml $@ | grep offset | wc -l`
  if [ "$NUM_CORRECTIONS" -gt "0" ]; then
    echo -e "\e[1m--------------------------------------------------------\e[0m"
    echo -e "\e[1mclang-format suggested changes, please run 'make format'\e[0m"
    echo -e "\e[1m--------------------------------------------------------\e[0m"
    exit 1
  fi
fi
