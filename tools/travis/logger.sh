#!/usr/bin/env bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# include guard
[ -n "${LOGGER_SH+x}" ] && return || readonly LOGGER_SH=1

LOGGER_APPNAME="logger"
LOGGER_VERSION=0.0.1

# log levels
readonly LOG_LEVEL_OFF=0        # none
readonly LOG_LEVEL_FATAL=100    # unusable, crash
readonly LOG_LEVEL_ERROR=200    # error conditions
readonly LOG_LEVEL_WARN=300     # warning conditions
readonly LOG_LEVEL_NOTICE=400   # Nothing serious, but notably nevertheless.
readonly LOG_LEVEL_INFO=500     # informational
readonly LOG_LEVEL_DEBUG=600    # debug-level messages
readonly LOG_LEVEL_TRACE=700    # see stack traces
readonly LOG_LEVEL_ALL=-1       # all enabled

# @brief Template based on a number between '@x@'
# 
# Meaning, "@a:b@" means: a is the string length and b is the selector or @a@ means: a is the 
# selector so, @1@ will return the timestamp and @5:1@ will return the timestamp of string length 5
# 
# The following selectors are defined:
#   1: timestamp
#   2: log level name
#   3: function name
#   4: line number
#   5: log message
#   6: space
LOGGER_DEFAULT_TEMPLATE="[@23:1@][@6:2@][@3@:@3:4@] @5@"  # default template

# log levels information
# level code, level name, level template, prefix(colors etc.), suffix(colors etc.)
LOG_LEVELS=(
  ${LOG_LEVEL_FATAL}  "FATAL"  "${LOGGER_DEFAULT_TEMPLATE}" "\e[41;37m" "\e[0m"
  ${LOG_LEVEL_ERROR}  "ERROR"  "${LOGGER_DEFAULT_TEMPLATE}" "\e[1;31m" "\e[0m"
  ${LOG_LEVEL_WARN}   "WARN"   "${LOGGER_DEFAULT_TEMPLATE}" "\e[1;33m" "\e[0m"
  ${LOG_LEVEL_NOTICE} "NOTICE" "${LOGGER_DEFAULT_TEMPLATE}" "\e[1;32m" "\e[0m"
  ${LOG_LEVEL_INFO}   "INFO"   "${LOGGER_DEFAULT_TEMPLATE}" "\e[37m" "\e[0m"
  ${LOG_LEVEL_DEBUG}  "DEBUG"  "${LOGGER_DEFAULT_TEMPLATE}" "\e[1;34m" "\e[0m"
  ${LOG_LEVEL_TRACE}  "TRACE"  "${LOGGER_DEFAULT_TEMPLATE}" "\e[94m" "\e[0m"
)

# log levels columns
readonly LOG_LEVELS_LEVEL=0
readonly LOG_LEVELS_NAME=1
readonly LOG_LEVELS_TEMPLATE=2
readonly LOG_LEVELS_PREFIX=3
readonly LOG_LEVELS_SUFFIX=4

LOG_LEVEL=${LOG_LEVEL_WARN}              # current log level
LOGGER_LOG_VIA_STDOUT=true               # log via stdout
LOGGER_LOG_VIA_FILE=""                   # file if logging via file (file, add suffix, add prefix)
LOGGER_LOG_VIA_FILE_PREFIX=false         # add prefix to log file
LOGGER_LOG_VIA_FILE_SUFFIX=false         # add suffix to log file
LOGGER_LOG_VIA_SYSLOG=""                 # syslog flags so that "syslog 'flags' message"
LOGGER_TS=""                             # timestamp variable
LOGGER_TS_FORMAT="%Y-%m-%d %H:%M:%S.%N"  # timestamp format
LOGGER_LOG_LEVEL_NAME=""                 # the name of the log level
LOGGER_LOG_MESSAGE=""                    # the log message

# @brief internal error message handler
#
# @param $1     Return code of a command etc.
# @param $2     Message when return code is 1
function logger_impl_err() {
  local return_code=${1:-0}
  local return_message=${2:=""}
  local prefix="\e[1;31m" # error color
  local suffix="\e[0m"    # error color
  if [ $return_code -eq 1 ]; then
    echo -e "${prefix}${return_message}${suffix}"
  fi
}

# @brief setup interface
function logger_impl(){
  local OPTIND=""
  
  # @brief prints the short usage of the script
  function print_usage() {
    echo ""
    echo "Usage: logger_impl [options]"
    echo "  -h, --help              Show usage"
    echo "  -V, --version           Version"
    echo "  -d, --date-format       Date format used in the log eg. '%Y-%m-%d %H:%M:%S.%N'"
    echo "  -o, --stdout            Log over stdout (true/false) default true."
    echo "  -f, --file              File to log to, none set means disabled"
    echo "  --file-prefix-enable    Enable the prefix for the log file"
    echo "  --file-prefix-disable   Disable the prefix for the log file"
    echo "  --file-suffix-enable    Enable the suffix for the log file"
    echo "  --file-suffix-disable   Disable the suffix for the log file"
    echo "  -s, --syslog            'switches you want to use'. None set means disabled"
    echo "                          results in: \"logger 'switches' log-message\""
    echo "  -l, --log-level         The log level"
    echo "                          Log levels       : value"
    echo "                          ---------------- : -----"
    echo "                          LOG_LEVEL_OFF    : ${LOG_LEVEL_OFF}"
    echo "                          LOG_LEVEL_FATAL  : ${LOG_LEVEL_FATAL}"
    echo "                          LOG_LEVEL_ERROR  : ${LOG_LEVEL_ERROR}"
    echo "                          LOG_LEVEL_WARN   : ${LOG_LEVEL_WARN}"
    echo "                          LOG_LEVEL_NOTICE : ${LOG_LEVEL_NOTICE}"
    echo "                          LOG_LEVEL_INFO   : ${LOG_LEVEL_INFO}"
    echo "                          LOG_LEVEL_DEBUG  : ${LOG_LEVEL_DEBUG}"
    echo "                          LOG_LEVEL_TRACE  : ${LOG_LEVEL_TRACE}"
    echo ""
  }

  for arg in "$@"; do # transform long options to short ones
    shift
    case "$arg" in
      "--help") set -- "$@" "-h" ;;
      "--version") set -- "$@" "-V" ;;
      "--log-level") set -- "$@" "-l" ;;
      "--date-format") set -- "$@" "-d" ;;
      "--stdout") set -- "$@" "-o" ;;
      "--file") set -- "$@" "-f" ;;
      "--file-prefix-enable") set -- "$@" "-a" "file-prefix-enable" ;;
      "--file-prefix-disable") set -- "$@" "-a" "file-prefix-disable" ;;
      "--file-suffix-enable") set -- "$@" "-a" "file-suffix-enable" ;;
      "--file-suffix-disable") set -- "$@" "-a" "file-suffix-disable" ;;
      "--syslog") set -- "$@" "-s" ;;
      *) set -- "$@" "$arg"
    esac
  done

  # get options
  while getopts "hVd:o:f:s:l:a:" optname
  do
    case "$optname" in
      "h")
        print_usage
        ;;
      "V")
        echo "${LOGGER_APPNAME} v${LOGGER_VERSION}"
        ;;
      "d")
        LOGGER_TS_FORMAT=${OPTARG}
        ;;
      "o")
        if [ "${OPTARG}" = true ]; then
          LOGGER_LOG_VIA_STDOUT=true
        else
          LOGGER_LOG_VIA_STDOUT=false
        fi
        ;;
      "f")
        LOGGER_LOG_VIA_FILE=${OPTARG}
        ;;
      "a")
        case ${OPTARG} in
          'file-prefix-enable' )
            LOGGER_LOG_VIA_FILE_PREFIX=true
            ;;
          'file-prefix-disable' )
            LOGGER_LOG_VIA_FILE_PREFIX=false
            ;;
          'file-suffix-enable' )
            LOGGER_LOG_VIA_FILE_SUFFIX=true
            ;;
          'file-suffix-disable' )
            LOGGER_LOG_VIA_FILE_SUFFIX=false
            ;;
          *)
            ;;
        esac
        ;;
      "s")
        LOGGER_LOG_VIA_SYSLOG=${OPTARG}
        ;;
      "l")
        LOG_LEVEL=${OPTARG}
        ;;
      *)
        logger_impl_err '1' "unknown error while processing logger_impl option."
        ;;
    esac
  done
  shift "$((OPTIND-1))" # shift out all the already processed options
}

# @brief get the log level information
#
# @param $1 log type
# @return information in the variables
#   - log level name
#   - log level template
#   ...
function logger_impl_get_log_level_info() {
  local log_level=${1:-"$LOG_LEVEL_ERROR"}
  LOG_FORMAT=""
  LOG_PREFIX=""
  LOG_SUFFIX=""
  local i=0
  for ((i=0; i<${#LOG_LEVELS[@]}; i+=$((LOG_LEVELS_SUFFIX+1)))); do
    if [[ "$log_level" == "${LOG_LEVELS[i]}" ]]; then
      LOGGER_LOG_LEVEL_NAME="${LOG_LEVELS[i+${LOG_LEVELS_NAME}]}"
      LOG_FORMAT="${LOG_LEVELS[i+${LOG_LEVELS_TEMPLATE}]}"
      LOG_PREFIX="${LOG_LEVELS[i+${LOG_LEVELS_PREFIX}]}"
      LOG_SUFFIX="${LOG_LEVELS[i+${LOG_LEVELS_SUFFIX}]}"
      return 0
    fi
  done
  return 1
}

# @brief converts the template to a usable string only call this after filling the global parameters
# @return fills a variable called 'LOGGER_CONVERTED_TEMPLATE_STRING'.
function logger_impl_convert_template() {
  local template=${*:-}
  local selector=0
  local str_length=0
  local to_replace=""
  local log_layout_part=""
  local found_pattern=true
  LOGGER_CONVERTED_TEMPLATE_STRING=""
  while $found_pattern ; do
    if [[ "${template}" =~ @[0-9]+@ ]]; then
      to_replace=${BASH_REMATCH[0]}
      selector=${to_replace:1:(${#to_replace}-2)}
    elif [[ "${template}" =~ @[0-9]+:[0-9]+@ ]]; then
      to_replace=${BASH_REMATCH[0]}
      if [[ "${to_replace}" =~ @[0-9]+: ]]; then
        str_length=${BASH_REMATCH[0]:1:(${#BASH_REMATCH[0]}-2)}
      else
        str_length=0
      fi
      if [[ "${to_replace}" =~ :[0-9]+@ ]]; then
        selector=${BASH_REMATCH[0]:1:(${#BASH_REMATCH[0]}-2)}
      fi
    else
      found_pattern=false
    fi

    case "$selector" in
      1) # timestamp
        log_layout_part="${LOGGER_TS}"
        ;;
      2) # log level name
        log_layout_part="${LOGGER_LOG_LEVEL_NAME}"
        ;;
      3) # function name
        log_layout_part="${FUNCNAME[2]}"
        ;;
      4) # line number
        log_layout_part="${BASH_LINENO[1]}"
        ;;
      5) # message
        log_layout_part="${LOGGER_LOG_MESSAGE}"
        ;;
      6) # space
        log_layout_part=" "
        ;;
      *)
        logger_impl_err '1' "unknown template parameter: '$selector'"
        log_layout_part=""
      ;;
    esac
    if [ ${str_length} -gt 0 ]; then # custom string length
      if [ ${str_length} -lt ${#log_layout_part} ]; then
        # smaller as string, truncate
        log_layout_part=${log_layout_part:0:str_length}
      elif [ ${str_length} -gt ${#log_layout_part} ]; then
        # bigger as string, append
        printf -v log_layout_part "%-0${str_length}s" $log_layout_part
      fi
    fi

    str_length=0 # set default
    template="${template/$to_replace/$log_layout_part}"
  done
  LOGGER_CONVERTED_TEMPLATE_STRING=${template}
  return 0
}

# @description
# @param $1 log type
# $2... the rest are messages
function logger_impl_message() {
  local file_directory=""
  local err_ret_code=0

  LOGGER_TS=$(date +"${LOGGER_TS_FORMAT}") # get the date
  log_level=${1:-"$LOG_LEVEL_ERROR"}
  if [ ${log_level} -gt ${LOG_LEVEL} ]; then # check log level
    if [ ! ${LOG_LEVEL} -eq ${LOG_LEVEL_ALL} ]; then # check log level
      return 0;
    fi
  fi

  # log level bigger as LOG_LEVEL? and level is not -1? return
  shift
  local message=${*:-}
  if [ -z "$message" ]; then # if message is empty, get from stdin
    message="$(cat /dev/stdin)"
  fi
  LOGGER_LOG_MESSAGE="${message}"
  logger_impl_get_log_level_info "${log_level}" || true
  logger_impl_convert_template ${LOG_FORMAT} || true

  # output to stdout
  if [ "${LOGGER_LOG_VIA_STDOUT}" = true ]; then
    echo -ne "$LOG_PREFIX"
    echo -ne "${LOGGER_CONVERTED_TEMPLATE_STRING}"
    echo -e "$LOG_SUFFIX"
  fi

  # output to file
  if [ ! -z "${LOGGER_LOG_VIA_FILE}" ]; then
    file_directory=$(dirname $LOGGER_LOG_VIA_FILE)
    if [ ! -z "${file_directory}" ]; then
      if [ ! -d "${LOGGER_LOG_VIA_FILE%/*}" ]; then # check directory
        # directory does not exist
        mkdir -p "${file_directory}" || err_ret_code=$?
        logger_impl_err "${err_ret_code}" "Error while making log directory: '${file_directory}'. Are the permissions ok?"
      fi
    fi
    if [ ! -e "${LOGGER_LOG_VIA_FILE}" ]; then # check file
      # file does not exist and making of folder went ok
      if [ $err_ret_code -ne 1 ]; then
        touch "${LOGGER_LOG_VIA_FILE}" || err_ret_code=$?
        logger_impl_err "${err_ret_code}" "Error while making log file: '${LOGGER_LOG_VIA_FILE}'. Are the permissions ok?"
      fi
    else
      message=""
      if [ "${LOGGER_LOG_VIA_FILE_PREFIX}" = true ]; then
        message="${message}${LOG_PREFIX}"
      fi
      message="${message}${LOGGER_CONVERTED_TEMPLATE_STRING}"
      if [ "${LOGGER_LOG_VIA_FILE_SUFFIX}" = true ]; then
        message="${message}${LOG_SUFFIX}"
      fi
      echo -e "${message}" >> ${LOGGER_LOG_VIA_FILE} || true
    fi
  fi
  # output to syslog
  if [ ! -z "${LOGGER_LOG_VIA_SYSLOG}" ]; then
      logger ${LOGGER_LOG_VIA_SYSLOG} "${LOGGER_CONVERTED_TEMPLATE_STRING}" || err_ret_code=$?
      logger_impl_err "${err_ret_code}" "Error while logging with syslog. Where these flags ok: '${LOGGER_LOG_VIA_SYSLOG}'"
  fi
}

# Setting of log level
function LOG_LEVEL_OFF()    { logger_impl --log-level ${LOG_LEVEL_OFF}    "$@"; }
function LOG_LEVEL_FATAL()  { logger_impl --log-level ${LOG_LEVEL_FATAL}  "$@"; }
function LOG_LEVEL_ERROR()  { logger_impl --log-level ${LOG_LEVEL_ERROR}  "$@"; }
function LOG_LEVEL_WARN()   { logger_impl --log-level ${LOG_LEVEL_WARN}   "$@"; }
function LOG_LEVEL_NOTICE() { logger_impl --log-level ${LOG_LEVEL_NOTICE} "$@"; }
function LOG_LEVEL_INFO()   { logger_impl --log-level ${LOG_LEVEL_INFO}   "$@"; }
function LOG_LEVEL_DEBUG()  { logger_impl --log-level ${LOG_LEVEL_DEBUG}  "$@"; }
function LOG_LEVEL_TRACE()  { logger_impl --log-level ${LOG_LEVEL_TRACE}  "$@"; }
function LOG_LEVEL_ALL()    { logger_impl --log-level ${LOG_LEVEL_ALL}    "$@"; }

# Log commands
function FATAL()    { logger_impl_message ${LOG_LEVEL_FATAL}  "$@"; }
function ERROR()    { logger_impl_message ${LOG_LEVEL_ERROR}  "$@"; }
function WARN()     { logger_impl_message ${LOG_LEVEL_WARN}   "$@"; }
function NOTICE()   { logger_impl_message ${LOG_LEVEL_NOTICE} "$@"; }
function INFO()     { logger_impl_message ${LOG_LEVEL_INFO}   "$@"; }
function DEBUG()    { logger_impl_message ${LOG_LEVEL_DEBUG}  "$@"; }
function TRACE()    { logger_impl_message ${LOG_LEVEL_TRACE}  "$@"; }
