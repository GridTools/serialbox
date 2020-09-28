#!/bin/bash
#SBATCH --job-name=serialbox-unittest
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks-per-socket=1
#SBATCH --output=serialbox-unittest.out
#SBATCH --partition=debug
#SBATCH --time=00:2:00
#SBATCH --cpus-per-task=12
#SBATCH --distribution block:block

## Print help statement and exit.
print_help()
{
    printf "Usage: $0 [options]\n\n"
    printf "Options:\n"
    
    # --fc-compiler
    printf "  %-35s %s\n" \
           "-f, --fc-compiler [gnu|cray|pgi]" \
           "Select Fortran compiler [default: gnu]." 
   
    # --help
    printf "  %-35s %s\n" "-h, --help" "Print this help statement."
    printf "\n" 
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

#------------------------------ Parse options ----------------------------------
ENV_ARGS=$(getopt -o f:h:: -l fc-compiler,help:: -n \
           'submit.kesch.slurm' -- "$@");

if [ $? -ne 0 ]; then
  exit 1
fi

eval set -- "$ENV_ARGS"

while true; do 
    case "$1" in
        -h|--h*) print_help; exit 0;;
        -f|--fc-compiler) ARG_FC_COMPILER=$(to_lower_and_trim $2); shift 2;;
        --) shift; break ;;
        *) echo "$0: internal error." ; exit 1 ;;
    esac
done

# Fortran Compiler
if [ "${ARG_FC_COMPILER}" = "cray" ]; then
    printf "%-20s: %s\n" "Fortran compiler" "cray"
    FC_COMPILER="ftn"
elif [ "${ARG_FC_COMPILER}" = "pgi" ]; then
    printf "%-20s: %s\n" "Fortran compiler" "pgi"
    FC_COMPILER="pgfortran"
else
    printf "%-20s: %s\n" "Fortran compiler" "gnu"
    FC_COMPILER="gfortran"
fi

#------------------------------ Load environment -------------------------------

CURRENT_PATH=$(pwd)

# Load environment
source ${CURRENT_PATH}/env_kesch.sh -f ${FC_COMPILER}

#------------------------------ Run --------------------------------------------

RUN_SCRIPT=${CURRENT_PATH}/../../build_gcc_${ARG_FC_COMPILER}/run_tests.sh

printf "%-10s: %s\n" "Host" "$HOSTNAME"
printf "%-10s: %s\n" "Device" "CPU"
echo "---------------------------------------------"

srun -n 1 bash ${RUN_SCRIPT}

echo "---------------------------------------------"
echo "Parallel execution finished."

