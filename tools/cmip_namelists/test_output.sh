#! /bin/bash

# Script to create and start 1-month NF1850[Esm], 2 degree runs with each
# output set defined by this tool.

mydir="$(cd $(dirname $0); pwd -P)"
camdir="$(dirname $(dirname ${mydir}))"
cimedir="$(dirname $(dirname ${camdir}))/cime"
cnewcase="${cimedir}/scripts/create_newcase"
cclone="${cimedir}/scripts/create_clone"
usermods_dirs="${camdir}/cime_config/usermods_dirs"
cfg_file="${mydir}/usermods_sets.cfg"
PROJECT=${PROJECT:-NN9560K}

# Track cases for cloning
declare -A case_reference=()

##
## Error output function (inputs: error code {1} and a message string)
##
perr() {
    local ecode
    ecode=${1}
    shift
    if [ ${ecode} -ne 0 ]; then
        echo -e "\nERROR (${ecode}): ${@}\n"
        if [ ${ecode} -lt 0 ]; then
            help ${ecode}
        else
            exit ${ecode}
        fi
    fi
}

if [ ! -f "${cnewcase}" ]; then
    perr 1 "Cannot find create_newcase in ${cimedir}"
fi

host="$(hostname)"
if [ "${host:0:4}" == "uan0" ]; then
    mach=olivia
    workdir=/cluster/work/projects/nn9560k/${USER}
elif [ "${host:0:6}" == "login-" ]; then
    mach=betzy
    workdir=/cluster/work/users/${USER}
else
    perr 1 "Unknown machine, '${host}'"
fi

if [ ! -f "${cfg_file}" ]; then
    perr 1 "Cannot find usermods_sets.cfg in ${mydir}"
fi

for usermod in $(grep usermod_dir ${cfg_file} | grep '=' | cut -d'=' -f2); do
    if [[ "${usermod}" == *ESM* ]]; then
        cset="NF1850Esm"
    else
        cset="NF1850"
    fi
    if [[ -n "$(echo ${usermod} | grep -i cosp)" ]]; then
        has_cosp="_cosp"
    else
        has_cosp=""
    fi
    caseref="${cset}${has_cosp}"
    casedir="${workdir}/${usermod}"
    if [ -d "${casedir}" ]; then
        # Assume an existing case directory can be cloned
        case_reference[${caseref}]="${casedir}"
        # Do not try to rebuild or rerun this case
        continue
    fi
    create_newcase="no"
    if [ -n "${case_reference[${caseref}]}" ]; then
        # We can clone this case
        arglist="--case ${casedir} --clone ${case_reference[${cset}]}"
        arglist="${arglist} --user-mods-dirs ${usermods_dirs}/${usermod}"
        arglist="${arglist} --keepexe"
        ${cclone} ${arglist}
    else
        # Create a new case and store a reference
        arglist="--case ${casedir} --compset ${cset} --res ne16pg3_tn14"
        arglist="${arglist} --project ${PROJECT} --mach ${mach}"
        arglist="${arglist} --user-mods-dirs ${usermods_dirs}/${usermod}"
        arglist="${arglist} --output-root ${workdir} --run-unsupported"
        ${cnewcase} ${arglist}
        perr $? "create_newcase failed for ${usermod}"
        case_reference[${caseref}]="${casedir}"
        create_newcase="yes"
    fi
    cd ${casedir}
    perr $? "Cannot cd to '${casedir}'"
    if [ "${create_newcase}" == "yes" ]; then
        # Only setup the case after create_newcase
        if [ -n "${has_cosp}" ]; then
            ./xmlchange -append CAM_CONFIG_OPTS="-cosp"
            perr $? "Error trying to append '-cosp' to CAM_CONFIG_OPTS"
        fi
        ./xmlchange DOUT_S="FALSE"
        perr $? "Error trying 'DOUT_S=\"FALSE\"'"
    fi
    ./case.setup
    perr $? "Failure in case.setup for ${usermod}"
    ./xmlchange STOP_OPTION=nmonths
    perr $? "Failure in xmlchange STOP_OPTION=nmonths for ${usermod}"
    ./xmlchange STOP_N=1
    perr $? "Failure in xmlchange STOP_N=1 for ${usermod}"
    ./case.build
    perr $? "Error in case.build for ${usermod}"
    ./case.submit
    perr $? "Error in case.submit for ${usermod}"
done
