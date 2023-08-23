#! /bin/bash

##
## General syntax help function
## Usage: help <exit status>
##
help () {
  local hname="Usage: $( basename ${0} )"
  local hprefix="$( echo ${hname} | tr '[!-~]' ' ' )"
  echo "${hname}   --root <cam_root_dir>"
  echo "${hprefix} [ --project <project num> ]"
  echo "${hprefix} [ --compare <tag> ]"
  echo "${hprefix} [ --compiler <compiler> ]"
  echo "${hprefix} [ --generate <CAM-Nor version> ]"
  echo "Less common options"
  echo "${hprefix} [ --rerun ]"
  echo "${hprefix} [ --baselinedir <baseline_dirname> ]"
  echo "${hprefix} [ --testdir <test_dirname> ]"
  echo "${hprefix} [ --clean ]"
  echo "${hprefix} [ --cleanall ]"
  echo "${hprefix} [ --dryrun ]"
  echo "${hprefix} [ --rm-testdir ]"
  exit $1
}

perr() {
##
## Output an error message <$2> if error code <$1> is non-zero
##
  if [ $1 -ne 0 ]; then
    echo -e "\nERROR ${1}: ${2}\n"
    exit $1
  fi
}

lastCamTag () {
  local taglist
  local tag
  local currdir
  currdir="$( pwd -P 2> /dev/null )"
  if [ -d "${CAM_ROOT}/components/cam" ]; then
    cd ${CAM_ROOT}/components/cam
  elif [ -d "${CAM_ROOT}/models/atm/cam" ]; then
    perr 1 "lastCamTag does not work with old directory structure"
  elif [ -d "${CAM_ROOT}/test/system" ]; then
    cd ${CAM_ROOT}
  else
    perr 1 "Cannot find CAM source at, \"${CAM_ROOT}\""
  fi
  tag="$( git describe | sed -e 's/[-][0-9]*[-][0-9a-g]*$//' )"
  cd ${currdir}
  echo $tag
}

extract_testtype () {
  # extract_testtype testdir
  local IFS
  local dcomps
  local ftype
  local addtype
  IFS=.
  dcomps=(${1})
  shift
  ftype="${dcomps[1]}"
  addtype="yes"
  for item in ${@}; do
    if [ "${item}" == "${ftype}" ]; then
      addtype="no"
    fi
  done
  if [ "${addtype}" == "yes" ]; then
    echo "${ftype}"
  else
    echo ""
  fi
}

clean_testdir () {
  # clean_testdir CAM_TESTDIR cleanall [<test_id>]
  local cam_tdir # $1
  local cleanall # $2
  local cscmd
  local ctest
  local currdir
  local test_id  # $3
  local testdir
  local tname
  cam_tdir="${1}"
  cleanall="${2}"
  test_id="${3}"
  currdir="$( pwd -P )"
  # To clean tests, we need to find the proper test directory
  if [ -n "${scratch}" -a -n "${test_id}" ]; then
    testdir="${scratch}/${test_id}"
    cscmd="${testdir}/cs.status.${test_id}"
    if [ -f "${cscmd}" ]; then
      fails="$( ${cscmd} | grep Overall | grep FAIL | cut -d' ' -f3 )"
    else
      fails=""
    fi
    for tname in ${fails}; do
      ctest="${tname}.GC.${test_id}"
      if [ "${cleanall}" == "yes" ]; then
        rm -rf ${testdir}/${ctest}
      else
        cd ${testdir}/${ctest}
        perr $? "Cannot enter ctest dir = '${testdir}/${ctest}'"
        rm -f TestStatus* run/*.log*
        rm -rf bld
        ./.case.test --reset
        ./case.setup --reset
      fi
      cd ${currdir}
    done
  fi
  # Clean up old log files
  # Do not remove the aux_cam_noresm_xxx files as those are reused.
  rm -rf ${cam_tdir}/cime-tests.o*
}

##################################################
##
## Beginning of script
##
##################################################

# Make sure we are starting off in a valid directory
if [ ! -d "." ]; then
  perr 1 "The current directory does not exist!!"
fi

if [ -z "${currdir}" ]; then
  currdir="$( pwd -P )"
fi
if [ -z "${scriptdir}" ]; then
  scriptdir="$( cd $( dirname $0 ); pwd -P )"
  perr $? "Line ${LINENO}: Cannot enter scriptdir = '${scriptdir}'"
fi

# By default, let the machine set the baseline root
BL_ROOT=""
# Default baseline version for compare will be latest tag (from git describe)
bl_version=""
# Let machine set default compiler
CAM_FC=""
# No default for CAM_ROOT but env value allowed
CAM_ROOT=${CAM_ROOT:-""}
# Clean all test status
cleanall="no"
# Clean failures for successful rebuild
doclean="no"
# Dry run, print out the command but do not run it.
dryrun="no"
# Generate new baselines with this tag
new_tag=""
# Most test machines require a project code
project=""
# Remove test directory before beginning
removeall="no"
# Rerun an existing aux_cam_noresm suite
rerun=""

## Process our input arguments
while [ $# -gt 0 ]; do
  case $1 in
    --h | -h | --help | -help)
      help 0
      ;;
    --baselinedir)
      if [ ! -d "${2}" ]; then
        perr 1 "Baseline root (${2}) must be an existing directory"
      fi
      BL_ROOT=${2}
      shift
      ;;
    --compare)
      if [ $# -lt 2 ]; then
        perr 1 "${1} requires a CAM tag for baseline tests"
      fi
      bl_version=${2}
      shift
      ;;
    --clean)
      doclean="yes"
      ;;
    --cleanall)
      cleanall="yes"
      ;;
    --compiler)
      if [ $# -lt 2 ]; then
        perr 1 "${1} requires a compiler name (e.g., INTEL)"
      fi
      CAM_FC="${2^^}"
      shift
      ;;
    --dryrun)
      dryrun="yes"
      ;;
    --generate)
      if [ $# -lt 2 ]; then
        perr 1 "${1} requires a new CAM tag name"
      fi
      new_tag=${2}
      shift
      ;;
    --project)
      if [ $# -lt 2 ]; then
        perr 1 "${1} requires a project or accounting code"
      fi
      project=${2}
      shift
      ;;
    --rerun)
      rerun="--rerun-cesm"
      ;;
    --rm-testdir)
      removeall="yes"
      ;;
    --root | -root)
      if [ $# -lt 2 ]; then
        perr 1 "${1} requires a CAM root directory"
      fi
#  We need to make sure that the install directory is a full path
#  First, we see if it looks like a full path (not sure Windows will like this)
      case $1 in
        /*)
          CAM_ROOT=$2
        ;;
        *)
          if [ ! -d "${2}" ]; then
            perr 1 "CAM root (${2}) must be an existing directory"
          fi
          CAM_ROOT="$( cd $2; pwd -P )"
          perr $? "CAM root must exist"
      esac
      if [ ! -d "${CAM_ROOT}" ]; then
        perr 1 "The specified CAM directory, \"${2}\", does not exist."
        exit 1
      fi
      shift
      ;;
    --testdir | -testdir)
      if [ $# -lt 2 ]; then
        perr 1 "${1} requires a test directory name"
      fi
      CAM_TESTDIR=${2}
      shift
      ;;
    *)
      perr 1 "Unrecognized option, \"${1}\""
      ;;
  esac
  shift
done

if [ ! -d "${CAM_ROOT}" ]; then
  perr 1 "Must specify CAM root with --root switch"
fi

####################################
##
##  Load machine-dependent settings
##
####################################

if [ -z "${HOST}" -o "${HOST:0:5}" == "login" ]; then
  export HOST="$( hostname )"
fi
if [ "$( echo ${HOST} | sed -e 's/^[^.]*[.]//' -e 's/[.].*$//' )" == "betzy" ]; then
  machname="betzy"
  CESMDATAROOT="/cluster/shared/noresm/inputdata"
  if [ -z "${BL_ROOT}" ]; then
    BL_ROOT="/cluster/shared/noresm/noresm_baselines"
  fi
  if [ -z "${CAM_FC}" ]; then
    export CAM_FC="intel"
  fi
  scratch="/cluster/work/users/${USER}"
else
  echo "ERROR: Unsupported host, \"${HOST}\"."
  exit 3
fi

## We are running NorESM but that is not yet
##    installed in the system as a separate model option.
export CIME_MODEL=cesm

## If a baseline tag was specified, make sure it exists
if [ -n "${bl_version}" ]; then
  if [ ! -d "${BL_ROOT}/${bl_version}" ]; then
    perr "Baseline directory, '${BL_ROOT}/${bl_version}', must exist"
  fi
fi

## Create a test directory if necessary
if [ -z "${CAM_TESTDIR}" ]; then
  CAM_TESTDIR="${scratch}/camtest_${CAM_FC,,}"
fi

# Find the test_id if this is a rerun
if [ -n "${rerun}" ]; then
  # We are doing a rerun, find the last test directory
  # We should be in the place to look
  logfile="$( ls ${CAM_TESTDIR}/aux_cam_noresm_${CAM_FC,,}_*.log | tail -n 1 )"
  scriptdir=$( cd $( dirname ${0}); pwd -P )
  perr $? "Cannot enter old run dir = '${scriptdir}'"
  test_info="$( ${scriptdir}/findTestInfo.py ${logfile} )"
  test_tokens=( $test_info )
  if [ ${#test_tokens[@]} -eq 2 ]; then
    CAM_TESTDIR=${test_tokens[0]}
    test_id=${test_tokens[1]}
  else
    perr 1 "Cannot find test info, \"${test_info}\""
  fi
  export CAM_TESTDIR
  rerun="${rerun} ${test_id}"
else
  test_id=""
fi

# If this is a rerun, we may have to do a clean
if [ -n "${CAM_TESTDIR}" ]; then
  if [ -d "${CAM_TESTDIR}" ]; then
    if [ "${removeall}" == "yes" ]; then
      if [ "${dryrun}" == "yes" ]; then
        echo "Dryrun: Remove old test dir, '${CAM_TESTDIR}'"
      else
        rm -rf "${CAM_TESTDIR}"
      fi
    elif [ "${doclean}" == "yes" -o "${cleanall}" == "yes" ]; then
      if [ "${dryrun}" == "yes" ]; then
        echo "Dryrun: Clean test dir, '${CAM_TESTDIR}', test ID = '${test_id}'"
      else
        clean_testdir "${CAM_TESTDIR}" "${cleanall}" "${test_id}"
      fi
    fi
  fi
  if [ "${dryrun}" != "yes" ]; then
    if [ ! -d "${CAM_TESTDIR}" ]; then
      mkdir -p $CAM_TESTDIR
    fi
    CAM_TESTDIR="$( cd ${CAM_TESTDIR}; pwd -P )"
    perr $? "Line ${LINENO}: Cannot enter CAM_TESTDIR = '${CAM_TESTDIR}'"
  fi
fi

# Export variables which might have been changed
export CAM_ROOT
if [ -d "${CAM_ROOT}/components/cam/test/system" ]; then
  export CAM_STEST="${CAM_ROOT}/components/cam/test/system"
elif [ -d "${CAM_ROOT}/models/atm/cam/test/system" ]; then
  export CAM_STEST="${CAM_ROOT}/models/atm/cam/test/system"
elif [ -d "${CAM_ROOT}/test/system" ]; then
  export CAM_STEST="${CAM_ROOT}/test/system"
else
  perr 1 "Cannot find system test directory from root, \"${CAM_ROOT}\""
fi
export CAM_FC
if [ -n "${CAM_TESTDIR}" ]; then
  export CAM_TESTDIR
fi

#########################################3

if [ -n "${CAM_TESTDIR}" ]; then
  if [ "${dryrun}" != "yes" ]; then
    cd $CAM_TESTDIR
    perr $? "Line ${LINENO}: Cannot enter CAM_TESTDIR = '${CAM_TESTDIR}'"
  fi
  echo "CAM_TESTDIR = ${CAM_TESTDIR}"
else
  perr 1 "No value for CAM_TESTDIR"
fi
if [ -n "${CAM_ROOT}" ]; then
  echo "CAM_ROOT = ${CAM_ROOT}"
fi
if [ -n "${CAM_FC}" ]; then
  echo "Tests using the '${CAM_FC}' compiler"
fi

command="${CAM_ROOT}/cime/scripts/create_test --xml-category aux_cam_noresm"
command="${command} --machine ${machname}"
command="${command} --test-root ${CAM_TESTDIR} --output-root ${CAM_TESTDIR}"
if [ -n "${project}" ]; then
  command="${command} --project nn9560k"
fi
if [ -n "${bl_version}" -o -n "${new_tag}" ]; then
  # We will generate and/or compare so enter a baseline root
  command="${command} --baseline-root ${BL_ROOT}"
fi
if [ -n "${new_tag}" ]; then
  command="${command} --test-id ${new_tag} --generate ${new_tag}"
fi
if [ -n "${bl_version}" ]; then
  command="${command} --compare ${bl_version}"
fi

if [ "${dryrun}" == "yes" ]; then
  echo "Running: ${command}"
else
  ${command}
fi
