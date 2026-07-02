#! /bin/env python3

"""Script to read version of the CMIP7 data request spreadsheet, check for
any field requests which are not availble from the CAM CMIP7 model
configurations, and produce the requested diagnostic sections of CAM's
runtime namelist.
Note that the data request spreadsheet must be in CSV format
"""

import argparse
import configparser
import contextlib
import csv
import os
import re
import sys
## Local imports
from chemistry import read_fieldname_file, all_chem_names, all_emission_names

# Input and output codes
_FREQUENCY_COLNAME = "CMIP7 Freq."
_MODELTYPE_COLNAME = "Modelling Realm - Primary"
_REGION_COLNAME = "Region"
_CAM_DIAG_COLNAME = "NorESM3 name (dependency)"
_CMIP_COMPOUND_NAME = "CMIP7 Compound Name"
_REQUIRED_HEADERS = [_FREQUENCY_COLNAME, _MODELTYPE_COLNAME, _REGION_COLNAME,
                     _CAM_DIAG_COLNAME]
_AVG_COLNAMES = [_CMIP_COMPOUND_NAME, "Processing type"]
_CMIP_AVGFLG_RE = re.compile(r"[a-zA-Z.]*[.](t[a-z])[-]")
# recognized CMIP7 averaging flags (not complete, just what is available to CAM)
_CMIP_AVGFLAGS = {'tavg':'A', 'tmin':'M', 'tmax':'X', 'tpt':'I'}

_HIST_FILEORDER = ['mon', 'day', '6hr', '3hr', '1hr', 'subhr']
_HIST_FRQCODES = {'mon':'0', 'day':'-24', '6hr':'-6', '3hr':'-3', '1hr':'-1', 'subhr':'1'}
_HIST_MFILT = {'mon':'1', 'day':'30', '6hr':'30', '3hr':'30', '1hr':'30', 'subhr':'30'}
_HIST_TITLES =  {'mon':'! monthly output', 'day':'! daily output', '6hr':'! 6-hourly output',
                 '3hr':'! 3-hourly output', '1hr':'! 1-hourly output',
                 'subhr':'! timestep output'}

# Special CAM diagnostics hardcoded in cam_history.F90 but not in fixed list
_CAM_FIXED_FIELDS = {'co2vmr', 'ch4vmr', 'n2ovmr', 'f11vmr', 'f12vmr',
                     'sol_tsi', 'ndcur', 'nscur', 'nsteph', 'area'}

# Relative paths
__MYDIR = os.path.abspath(os.path.dirname(__file__))
__CAMDIR = os.path.dirname(os.path.dirname(__MYDIR))

class Usermod():
    """Class to hold information about a history usermod directory"""

    def __init__(self, name, dirname, frequencies, usermods_dir, chemistry,
                 include_cosp=False, include_aerocom=False, emission_driven=False):
        """Initialize a history usermod section"""
        self.__name = name
        self.__dirname = os.path.normpath(os.path.join(usermods_dir, dirname))
        self.__chemistry = chemistry
        self.__freqset = set([x.strip() for x in frequencies.split(',')])
        self.__cosp = include_cosp
        self.__aerocom = include_aerocom
        self.__esm = emission_driven

    def namelist_file(self):
        """Construct and return the namelist filename for this object"""
        return os.path.join(self.dirname, "user_nl_cam")

    @property
    def name(self):
        """Return the name for this object"""
        return self.__name

    @property
    def dirname(self):
        """Return the usermod subdirectory name for this object"""
        return self.__dirname

    @property
    def chemistry(self):
        """Return the name of the chemistry scheme for this object"""
        return self.__chemistry

    @property
    def frequencies(self):
        """Return the frequencies for this object"""
        return self.__freqset

    @property
    def include_cosp(self):
        """Return True if COSP fields are to be active for this object"""
        return self.__cosp

    @property
    def include_aerocom(self):
        """Return True if aerocom fields are to be active for this object"""
        return self.__aerocom

    @property
    def emission_driven(self):
        """Return True if this object represents an emission-driven NorESM run"""
        return self.__esm

def is_number(text):
    """Return True if <text> represents a literal numeric constant.
    Return False otherwise."""
    val = False
    try:
        flt = float(text)
        val = True
    except ValueError as verr:
        val = False
    # end try
    return val

def quote_field(fieldname):
    """Ensure that <fieldname> has only single quotes.
    Return the quoted string."""
    fieldname = str(fieldname).strip()
    if (fieldname[0] == "'") or (fieldname[0] == '"'):
        fieldname = fieldname[1:]
    # end if
    if (fieldname[-1] == "'") or (fieldname[-1] == '"'):
        fieldname = fieldname[0:-1]
    # end if
    qm = "'"
    return f"{qm}{fieldname}{qm}"

def command_line(args):
    """Read the command line arguments (args) to retrieve the paths to the
    CMIP7 and CAM data request spreadsheets, the config file, and options.
    Return all argument values."""
    parser = argparse.ArgumentParser(description=__doc__)

    parser.add_argument("CMIP_file", type=str,
                        metavar='<path to CMIP7 data request file>')
    parser.add_argument("CAM_file", type=str,
                        metavar='<path to CAM data request file>')
    umod_def = os.path.join(__CAMDIR, "cime_config", "usermods_dirs")
    umod_help = ("Path to write namelist file entries. "
                 f"default: {umod_def}")
    parser.add_argument("--usermods-dir", dest='usermods', type=str,
                        metavar='<USERMODS FILEPATH>', default=umod_def,
                        help=umod_help)
    parser.add_argument("--overwrite", action='store_true', default=False,
                        help="Overwrite namelist file(s) if they exist")
    umod_def = os.path.join(__MYDIR, "usermods_sets.cfg")
    umod_help = ("Path to configuration file for usermods sets. "
                 f"default: {umod_def}")
    parser.add_argument("--usermods-config", dest='cfgfile', type=str,
                        metavar='<USERMODS CONFIG FILEPATH>',
                        default=umod_def, help=umod_help)
    umod_help = ("Stop processing if any missing fields found. "
                 "Default is to produce fieldlist files by ignoring any "
                 "missing fields.")
    parser.add_argument("--error-on-missing", action='store_true', default=False,
                        help=umod_help)
    umod_def = 80
    umod_help = f"Maximum line length for namelist files. default: {umod_def}"
    parser.add_argument("--max-line", type=int, default=umod_def, help=umod_help)
    umod_help = ("Produce more output on missing fields. "
                 "By default, a field is only declared missing if it is "
                 "not available in any configuration.")
    parser.add_argument("--verbose", action='store_true', default=False,
                        help=umod_help)
    umod_help=("Only process (update) the usermods sets specified."
               "By default, all configured usermods sets are processed "
               "and updated. On the command line, these section-names "
               "(the string in square brackets in the usermods-config "
               "file) is specified after the required arguments but "
               "before any options.")
    parser.add_argument("usermod_sets", nargs="*", default=[],
                        help=umod_help)
    pargs = parser.parse_args(args)
    return (pargs.CMIP_file, pargs.CAM_file, pargs.usermods, pargs.cfgfile,
            pargs.overwrite, pargs.error_on_missing, pargs.max_line,
            pargs.verbose, pargs.usermod_sets)

def read_config_file(filename, usermods_dir, overwrite):
    """Read a fincl group configuration (ini-style) file.
    Returns a dictionary of usermods sections with the section name as the key.
    If any errors are found, print and return None"""
    errors = False
    known_keywords = set(['frequencies', 'usermod_dir', 'chemistry',
                          'cosp_on', 'use_aerocom', 'emission_driven'])
    usermod_dict = {}
    config = configparser.ConfigParser()
    config.read(filename)
    for section in config.sections():
        cfg_sect = config[section]
        use_cosp = False
        include_aerocom = False
        is_ems_run = False
        frequencies = cfg_sect['frequencies']
        dirname = cfg_sect['usermod_dir']
        chemistry = cfg_sect['chemistry']
        if 'COSP_on' in cfg_sect:
            use_cosp = cfg_sect['COSP_on'] == "True"
        # end if
        if 'use_aerocom' in cfg_sect:
            include_aerocom = cfg_sect['use_aerocom'] == "True"
        # end if
        if 'emission_driven' in cfg_sect:
            is_ems_run = cfg_sect['emission_driven'] == "True"
        # end if
        if section in usermod_dict:
            print(f"Duplicate section, '{section}'")
            errors = True
        # end if
        bad_keywords = set([x.lower() for x in cfg_sect.keys()]) - known_keywords
        if bad_keywords:
            slist = ', '.join(sorted(bad_keywords))
            print(f"Unknown keywords in {section}, {slist}")
            errors = True
        # end if
        usermod_dict[section] = Usermod(section, dirname, frequencies,
                                        usermods_dir, chemistry,
                                        include_cosp=use_cosp,
                                        include_aerocom=include_aerocom,
                                        emission_driven=is_ems_run)
    # end for
    # Check for errors
    for name, usermod in usermod_dict.items():
        if not overwrite:
            path = usermod.namelist_file()
            if os.path.exists(path):
                print(f"namelist file, '{path}' exists and overwrite = False")
                errors = True
            # end if
        # end if
        # Check frequencies
        if not usermod.frequencies:
            print(f"Section, '{name}', contains no output frequencies")
            errors = True
        elif any([x not in _HIST_FILEORDER for x in usermod.frequencies]):
            unknown = list(set(usermod.frequencies) - set(_HIST_FILEORDER))
            freq = ', '.join(unknown)
            print(f"Section, '{section}', contains unknown frequencies: {freq}")
            errors = True
        # end if
    # end for
    if errors:
        return None
    # end if
    return usermod_dict

def read_diagnostic_fieldnames():
    """Read the fixed set of CAM diagnostic (history) fieldnames from the
       saved fixed list.
    Read separate sets of COSP and Aerocom fieldnames
    Return the three sets of fieldnames.
    """

    fixed_fieldnames = set(read_fieldname_file("fixed_fieldlist.txt"))
    cosp_fieldnames = set(read_fieldname_file("cosp_fieldlist.txt"))
    aerocom_fieldnames = set(read_fieldname_file("aerocom_fieldlist.txt"))

    return fixed_fieldnames, cosp_fieldnames, aerocom_fieldnames

def get_hist_proc_flag(row, avg_col, freq, rownum):
    """Figure and return a history processing flag or <row>.
    If <avg_col> is not none, parse the desired processing type from that.
    For <avg_col> is None, if <freq> is 'subhr', the processing type is 'I', otherwise,
    it is 'A'.
    It is an error to specify non-'I' processing for 'subhr' (time-step) history output.
    <rownum> is used for error messages.
    """
    hist_flag = ''
    if avg_col is None:
        avg_fld = None
    else:
        avg_fld = row[avg_col]
    # end if
    if avg_fld is None:
        if freq == 'subhr':
            hist_flag = 'I'
        else:
            hist_flag = 'A'
        # end if
    else:
        match = _CMIP_AVGFLG_RE.match(avg_fld)
        if match is None:
            # This should be a column which simply has the average flag we want
            if (len(avg_fld) != 1) or (avg_fld not in ['I', 'A', 'X', 'M', 'B', 'N', 'L', 'S']):
                raise ValueError(f"Error: Invalid processing flag, '{avg_fld}' on row {rownum}")
            # end if
            hist_flag = avg_fld
        else:
            hist_desc = match.group(1)
            if hist_desc in _CMIP_AVGFLAGS:
                hist_flag = _CMIP_AVGFLAGS[hist_desc]
            else:
                emsg = f"Error: Invalid {_CMIP_COMPOUND_NAME}, '{hist_desc}' on row {rownum}"
                raise ValueError(emsg)
            # end if
            if (freq == 'subhr') and (hist_flag != 'I'):
                emsg = f"Error: Invalid processing flag, '{hist_flag}' for time-step output"
                raise ValueError(f"{emsg} on row {rownum}")
            # end if
        # end if
    # end if
    return hist_flag

def parse_spreadsheet(csvfile, model_names=["atmos", "aerosol", "atmosChem"]):
    """Parse <csvfile> and return a dictionary of the requested CAM fields at
    different output frequencies.
    The dictionary keys are the frequency and the value is a set of
    fieldnames.
    <model_names> is an optional list of modelling (modeling) realms. The
    default is the list of CAM realms."""
    cmip_dict = {}
    with open(csvfile, mode='r', newline="") as infile:
        reader = csv.reader(infile)
        headers = next(reader)
        # Create a dictionary with the column number for each required column
        col_dirs = {}
        for colnum, col in enumerate(headers):
            if col in _REQUIRED_HEADERS:
                if col in col_dirs:
                    emsg = (f"Duplicate column entry, '{col}', in columns "
                            f"{col_dirs[col]} and {colnum}")
                    raise ValueError(emsg)
                # end if
                col_dirs[col] = colnum
            # end if
        # end for
        if len(col_dirs) != len(_REQUIRED_HEADERS):
            missing = ', '.join(set(_REQUIRED_HEADERS) - set(col_dirs.keys()))
            raise ValueError(f"Missing headers: {missing}")
        # end if
        rownum = 1
        freq_col = col_dirs[_FREQUENCY_COLNAME]
        model_col = col_dirs[_MODELTYPE_COLNAME]
        region_col = col_dirs[_REGION_COLNAME]
        name_col = col_dirs[_CAM_DIAG_COLNAME]
        avg_col = None
        for flag_col_name in _AVG_COLNAMES:
            if flag_col_name in col_dirs:
                avg_col = col_dirs[flag_col_name]
                exit
            # end if
        # end for
        for row in reader:
            rownum += 1
            if row[model_col] not in model_names:
                continue
            # end if
            if (row[region_col].upper() != "GLB") and row[name_col].strip():
                print(f"Field {row[name_col]} on row {rownum} has region, "
                      f"{row[region_col]},  skipping")
            else:
                # First, make sure there is a dictionary entry for this frequency
                if row[freq_col] not in cmip_dict:
                    cmip_dict[row[freq_col]] = set()
                # end if
                names = [x.strip() for x in re.split(r'[+/,*()-]', row[name_col])
                         if x.strip() and (not is_number(x.strip()))]
                # What history processing flag should we add?
                hist_flag = get_hist_proc_flag(row, avg_col, row[freq_col], rownum)
                for name in names:
                    if name in _CAM_FIXED_FIELDS:
                        cmip_dict[row[freq_col]].add(f"{name}:I")
                    else:
                        cmip_dict[row[freq_col]].add(f"{name}:{hist_flag}")
                    # end if
            # end if
        # end for
    # end with
    return cmip_dict

def combine_data_requests(dict1, dict2):
    """Combine entries for common keys each key in <dict1> and <dict2>.
    Return a combined dictionary."""
    data_request = {}
    for key in set(dict1.keys()) | set(dict2.keys()):
        if key not in dict2:
            data_request[key] = dict1[key]
        elif key not in dict1:
            data_request[key] = dict2[key]
        else:
            data_request[key] = dict1[key] | dict2[key]
        # end if
    # end for
    return data_request

def dict_to_set(request_dict):
    """Collect all the fields from <request_dict> into a set after removing any
    history processing flags.
    Return the set.
    """
    request_set = set()
    for fields in request_dict.values():
        request_set |= set([x.split(':')[0] for x in list(fields)])
    # end for
    return request_set

def check_for_missing_fieldnames(fixedset, data_request):
    """Given a data request dictionary (<data_request>),
    check to see if any are not in <fixedset>.
    Return a set of missing fields names (an empty set means none).
    Print out any missing fields.
    Checks for fields in <fields_to_ignore> are bypassed.
    Clean <data_request> to remove missing field entries (side effect)."""
    # Gather the set of all fields (combine different frequencies)
    all_reqfields = dict_to_set(data_request)
    # Any fields not in <fixedset> are missing
    missing = all_reqfields - fixedset
    # Remove missing fields from data_request
    for key in data_request:
        data_request[key] -= missing
    # end for
    # Remove fixed fields from missing after removing them from data request
    # This is because they do not have an associated addfld/outfld in CAM.
    missing -= _CAM_FIXED_FIELDS
    return missing

def generate_namelist_entries(data_request, usermod_config, fixed_fieldnames,
                              cosp_fieldnames, aerocom_fieldnames,
                              usermods_sets, maxline):
    """Write the sets of namelist entries represented by <data_request> to
    the usermods files defined in <usermod_config>.
    Return a dictionary of field names not found in the CAM fixed list. The missing
    names are found and reported from each config set """
    missing_fields = {}
    for usermod in usermod_config.values():
        if usermods_sets and (usermod.name not in usermods_sets):
            continue
        # end if
        lbreak = ''
        if not os.path.exists(usermod.dirname):
            os.makedirs(usermod.dirname)
        # end if
        # Create the set of fields available for this config section
        chem_fieldnames = all_chem_names(usermod.chemistry)
        avail_fieldnames = fixed_fieldnames | chem_fieldnames
        if usermod.include_cosp:
            avail_fieldnames |= cosp_fieldnames
        # end if
        if usermod.include_aerocom:
            avail_fieldnames |= aerocom_fieldnames
        # end if
        if usermod.emission_driven:
            avail_fieldnames |= all_emission_names()
        # end if
        # Add any missing fields to the dict (already removed from <data_request>
        missing = check_for_missing_fieldnames(avail_fieldnames, data_request)
        for field in missing:
            if field not in missing_fields:
                missing_fields[field] = []
            # end if
            missing_fields[field].append(usermod.name)
        # end for
        with open(usermod.namelist_file(), mode="w") as outfile:
            outfile.write(f"! CAM {usermod.name} diagnostic namelist entries\n\n")
            if usermod.include_aerocom:
                outfile.write("! Aerocom fields will be output for this run\n")
                outfile.write("use_aerocom = .true.\n\n")
            # end if
            if usermod.include_cosp:
                outfile.write("! COSP fields will be output for this run\n")
                outfile.write("! Note: This requires building the model with "
                              "the -cosp flag in CAM_CONFIG_OPTS\n")
                outfile.write("docosp = .true.\n\n")
            # end if
            outfile.write(f"! Only output fields listed in this file\n")
            outfile.write(f"empty_htapes = .true.\n\n")
            for freq in sorted(usermod.frequencies,
                               key=lambda x: _HIST_FILEORDER.index(x)):
                if freq in data_request:
                    # index is the fincl number for this frequency
                    index = _HIST_FILEORDER.index(freq) + 1
                    # Write history file config info
                    outfile.write(f"{lbreak}{_HIST_TITLES[freq]}\n")
                    outfile.write(f"nhtfrq({index}) = {_HIST_FRQCODES[freq]}\n")
                    outfile.write(f"mfilt({index}) = {_HIST_MFILT[freq]}\n")
                    fields = data_request[freq]
                    # Convert to sorted list, skip fields not in available fields
                    fields = sorted([quote_field(x) for x in fields
                                     if x.split(':')[0] in avail_fieldnames])
                    fldstring = ', '.join(fields)
                    nlstr = f"fincl{index} = {fldstring}"
                    # Write the fincl string with appropriate line breaks
                    begpos = 0
                    strlen = len(nlstr)
                    while begpos < strlen:
                        endpos = strlen
                        if endpos - begpos > maxline:
                            endpos = nlstr[0:begpos + maxline].rfind(' ')
                            if endpos < begpos:
                                endpos = strlen
                            # end if
                        # end if
                        outfile.write(f"{nlstr[begpos:endpos]}\n")
                        begpos = endpos
                    # end while
                # end if
                lbreak = '\n'
                # end for
            # end if
        # end with (open file)
    # end for (sections)
    return missing_fields

###############################################################################

if __name__ == "__main__":
    arglist = command_line(sys.argv[1:])
    cmipfile, camfile, usermods, configfile, overwrite, error, maxline, verbose, usets = arglist
    # read configuration
    usermod_dict = read_config_file(configfile, usermods, overwrite)
    errmsg = "not producing any namelist usermods files"
    fieldnames = read_diagnostic_fieldnames()
    fixed_fieldnames, cosp_fieldnames, aerocom_fieldnames = fieldnames
    chem_fieldnames = all_chem_names()
    cmip7_request = parse_spreadsheet(cmipfile)
    cam_request = parse_spreadsheet(camfile)
    if error and (missing7 or missingc):
        print(f"Missing fields found, {errmsg}")
    elif usermod_dict:
        data_request = combine_data_requests(cmip7_request, cam_request)
        missing = generate_namelist_entries(data_request, usermod_dict, fixed_fieldnames,
                                            cosp_fieldnames, aerocom_fieldnames,
                                            usets, maxline)
        num_sections = len(usermod_dict)
        if not verbose:
            # Remove missing fields that are defined in at least one usermod
            to_remove = []
            for field in missing:
                if len(missing[field]) < num_sections:
                    to_remove.append(field)
                # end if
            # end for
            for field in to_remove:
                del missing[field]
            # end for
        # end if
        if missing:
            if verbose:
                print(f"The following {len(missing)} fields are not output in some "
                      "usermod configurations")
            else:
                print(f"The following {len(missing)} fields are not output from CAM:")
            # end if
        # end if
        jstr = ', '
        for data_request, label in [(cmip7_request, "CMIP7"), (cam_request, "CAM")]:
            request_set = dict_to_set(data_request)
            message_shown = False
            for field in sorted(missing) if missing else []:
                if field in request_set:
                    if not message_shown:
                        print(f"The following fields are from the {label} data request spreadsheet:")
                        message_shown = True
                    # end if
                    if verbose:
                        print(f"  {field}: {jstr.join(missing[field])}")
                    else:
                        print(f"  {field}")
                    # end if
                # end if
            # end for
        # end for
    # end if
    if not usermod_dict:
        print(f"Errors and/or conflicts found in usermod config file, {errmsg}")
    # end if
    sys.exit(0)
