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

# Input and output codes
_FREQUENCY_COLNAME = "CMIP7 Freq."
_MODELTYPE_COLNAME = "Modelling Realm - Primary"
_REGION_COLNAME = "Region"
_CAM_DIAG_COLNAME = "NorESM3 name (dependency)"
_REQUIRED_HEADERS = [_FREQUENCY_COLNAME, _MODELTYPE_COLNAME, _REGION_COLNAME, _CAM_DIAG_COLNAME]
_HIST_FILEORDER = ['mon', 'day', '6hr', '3hr', '1hr', 'subhr']
_HIST_FRQCODES = {'mon':'0', 'day':'-24', '6hr':'-6', '3hr':'-3', '1hr':'-1', 'subhr':'1'}
_HIST_MFILT = {'mon':'1', 'day':'30', '6hr':'30', '3hr':'30', '1hr':'30', 'subhr':'30'}
_HIST_TITLES =  {'mon':'! monthly output', 'day':'! daily output', '6hr':'! 6-hourly output',
                 '3hr':'! 3-hourly output', '1hr':'! 1-hourly output',
                 'subhr':'! timestep output'}

# Relative paths
__MYDIR = os.path.abspath(os.path.dirname(__file__))
__CAMDIR = os.path.dirname(os.path.dirname(__MYDIR))

class Usermod():
    """Class to hold information about a history usermod directory"""

    def __init__(self, name, dirname, frequencies, usermods_dir,
                 include_cosp=False, include_aerocom=False):
        """Initialize a history usermod section"""
        self.__name = name
        self.__dirname = os.path.normpath(os.path.join(usermods_dir, dirname))
        self.__freqset = set([x.strip() for x in frequencies.split(',')])
        self.__cosp = include_cosp
        self.__aerocom = include_aerocom

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

def quote_field(fieldname, avgflag):
    """Combine <fieldname> with <avgflag> and add single quote marks around
    the combination. Remove quotes around <fieldname> if present.
    Return the quoted string."""
    fieldname = str(fieldname).strip()
    if fieldname[0] == "'":
        fieldname = fieldname[1:]
    # end if
    if fieldname[-1] == "'":
        fieldname = fieldname[0:-1]
    # end if
    qm = "'"
    return f"{qm}{fieldname}:{avgflag}{qm}"

def command_line(args):
    """Read the command line arguments (args) to retrieve the paths to the
    CMIP7 and CAM data request spreadsheets, the config file, and options.
    Return all argument values."""
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("CMIP_file", type=str,
                        metavar='<path to CMIP7 data request file>')
    parser.add_argument("CAM_file", type=str,
                        metavar='<path to CAM data request file>')
    parser.add_argument("--usermods-dir", dest='usermods', type=str,
                        default=os.path.join(__CAMDIR, "cime_config", "usermods_dirs"),
                        help="Path to write namelist file entries")
    parser.add_argument("--overwrite", action='store_true', default=False,
                        help="Overwrite namelist file(s) if they exist")
    parser.add_argument("--usermods-config", dest='cfgfile', type=str,
                        default=os.path.join(__MYDIR, "usermods_sets.cfg"),
                        help="Path to configuration file for usermods sets")
    parser.add_argument("--error-on-missing", action='store_true',
                        default=False,
                        help="""Stop processing if any missing fields found.
                        Default is to produce fieldlist files by ignoring any
                        missing fields.""")
    parser.add_argument("--max-line", type=int, default=80,
                        help="Maximum line length for namelist files")
    pargs = parser.parse_args(args)
    return (pargs.CMIP_file, pargs.CAM_file, pargs.usermods, pargs.cfgfile,
            pargs.overwrite, pargs.error_on_missing, pargs.max_line)

def read_config_file(filename, usermods_dir, overwrite):
    """Read a fincl group configuration (ini-style) file.
    Returns a dictionary of usermods sections with the section name as the key.
    If any errors are found, print and return None"""
    errors = False
    usermod_dict = {}
    config = configparser.ConfigParser()
    config.read(filename)
    for section in config.sections():
        cfg_sect = config[section]
        use_cosp = None
        include_aerocom = None
        frequencies = cfg_sect['frequencies']
        dirname = cfg_sect['usermod_dir']
        if 'COSP_on' in cfg_sect:
            use_cosp = cfg_sect['COSP_on'] == "True"
        # end if
        if 'use_aerocom' in cfg_sect:
            include_aerocom = cfg_sect['use_aerocom'] == "True"
        # end if
        if section in usermod_dict:
            print(f"Duplicate section, '{section}'")
            errors = True
        # end if
        usermod_dict[section] = Usermod(section, dirname, frequencies,
                                        usermods_dir,
                                        include_cosp=use_cosp,
                                        include_aerocom=include_aerocom)
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

def read_fieldname_file(filename):
    """Read a fieldname file and return all fieldnames as a list."""
    diag_fieldname_file = os.path.join(__MYDIR, filename)
    all_fieldnames = []
    with open(diag_fieldname_file, mode='r') as infile:
        for line in infile:
            fieldnames = [x.strip() for x in line.split()]
            all_fieldnames.extend(fieldnames)
        # end for
    # end with
    return all_fieldnames

def read_diagnostic_fieldnames():
    """Read the master set of CAM diagnostic (history) fieldnames from the
       saved master list.
    Read separate sets of COSP and Aerocom fieldnames
    Return the three sets of fieldnames
    Note: The master set (all_fieldnames) includes the COSP and
          Aerocom fieldnames."""

    all_fieldnames = set(read_fieldname_file("master_fieldlist.txt"))
    cosp_fieldnames = set(read_fieldname_file("cosp_fieldlist.txt"))
    aerocom_fieldnames = set(read_fieldname_file("aerocom_fieldlist.txt"))

    all_fieldnames |= cosp_fieldnames
    all_fieldnames |= aerocom_fieldnames

    return all_fieldnames, cosp_fieldnames, aerocom_fieldnames

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
        for row in reader:
            rownum += 1
            if row[model_col] not in model_names:
                continue
            # end if
            if (row[region_col] != "GLB") and row[name_col].strip():
                print(f"Field {row[name_col]} on row {rownum} has region, "
                      "{row[region_col]},  skipping")
            else:
                # First, make sure there is a dictionary entry for this frequency
                if row[freq_col] not in cmip_dict:
                    cmip_dict[row[freq_col]] = []
                # end if
                names = [x.strip() for x in re.split(r'[+/,*()-]', row[name_col])
                         if x.strip() and (not is_number(x.strip()))]
                cmip_dict[row[freq_col]].extend(names)
            # end if
        # end for
    # end with
    # Cleanup each request to remove duplicates and sort
    for freq in cmip_dict:
        cmip_dict[freq] = set(cmip_dict[freq])
    # end for
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
            data_request[key] = set(dict1[key]) | set(dict2[key])
        # end if
    # end for
    return data_request

def check_for_missing_fieldnames(masterset, data_request, request_name):
    """Given a data request dictionary (<data_request>),
    check to see if any are not in <masterset>.
    Return a set of missing fields names (an empty set means none).
    Print out any missing fields.
    Clean <data_request> to remove missing field entries (side effect)."""
    # Gather the set of all fields (combine different frequencies)
    all_reqfields = set()
    for fields in data_request.values():
        all_reqfields |= fields
    # end for
    # Any fields not in <masterlist> are missing
    missing = all_reqfields - masterset
    # Remove missing fields from data_request
    for key in data_request:
        data_request[key] -= missing
    # end for
    if missing:
        print(f"The following {len(missing)} fields are not output from CAM:")
        for field in sorted(missing):
            print(f"  {field}")
        # end for
        print(f"These fields were found in the {request_name} data request spreadsheet")
    # end if
    return missing

def generate_namelist_entries(data_request, usermod_config,
                              cosp_fieldnames, aerocom_fieldnames, maxline):
    """Write the sets of namelist entries represented by <data_request> to
    the usermods files defined in <usermod_config>."""
    for usermod in usermod_config.values():
        lbreak = ''
        if not os.path.exists(usermod.dirname):
            os.makedirs(usermod.dirname)
        # end if
        with open(usermod.namelist_file(), mode="w") as outfile:
            outfile.write(f"! CAM {usermod.name} diagnostic namelist entries\n\n")
            if usermod.include_aerocom:
                outfile.write("! Aerocom fields will be output for this run\n")
                outfile.write("use_aerocom = .true.\n\n")
            # end if
            outfile.write(f"! Only output fields listed in this file\n")
            outfile.write(f"empty_htapes = .true.\n\n")
            for freq in sorted(usermod.frequencies,
                               key=lambda x: _HIST_FILEORDER.index(x)):
                if freq in data_request:
                    # index is the fincl number for this frequency
                    index = _HIST_FILEORDER.index(freq) + 1
                    if freq == 'subhr':
                        avgflag = 'I'
                    else:
                        avgflag = 'A'
                    # end if
                    # Write history file config info
                    outfile.write(f"{lbreak}{_HIST_TITLES[freq]}\n")
                    outfile.write(f"nhtfrq({index}) = {_HIST_FRQCODES[freq]}\n")
                    outfile.write(f"mfilt({index}) = {_HIST_MFILT[freq]}\n")
                    fields = data_request[freq]
                    if not usermod.include_cosp:
                        fields -= cosp_fieldnames
                    # end if
                    if not usermod.include_aerocom:
                        fields -= aerocom_fieldnames
                    # end if
                    # Convert to sorted list
                    fields = sorted([quote_field(x, avgflag) for x in fields])
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

###############################################################################

if __name__ == "__main__":
    arglist = command_line(sys.argv[1:])
    cmipfile, camfile, usermods, configfile, overwrite, error, maxline = arglist
    # read configuration
    usermod_dict = read_config_file(configfile, usermods, overwrite)
    errmsg = "not producing any namelist usermods files"
    if usermod_dict:
        fieldnames = read_diagnostic_fieldnames()
        all_fieldnames, cosp_fieldnames, aerocom_fieldnames = fieldnames
        cmip7_request = parse_spreadsheet(cmipfile)
        missing7 = check_for_missing_fieldnames(all_fieldnames, cmip7_request,
                                                "CMIP7")
        cam_request = parse_spreadsheet(camfile)
        missingc = check_for_missing_fieldnames(all_fieldnames, cmip7_request,
                                                "CAM")
        if error and (missing7 or missingc):
            print(f"Missing fields found, {errmsg}")
        else:
            data_request = combine_data_requests(cmip7_request, cam_request)
            generate_namelist_entries(data_request, usermod_dict,
                                      cosp_fieldnames, aerocom_fieldnames,
                                      maxline)
        # end if
    else:
        print(f"Errors found in usermod config file, {errmsg}")
    # end if
    sys.exit(0)
