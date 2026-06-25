#! /bin/env python3

"""Script to read a CAM log file and extract the fixed list of
available diagnostic (history) field names (field names that should be
available in all NorESM3 simulations).
Note that the log file must be unzipped
"""

import argparse
import os
import re
import sys
## Local imports
from chemistry import all_chem_names, all_emission_names, read_fieldname_file

_BEGIN_FIXED_LIST_RE = re.compile(r"[ ]*[*]+ MASTER FIELD LIST [*]+")
_END_FIXED_LIST_RE = re.compile(r"[ ]*intht:nfmaster=")
_FIELDLINE_RE = re.compile(r"[ ]*[0-9]+[ ]*([A-Za-z0-9_&]+)")

## Save our location
__MYDIR = os.path.abspath(os.path.dirname(__file__))

def command_line(args):
    """Read the command line arguments (args) to retrieve the path to the
    log file to read and command options.
    Return the path to the log file and a boolean for optional print."""
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("log_filepath", metavar='<path(s) to CAM logfile(s)>',
                        type=str, action="extend", nargs="+")

    def_fixedlist = os.path.join(__MYDIR, "fixed_fieldlist.txt")
    parser.add_argument("--output-file", type=str, default=def_fixedlist,
                        help="Location for the new fixed diagnostic field list")

    pargs = parser.parse_args(args)
    return pargs.log_filepath, pargs.output_file

def parse_diagnostic_fieldnames(logfiles, output_file):
    """Parse one or more CAM <logfiles> to collect and return a list of
    all the available diagnostic (history) fieldnames for that CAM
    configuration.
    Fields that only appear on CAM initial condition files are ignored.
    Write all available diagnostic fieldnames to <output_file>"""

    all_fieldnames = set()
    in_masterlist = False
    exclude_names = all_chem_names() | all_emission_names()
    exclude_names |= set(read_fieldname_file("cosp_fieldlist.txt"))
    exclude_names |= set(read_fieldname_file("aerocom_fieldlist.txt"))
    linenum = 0
    for logfile in logfiles:
        with open(logfile, mode='r') as infile:
            for line in infile:
                linenum += 1
                if in_masterlist:
                    if _END_FIXED_LIST_RE.match(line) is not None:
                        in_masterlist = False
                        break
                    else:
                        fieldmatch = _FIELDLINE_RE.match(line)
                        if fieldmatch is not None:
                            fldname = fieldmatch.group(1)
                            if ('&IC' not in fldname) and (fldname not in exclude_names):
                                all_fieldnames.add(fldname)
                            # end if
                        else:
                            errmsg = "Unreadable line on line"
                            raise ValueError(f"{errmsg} {logfile}:{linenum}")
                        # end if
                    # end if
                elif _BEGIN_FIXED_LIST_RE.match(line) is not None:
                    in_masterlist = True
                # end if
            # end for
        # end with
    # end for
    # Convert the set into a sorted list
    all_fieldnames = sorted(all_fieldnames)
    maxlen=max([len(x) for x in all_fieldnames])
    num_on_line = int(82/(maxlen + 2))
    fieldnum = 0
    num_fields = len(all_fieldnames)
    with open(output_file, "w") as outfile:
        while fieldnum < num_fields:
            line = ""
            for index in range(fieldnum, min(fieldnum + num_on_line, num_fields)):
                fieldname = all_fieldnames[index]
                pad = " "*(maxlen + 2 - len(fieldname))
                line += f"{fieldname}{pad}"
            # end for
            outfile.write(f"{line.strip()}\n")
            fieldnum += num_on_line
        # end while
    # end if
    return all_fieldnames

###############################################################################

if __name__ == "__main__":
    logfiles, output_file = command_line(sys.argv[1:])
    fieldnames = parse_diagnostic_fieldnames(logfiles, output_file)
    if len(logfiles) == 1:
        msg = f"{len(logfiles)} logfile"
    else:
        msg = f"{len(logfiles)} logfiles"
    # end if
    print(f"{len(fieldnames)} diagnostic fieldnames found in {msg}")
    sys.exit(0)
