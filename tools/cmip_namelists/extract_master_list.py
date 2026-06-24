#! /bin/env python3

"""Script to read a CAM log file and extract the complete (master) list of
available diagnostic (history) field names.
Note that the log file must bu unzipped
"""

import argparse
import os
import re
import sys

_BEGIN_MASTER_LIST_RE = re.compile(r"[ ]*[*]+ MASTER FIELD LIST [*]+")
_END_MASTER_LIST_RE = re.compile(r"[ ]*intht:nfmaster=")
_FIELDLINE_RE = re.compile(r"[ ]*[0-9]+[ ]*([A-Za-z0-9_&]+)")

def command_line(args):
    """Read the command line arguments (args) to retrieve the path to the
    log file to read and command options.
    Return the path to the log file and a boolean for optional print."""
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("log_filepath", metavar='<path to CAM logfile>', type=str)
    parser.add_argument("--write-names", action='store_true', default=False,
                        help="Write diagnostic fieldnames to standard output")

    pargs = parser.parse_args(args)
    return pargs.log_filepath, pargs.write_names

def parse_diagnostic_fieldnames(logfile, save_names=False):
    """Parse a CAM <logfile> to collect and return a list of all the available
    diagnostic (history) fieldnames for that CAM configuration.
    Fields that only appear on CAM initial condition files are ignored.
    If <save_names> is True, write the fieldnames to standard output."""

    all_fieldnames = []
    in_masterlist = False
    linenum = 0
    with open(logfile, mode='r') as infile:
        for line in infile:
            linenum += 1
            if in_masterlist:
                if _END_MASTER_LIST_RE.match(line) is not None:
                    in_masterlist = False
                    break
                else:
                    fieldmatch = _FIELDLINE_RE.match(line)
                    if fieldmatch is not None:
                        fldname = fieldmatch.group(1)
                        if '&IC' not in fldname:
                            all_fieldnames.append(fldname)
                        # end if
                    else:
                        raise ValueError(f"Unreadable line on line {linenum}")
                    # end if
                # end if
            elif _BEGIN_MASTER_LIST_RE.match(line) is not None:
                in_masterlist = True
            # end if
        # end for
    # end with
    if save_names:
        maxlen=max([len(x) for x in all_fieldnames])
        num_on_line = int(82/(maxlen + 2))
        fieldnum = 0
        num_fields = len(all_fieldnames)
        while fieldnum < num_fields:
            line = ""
            for index in range(fieldnum, min(fieldnum + num_on_line, num_fields)):
                fieldname = all_fieldnames[index]
                pad = " "*(maxlen + 2 - len(fieldname))
                line += f"{fieldname}{pad}"
            # end for
            print(line.strip())
            fieldnum += num_on_line
        # end while
    # end if
    return all_fieldnames

###############################################################################

if __name__ == "__main__":
    logfile, save_names = command_line(sys.argv[1:])
    fieldnames = parse_diagnostic_fieldnames(logfile, save_names)
    print(f"{len(fieldnames)} diagnostic fieldnames found in {logfile}")
    sys.exit(0)
