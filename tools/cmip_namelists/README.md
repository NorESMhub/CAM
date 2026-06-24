This directory contains scripts which help create specialized
diagnostic (history) output lists for CMIP7 experiments.

The master list of CAM fields is in the file,
**master_fieldlist.txt**. This file can be regenerated from a CAM
logfile using the script in **extract_master_list.py**:

```
./extract_master_list.py <path to logfile> --write-names > master_fieldlist.txt
```

The script, **cmip_diagnostic_namelists.py** is responsible for
reading the CMIP7 data request spreadsheet and creating CAM history
namelist files. This script collects all the global atmosphere
diagnostic field requests in the spreadsheet and groups them by
requested frequency of output. A check is then performed to look for
and omit any fields which are not available in the CAM master field
list. The rest of the fields are output as various `fincl<n>`
lists. Finally, the script outputs some general history namelist
variables, e.g., `nhtfrq`.

To run the script, first download a current version of the [CMIP7 data
request
spreadsheet](https://docs.google.com/spreadsheets/d/1XUdCTl1zKsWi_yTvMZqsMtBnnUPIhIeTPVSLHgL0Hdo) and the separate [CAM field request spreadsheet](https://docs.google.com/spreadsheets/d/1-ohp9nwJ5BUKQ7qxubg_PD1bVMWMMz00bS-MQ-z30p8/edit?gid=0#gid=0)
(File `==>` Download `==>` .csv). The script is then run as:

```
./cmip_diagnostic_namelists.py <cmip7-filename> < cam-filename> [ options ]
```

For the full interface (with options), see the help menu:
```
usage: cmip_diagnostic_namelists.py [--help]
```

To use this script, the following workflow is recommended:
1. Use `git rm` to remove existing usermods files (or use `--overwrite` and only remove files no longer in use).
2. Run script to generate new usermods files
3. Use `git add` to add any new usermods files
4. Use `git commit -a` to commit all the new usermods files. Note that git will only show untracked directory names if none of the files in that directory are currently in the repository.
