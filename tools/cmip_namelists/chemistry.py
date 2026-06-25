#! /bin/env python3

"""Utilities for parsing and organizing diagnostic fieldnames for chemical
species in a given chemistry scheme.
"""
import glob
import re
import os

## Save our location
__MYDIR = os.path.abspath(os.path.dirname(__file__))
__CAMDIR = os.path.dirname(os.path.dirname(__MYDIR))
__CHEMDIR = os.path.join(__CAMDIR, "src", "chemistry")

## chemistry diag pre- and post-fixes
## Each list entry is a pair of possible new chemistry names
__CHEM_DIAG_PRE_POST = [('GS_', ''),        ('AQ_', ''),       ('AQ_', '_OCW'),
                        ('sink_', ''),      ('sink_', '_S'),   ('CT_', ''),
                        ('SF', ''),         ('emis_', ''),     ('D', 'CHM'),
                        ('F', '_fvm'),      ('TA', ''),        ('TM', ''),
                        ('VD', ''),         ('', 'CONU'),      ('', 'DDF'),
                        ('', 'DDV'),        ('', 'DTQ'),       ('', 'GVF'),
                        ('', 'INS'),        ('', 'SBC'),       ('', 'SBS'),
                        ('', 'SF'),         ('', 'SFSBC'),     ('', 'SFSBD'),
                        ('', 'SFSBS'),      ('', 'SFSEC'),     ('', 'SFSED'),
                        ('', 'SFSES'),      ('', 'SFSIC'),     ('', 'SFSID'),
                        ('', 'SFSIS'),      ('', 'SFWET'),     ('', 'SFWETC'),
                        ('', 'SIC'),        ('', 'SIS'),       ('', 'TBF'),
                        ('', 'WET'),        ('', 'WETC'),      ('', '_CHML'),
                        ('', '_CHMP'),      ('', '_SRF'),      ('', '_fvm'),
                        ('', '_mixnuc1'),   ('', '_num'),      ('', '_qneg3'),
                        ('', '_qneg3_col'), ('', '_sfcoag1'),  ('', '_sfcsiz3'),
                        ('', '_sfcsiz4'),   ('', '_sfgaex2'),  ('', '_OCWDDF'),
                        ('', '_OCWGVF'),    ('', '_OCWSFSBC'), ('', '_OCW'),
                        ('', '_OCWSFSBS'),  ('', '_OCWSFSIC'),
                        ('', '_OCWSFSIS'),  ('', '_OCWSFWET'), ('', '_OCWTBF'),
                        ('', '_OCW_mixnuc1'),
                        ('', '_OCWclcoagTend'),                ('', 'coagTend'),
                        ('', 'condTend'),   ('', '_CLXF'),     ('', '_CMXF'),
                        ('', '_XFRC'),      ('', 'clcoagTend'),
                        ('DC', ''),         ('WD_A_', ''),     ('cb_', ''),
                        ('cb_', '_OCW')]

__ESM_FIXED_FIELDS = {'CO2_OCN', 'CO2_FFF', 'CO2_LND', 'CO2'}
__ESM_DIAG_PRE_POST = [('', '_BOT'),       ('', '_fvm'),  ('', '_qneg3'),
                       ('', '_qneg3_col'), ('F', '_fvm'), ('SF', ''),
                       ('TA', ''),         ('TM', ''),    ('VD', '')]

## Find chem species in mo_sim_dat.F90
__SOLSYM_RE = re.compile(r"solsym[(][: 0-9]+[)] = [(]/(.*)$")
__END_SSYM_RE = re.compile(r"(.*)/[)]")

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

def parse_chem_spec_line(species_text):
    """Extract and return a list of chemical species names from the
    Fortran line fragment, <species_text>.
    """
    # Store species without quotes
    match_text = species_text.rstrip('[/), &]')
    slist = [x.strip("[' ]") for x in match_text.split(',')]
    return slist

def read_chem_species(chem_name):
    """Extract species names from the mo_sim_dat.F90 file from the
    chemistry scheme represented by <chem_name>. Return a list of
    species names extracted from solsym entries.
    """
    # Validate directory exists
    chem_src_dir = os.path.join(__CHEMDIR, f"pp_{chem_name}")
    if not os.path.isdir(chem_src_dir):
        emsg = f"ERROR: read_chem_species cannot find {chem_src_dir}"
        raise FileNotFoundError(emsg)
    # end if

    species_list = []
    filepath = os.path.join(chem_src_dir, "mo_sim_dat.F90")

    try:
        in_solsym = False
        with open(filepath, 'r') as infile:
            for line in infile:
                beg_match = __SOLSYM_RE.match(line.strip())
                end_match = __END_SSYM_RE.match(line.strip())
                if beg_match is not None:
                    slist = parse_chem_spec_line(beg_match.group(1))
                    species_list.extend(slist)
                    in_solsym = end_match is None
                    if not in_solsym:
                        # We are done with this file, one-line solsym def
                        exit
                    # end if
                elif in_solsym:
                    if end_match is not None:
                        in_solsym = False
                        slist = parse_chem_spec_line(end_match.group(1))
                    else:
                        slist = parse_chem_spec_line(line.strip())
                    # end if
                    species_list.extend(slist)
                    if not in_solsym:
                        # We are done with this file
                        exit
                    # end if
                # end if
            # end for
        # end with
    except FileNotFoundError:
        emsg = f"File mo_sim_dat.F90 not found in {chem_src_dir}"
        raise FileNotFoundError(emsg)
    # end try

    return species_list

def all_diags_set(species_list, pre_post_list):
    """Using a list of species names, <species_list>, create a set that
    includes these names plus each name decorated by the pre- and post-patterns
    from <pre_post_list>.
    Return this set of strings.
    """
    diag_names = set()
    for speci in species_list:
        diag_names.add(speci)
        for decor in pre_post_list:
            diag_names.add(f"{decor[0]}{speci}{decor[1]}")
        # end if
    # end for
    return diag_names

def all_chem_names(chem_name=None):
    """Return a set containing all chemistry diagnostic names for one
    chemistry scheme (chem_name==None) or for all chemistry schemes
    (chem_name!=None).
    """
    diag_names = set()
    if chem_name != None:
        all_species = read_chem_species(chem_name)
    else:
        all_chem_file = os.path.join(__MYDIR, "chemistry_fieldlist.txt")
        all_species = read_fieldname_file(all_chem_file)
    # end if
    diag_names = all_diags_set(all_species, __CHEM_DIAG_PRE_POST)
    return diag_names

def all_emission_names():
    """Return a set of all the diagnostic field names associated with the
    CO2 emission-driven constituent fields, including CO2.
    """
    return all_diags_set(__ESM_FIXED_FIELDS, __ESM_DIAG_PRE_POST)

###############################################################################

if __name__ == "__main__":
    all_specs = set()
    chem_dirs = glob.glob(os.path.join(__CHEMDIR, "pp_*"))
    for chem_name in [os.path.basename(x)[3:] for x in chem_dirs]:
        if chem_name != "none":
           all_specs |= set(read_chem_species(chem_name))
        # end if
    # end for
    print(f"{sorted(all_specs)}")
