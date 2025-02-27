#!/bin/env python3
# SPDX-FileCopyrightText: Copyright (c) 2025 Andrew T. Walter <me@atwalter.com>
# SPDX-License-Identifier: MIT

"""

This script will automatically generate a grovel file for the Z3 C API
types that the Lisp-Z3 interface requires. To use it, you must have the
pyclibrary package installed, as well as the headers for Z3's C API
(you should have this if you have Z3 installed).

The Lisp-Z3 system expects the grovel file to exist at
`src/ffi/z3-grovel.lisp`.

To use this script, you must provide the path to the z3.h header file.
The location of this file depends on your OS and how you installed Z3,
but here are a few options:

- If you installed Z3 using Homebrew, the path to the file should be
  the result of running `brew --prefix z3`, followed by `/include/z3.h`.

- If you installed Z3 using a Linux package manager, or by building from
  scratch, the file should be located at `/usr/include/z3.h` or
  `/usr/local/include/z3.h`.

Once you have the path to z3.h (denoted <z3-path> below), you can run
this script in the following way (assuming you're in the root directory
of this repo):

`python scripts/gen-z3-grovel.py <z3-path> -o <output-file>`

where <output-file> denotes a path to the output file that should be
created. Note that if <output-file> exists and you would like to
overwrite it, you will also need to pass the -f flag. For example, to
regenerate the `z3-grovel.lisp` file that Lisp-Z3 expects, I run the
following:

`python scripts/gen-z3-grovel.py <z3-path> -o src/ffi/z3-grovel.lisp -f`

One reason why one might want to regenerate the `z3-grovel.lisp` file is
to ensure that the enum types in that file match those in the version of
Z3 that you are using. The grovel file supplied with the Lisp-Z3 system
may not be fully up-to-date with the latest version of Z3, or you may be
trying to use an older version of Z3 than the grovel file was built to
support. In either case, you could regenerate the grovel file yourself
to match the version of Z3 you have installed.

"""

from pyclibrary import CParser
import sys
from pathlib import Path
import argparse
from util import find_z3_headers, get_typedef_enum, gather_enums

def remove_z3_prefix(name):
    return name.removeprefix("Z3_")

def output_enum_value(val, outfile):
    outfile.write(f'((:{remove_z3_prefix(val)} "{val}"))\n')

def process_enums(enums, outfile):
    for name, values in enums.items():
        outfile.write(f'(cenum ({name})\n')
        for value in values:
            outfile.write('       ')
            output_enum_value(value, outfile)
        outfile.write(')\n')

def process_opaque_pointers(defs, outfile):
    # For whatever reason pyclibrary seems to categorize the opaque
    # pointer types as "variables"
    for name in defs['variables'].keys():
        outfile.write(f'(ctype {name}_raw "{name}")\n')

def gen_grovel_file(defs, outfile):
    grovel_header = [
        ';; THIS FILE IS AUTOGENERATED',
        ';; See scripts/gen-z3-grovel.py for more information',
        '(in-package :z3-c-types)',
        '(include "z3.h")',
        '(feature z3-safe-errors "SAFE_ERRORS")'
    ]
    for line in grovel_header:
        outfile.write(line)
        outfile.write('\n')
    process_opaque_pointers(defs, outfile)
    enums = gather_enums(defs)
    process_enums(enums, outfile)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Automatically generate a grovel file for the Z3 C API types that the Lisp-Z3 interface requires.')
    parser.add_argument('header', type=Path, help='The z3.h header file to read from.')
    parser.add_argument('-o', '--output', type=Path, help='The file to write the generated grovel code to. Outputs to stdout if not provided.')
    parser.add_argument('-f', '--force', action='store_true', help='Force overwriting')
    args = parser.parse_args()
    if args.output is not None and not args.force and args.output.exists():
        print('The output file already exists! If you would like to overwrite it, provide the -f argument. Exiting...')
        sys.exit(1)
    headers = find_z3_headers(args.header)
    if len(headers) == 0:
        print('Was unable to find the rest of the Z3 header files given the contents of the main header file. Exiting...')
        sys.exit(1)
    parser = CParser(headers)
    if args.output is None:
        gen_grovel_file(parser.defs, sys.stdout)
    else:
        with open(args.output, 'w') as f:
            gen_grovel_file(parser.defs, f)
