#!/bin/env python3
# SPDX-FileCopyrightText: Copyright (c) 2025 Andrew T. Walter <me@atwalter.com>
# SPDX-License-Identifier: MIT

"""

This script will automatically generate an binding file for the
functions exposed by the Z3 C API. This is required to make the
functions accessible to Lisp-Z3. To use this script, you must have the
pyclibrary package installed, as well as the headers for Z3's C API
(you should have this if you have Z3 installed).

The Lisp-Z3 system expects the binding file to exist at
`src/ffi/z3-api.lisp`.

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

`python scripts/gen_z3_api.py <z3-path> -o <output-file>`

where <output-file> denotes a path to the output file that should be
created. Note that if <output-file> exists and you would like to
overwrite it, you will also need to pass the -f flag. For example, to
regenerate the `z3-api.lisp` file that Lisp-Z3 expects, I run the
following:

`python scripts/gen_z3_api.py <z3-path> -o src/ffi/z3-api.lisp -f`

One reason why one might want to regenerate the `z3-api.lisp` file is
to make available additional functions that are not included in the
`z3-api.lisp` file that ships with Lisp-Z3. The set of functions that
we include bindings for is intentionally minimized in an attempt to
provide support for as wide a range of Z3 versions as possible.

"""

import sys
from pathlib import Path
import argparse
import re
from collections import namedtuple
from pyclibrary import CParser

type_mapping = {
    "VOID": ":void",
    "INT": ":int",
    "UINT": ":uint",
    "BOOL": ":bool",
    "STRING": ":string",
    "INT64": ":int64",
    "UINT64": ":uint64",
    "DOUBLE": ":double",
    "SYMBOL": "sym",
    "VOID_PTR": ":pointer",
    "CHAR_PTR": ":string",
}

def lispify_underscores(x):
    return x.replace('_', '-')

def fix_t(x):
    if x == "t":
        return "x"
    return x

def translate_in_type(ty):
    if ty in type_mapping:
        return type_mapping[ty]
    return lispify_underscores(ty.lower())

def translate_typespec(typespec):
    #print(typespec)
    match typespec[0]:
        case "in":
            return translate_in_type(typespec[1])
        case "in_array" | "out" | "out_array" | "out_array2" | "inout_array" | "fnptr":
            return ":pointer"
        case _:
            raise ValueError(f"Unknown typespec kind: {typespec[0]}")

def process_typespec(ty):
    m = re.match(r'(\w+)\((\w+(?:,\s?\w+)*)\)', ty)
    kind = m.group(1).removeprefix("_")
    args = list(map(str.strip, m.group(2).split(",")))
    args.append(kind)
    args.reverse()
    return args

def process_brief(brief):
    r'''Process supported markup in the text of a brief.
    Right now, this is only \c, which indicates the next word should be
    rendered as code and \ccode which does the same but for a block of
    text.
    In both cases, we'll just surround the argument with grave accents,
    Markdown-style.
    '''
    processed_c = re.sub(r'\\c (\w+)', lambda x: f'`{x.group(1)}`', brief)
    processed_ccode = re.sub(r'\\ccode\{([^}]+)\}', lambda x: f'`{x.group(1)}`', processed_c)
    return processed_ccode

FunctionSpec = namedtuple("FunctionSpec", ["name", "ret", "args", "brief"])

def process_defapi(line, brief, parser):
    parts = line.split(",", maxsplit=2)
    name = parts[0].strip("'\"")
    return_ty = parts[1].strip()
    args = re.findall(r"(\w+\(\w+(?:,\s?\w+)*\))", parts[2])
    processed_args = list(map(process_typespec, args))
    # Use the parsed C parameter names
    if name not in parser.defs['functions']:
        raise ValueError(f'Function {name} was defined in the comments of one of the Z3 API headers but not in the parsed C function definitions from the Z3 API headers!')
    fndef = parser.defs['functions'][name]
    # Hack to detect void argument lists
    if len(fndef[1]) == 1 and fndef[1][0][0] is None and fndef[1][0][1].type_spec == "void":
        param_names = []
    else:
        param_names = list(map(lambda arg: fix_t(lispify_underscores(arg[0])), fndef[1]))
    if len(param_names) != len(processed_args):
        raise ValueError(f'Unable to determine appropriate parameter names for function {name}')
    args = list(zip(param_names, processed_args))
    processed_brief = process_brief(brief)
    return FunctionSpec(name=name, ret=return_ty, args=args, brief=processed_brief)

def get_api_fns(f, parser):
    # This really should be written as a proper parser, but I
    # don't have the bandwidth to do so right now.
    # This ignores the "long description" for each function, which
    # we probably want to include at some point.
    # Also ignoring the section header comments (e.g. `@name Goals`)
    in_comment = False
    in_brief = False
    brief = []
    api_fns = []
    extra_fns = []
    for line in f:
        sline = line.strip()
        if sline.endswith("*/"):
            in_comment = False
            in_brief = False
            brief = []
        elif sline.startswith("/*"):
            in_comment = True
        elif in_comment:
            if sline.startswith("\\brief"):
                in_brief = True
                brief.append(sline.removeprefix("\\brief").strip())
            elif in_brief and len(sline) > 0:
                brief.append(sline)
            elif in_brief:
                in_brief = False
            elif sline.startswith("def_API("):
                api_fns.append(process_defapi(sline.removeprefix("def_API(").removesuffix(")"), ' '.join(brief), parser))
            elif sline.startswith("extra_API("):
                extra_fns.append(process_defapi(sline.removeprefix("extra_API(").removesuffix(")"), ' '.join(brief), parser))  
    return api_fns, extra_fns

def write_fn_to_lisp_file(f, fn, is_extra=False):
    macro_name = "defcfun"
    if is_extra:
        macro_name = "defcfun?"
    brief = fn.brief.replace('"', r'\"').replace('\\', '\\\\')
    args = '\n  '.join(map(lambda arg: f'({arg[0]} {translate_typespec(arg[1])})', fn.args))
    f.write(f'''\n({macro_name} "{fn.name}" {translate_in_type(fn.ret)}
  "{brief}"
  {args})
''')

lisp_file_header = ''';; THIS FILE IS AUTOGENERATED
;; See scripts/gen_z3_api.py for more information
(in-package :z3-c)

(defmacro defcfun? (name &rest args)
  "A version of defcfun that first checks whether the foreign function
exists, and warns if it does not."
  `(if (not (cffi:foreign-symbol-pointer ,name))
       (warn "Couldn't find function with name ~S, skipping..." ,name)
     (defcfun ,name ,@args)))
'''

def gen_lisp_file(f, api_fns, extra_fns):
    f.write(lisp_file_header)
    for api_fn in api_fns:
        write_fn_to_lisp_file(f, api_fn)
    for extra_fn in extra_fns:
        write_fn_to_lisp_file(f, extra_fn, is_extra=True)

def find_z3_headers(main_header):
    '''Find the list of header files that the main z3.h header file
    #includes.'''
    header_dir = Path(main_header).parent
    headers = []
    with open(main_header, 'r') as f:
        for line in f:
            if line.startswith('#include "'):
                header_name = line.lstrip('#include "').rstrip().rstrip('"')
                header_path = header_dir / header_name
                headers.append(str(header_path))
    return headers

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Automatically generate a file that contains function bindings for Z3 API functions.')
    parser.add_argument('header', type=Path, help='The z3.h header file to read from.')
    parser.add_argument('-o', '--output', type=Path, help='The file to write the generated function bindings to. Outputs to stdout if not provided.')
    parser.add_argument('-f', '--force', action='store_true', help='Force overwriting')
    args = parser.parse_args()
    if args.output is not None and not args.force and args.output.exists():
        print('The output file already exists! If you would like to overwrite it, provide the -f argument. Exiting...')
        sys.exit(1)
    headers = find_z3_headers(args.header)
    #headers = [args.header]
    if len(headers) == 0:
        print('Was unable to find the rest of the Z3 header files given the contents of the main header file. Exiting...')
        sys.exit(1)
    parser = CParser(headers)
    all_api_fns = []
    all_extra_fns = []
    for header in headers:
        with open(header, 'r') as f:
            api_fns, extra_fns = get_api_fns(f, parser)
            all_api_fns.extend(api_fns)
            all_extra_fns.extend(extra_fns)
    if args.output is None:
        gen_lisp_file(sys.stdout, all_api_fns, all_extra_fns)
    else:
        with open(args.output, 'w') as f:
            gen_lisp_file(f, all_api_fns, all_extra_fns)
