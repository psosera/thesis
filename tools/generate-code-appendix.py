#!/usr/bin/python

from __future__ import print_function

import os
from os.path import splitext, join
import sys

listing_template = r'''\noindent\large\texttt{{{0}}}
\begin{{minted}}[fontsize=\footnotesize,frame=lines]{{ocaml}}
{1}
\end{{minted}}

\begin{{minted}}[fontsize=\footnotesize,frame=lines]{{ocaml}}
{2}
\end{{minted}}
'''
TEST_EXT = '.ml'
BASELINE_EXT = '.out'

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def read(path, strip=False):
    with open(path) as f:
        lines = f.readlines()
        if strip:
            lines = lines[1:]
        return ''.join(lines).strip()

def print_usage(args):
    print("Usage: {0} <testdir>".format(args[0]))

def main(args):
    if len(args) == 2:
        root = args[1]
        if not os.path.exists(root):
            print_usage(args)
        elif os.path.exists(root) and os.path.isdir(root):
            for (base, test) in find_tests(root):
                src      = join(base, test + TEST_EXT)
                baseline = join(base, test + BASELINE_EXT)
                print(listing_template.format(join(base, test + TEST_EXT).replace('_', '\_'), read(src), read(baseline, strip=True)))
        else:
            print_usage(args)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
