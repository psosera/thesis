#!/usr/bin/python

from __future__ import print_function
from jinja2 import *
import sys

def convert_to_table_lines(filename):
    with open(filename) as f:
        lines = f.readlines()
        lines = [line.replace(',', ' & ').replace('_', '\_') for line in lines]
        lines = [line[:len(line)-1] + ' \\\\' for line in lines]
        return lines

def main(args):
    if len(args) <> 3:
        print('Usage: {0} <template> <data file>'.format(args[0]))
    else:
        env = Environment(loader = FileSystemLoader('.'))
        template = env.get_template(args[1])
        lines = convert_to_table_lines(args[2])
        print(template.render(data = '\n'.join(lines), count = len(lines)))

if __name__ == '__main__':
    main(sys.argv)
