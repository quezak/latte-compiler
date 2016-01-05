#!/usr/bin/python2
""" Message printing functions using print() from python3. """
from __future__ import print_function
import sys

def debug(*objs):
    print('[dbg]', *objs, file=sys.stderr)

def message(*objs):
    print(*objs, file=sys.stderr)

def note(*objs):
    print('note:', *objs, file=sys.stderr)

def warning(*objs):
    print('warning:', *objs, file=sys.stderr)

def error(*objs):
    print('ERROR:', *objs, file=sys.stderr)
