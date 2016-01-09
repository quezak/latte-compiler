#!/usr/bin/python2
# -*- coding: utf8 -*-
""" Message printing functions using print() from python3. """
from __future__ import print_function
import sys

import Utils

class Colors:
    """ ANSI coloring text for terminal output """
    @staticmethod
    def _colorIfTerm(color):
        if (not sys.stderr.isatty()) or (not Utils.Flags.output_colors): return ''
        return '\033[' + color + 'm'

    @classmethod
    def reset(cls): return cls._colorIfTerm('')

    @classmethod
    def _coloredText(cls, text, code):
        return cls._colorIfTerm(code) + text + cls.reset()

    @classmethod
    def debug(cls, text): return cls._coloredText(text, '1;30')

    @classmethod
    def note(cls, text): return cls._coloredText(text, '1')

    @classmethod
    def warning(cls, text): return cls._coloredText(text, '1;33')

    @classmethod
    def error(cls, text): return cls._coloredText(text, '1;31')

    @classmethod
    def pos(cls, text): return cls._coloredText(text, '35')

    @classmethod
    def quote(cls, text): return cls._coloredText('\'' + text + '\'', '36')

def debug(*objs):
    if Utils.Flags.debug:
        print(Colors.debug('[dbg]'), *objs, file=sys.stderr)

def message(*objs):
    print(*objs, file=sys.stderr)

def note(*objs):
    print(Colors.note('note:'), *objs, file=sys.stderr)

def warning(*objs):
    print(Colors.warning('warning:'), *objs, file=sys.stderr)

def error(*objs):
    print(Colors.error('ERROR:'), *objs, file=sys.stderr)
