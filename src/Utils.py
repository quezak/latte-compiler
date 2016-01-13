#!/usr/bin/python2
# -*- coding: utf8 -*-
import argparse
from os.path import splitext

from LatteErrors import Status, LatteError


# A switch-like construction from z http://code.activestate.com/recipes/410692
# This class provides the functionality we want. You only need to look at
# this if you want to know how this works. It only needs to be defined
# once, no need to muck around with its internals.
class switch(object):
    def __init__(self, value):
        self.value = value
        self.fall = False

    def __iter__(self):
        """Return the match method once, then stop"""
        yield self.match
        raise StopIteration

    def match(self, *args):
        """Indicate whether or not to enter a case suite"""
        if self.fall or not args:
            return True
        elif self.value in args:  # changed for v1.5, see below
            self.fall = True
            return True
        else:
            return False


class Flags(object):
    """ A simple class to hold values of global compiler flags. """
    debug = False
    input_file = None
    bin_file = None
    asm_file = None
    runtime_file = 'runtime.o'
    output_colors = True
    run_optimizations = True

    @classmethod
    def parse_args(cls, argv):
        parser = argparse.ArgumentParser(description='Latte x86 compiler', prog='latc_x86')
        parser.add_argument('input_file',
                            help='Latte source file (\'-\' for stdin, implies asm_output=\'-\')')
        parser.add_argument('-o', '--output', dest='bin_file', help='custom output executable file')
        parser.add_argument('-s', '--asm_output', dest='asm_file', help="""custom output assembly
                            file ('-' for stdout, does not create executable)""")
        parser.add_argument('-d', '--debug', action='store_true', help="""print debug messages
                            and output messages immediately (in particular, omit the requirement for
                            'OK'/'ERROR' to be in the first line)""")
        parser.add_argument('-r', '--runtime', dest='runtime_file', default='runtime.o',
                            help='path to latte runtime library (default runtime.o)')
        parser.add_argument('-C', '--no-color', dest='output_colors', action='store_false',
                            help='disable output coloring')
        parser.add_argument('-N', '--no-optimizations', dest='run_optimizations',
                            action='store_false', help='disable optimizations')
        parser.parse_args(namespace=cls)  # Saves values in this class, exits on error.
        # Set output file names, if not provided.
        if not cls.asm_file:
            (cls.asm_file, _) = splitext(cls.input_file)
            if (cls.asm_file != '-'):
                cls.asm_file += '.s'
            if cls.asm_file == cls.input_file and cls.asm_file != '-':
                Status.add_error(
                    LatteError('assembly output cannot be the same file as the source code'),
                    fatal=True)
        if (not cls.bin_file) and cls.asm_file != '-':
            (cls.bin_file, _) = splitext(cls.input_file)
            if cls.bin_file == cls.input_file:
                Status.add_error(
                    LatteError('output binary cannot be the same file as the source code'),
                    fatal=True)

    @classmethod
    def input_from_stdin(cls):
        return cls.input_file == '-'

    @classmethod
    def output_to_stdout(cls):
        return cls.asm_file == '-'
