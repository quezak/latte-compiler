#!/usr/bin/python2
# -*- coding: utf8 -*-
import argparse

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
        elif self.value in args: # changed for v1.5, see below
            self.fall = True
            return True
        else:
            return False


class Flags(object):
    """ A simple class to hold values of global compiler flags. """
    # TODO turn off debug by default
    debug = True
    input_file = None

    @classmethod
    def parse_args(cls, argv):
        parser = argparse.ArgumentParser(description="Latte x86 compiler", prog="latc_x86")
        parser.add_argument("input_file", help="Latte source file ('-' for stdin)")
        # TODO output files
        parser.add_argument("-d", "--debug", action="store_true", help="print debug messags")
        parser.parse_args(namespace=cls) # Saves values in this class, exits on error.

    @classmethod
    def input_from_stdin(cls):
        return cls.input_file == '-'
