#!/usr/bin/python2
# -*- coding: utf8 -*-

from FuturePrint import debug
from LatteCodes import Codes, Loc


class LatteOptimizer(object):

    def __init__(self, codes):
        self.codes = codes
        self.labels = {}  # Map from label name to position in codes.
        self.jumps = {}  # Map from label name to positions which jump to it.
        self.print_codes()
        self.scan_labels()

    def run_all(self):
        """ Optimizer main function, which runs the implemented optimizations on the codes. """
        pass
        self.run_opt(del_unused_results)
        self.run_opt(clear_deleted_codes)

    def run_opt(self, function, max_passes=1, **kwargs):
        """ A function to run a single optimization, possibly with many passes in a row.

        An optimization is ran again if it returned a non-zero value and it has been started less
        than max_passes times.
        Other keyword arguments are passed to the optimization function. """
        for count in xrange(max_passes):
            debug("--- OPT:", function.__name__, " pass:", count+1)
            ret = function(self.codes, **kwargs)
            if not ret:
                debug("--- OPT:", function.__name__, "returned zero")
                break

    def print_codes(self):
        """ Debug: print the current codes list. """
        for i in xrange(len(self.codes)):
            code = self.codes[i]
            if (code['type'] == Codes.EMPTY):
                debug('\n', no_hdr=True)
                continue
            d = code.copy()
            del d['type']
            debug('[%d]' % i, Codes._code_name(code['type']) + ': ' + str(d), no_hdr=True)

    def scan_labels(self):
        """ A function that indexes the labels and jumps in the given codes. """
        for i in xrange(len(self.codes)):
            code = self.codes[i]
            if match(code, type=Codes.LABEL):
                self.labels[code['name']] = i
            elif match(code, type=(Codes.JUMP, Codes.IF_JUMP, Codes.CALL)):
                # as functions begin with labels, collect also their calls, it might be useful later
                label = code['name'] if code['type'] == Codes.CALL else code['dest']
                if label in self.jumps:
                    self.jumps[label].append(i)
                else:
                    self.jumps[label] = [i]
        debug('--- label maps ---')
        for label in self.labels:
            debug('[%d]' % self.labels[label], label, ': ', str(self.jumps.get(label, None)))


def match(code, attrlist=[], **kwargs):
    """ Return True if given code matches the specification:

    * contains every argument in attrlist (regardless of value)
    * for each attr=value pair in kwargs, code contains the same pair
    * for each attr=tuple pair in kwargs, code's value of attr is in tuple (tuples are not for code
      attr values) """
    for attr in attrlist:
        if attr not in code:
            return False
    for key, value in kwargs.iteritems():
        if isinstance(value, tuple):  # a tuple of values that can match
            if key not in code or code[key] not in value:
                return False
        else:  # a single value that must match
            if key not in code or code[key] != value:
                return False
    return True


def del_unused_results(codes, **kwargs):
    """ Find the stack pops that are marked as popping an unused result, and delete them along with
    the push that causes them. """
    return 0  # TODO


def clear_deleted_codes(codes, **kwargs):
    """ Really delete the codes marked DELETED. """
    return 0  # TODO
