#!/usr/bin/python2
# -*- coding: utf8 -*-
""" A helper class for matching sequences of instructions, used by the optimizer. """

from itertools import izip, islice, imap

from LatteCodes import Codes as CC


class AnyOf(object):
    """ Helper class to express match alternatives, passed to match() as argument of any kwarg. """
    def __init__(self, *args):
        self.options = args

    def __eq__(self, other):
        return any(map(lambda x: x == other, self.options))

    def __ne__(self, other):
        return not self.__eq__(other)


def code_spec(attrlist=[], **kwargs):
    """ Helper function to provide arguments to match_seq() the same way as to match(). """
    return (attrlist, kwargs)


class CodeMatcher(object):

    # Codes considered blank, e.g. can be skipped in iteration.
    BLANK = AnyOf(CC.EMPTY, CC.DELETED, CC.SCOPE, CC.ENDSCOPE)

    def __init__(self, optimizer):
        # Reference to optimier, so we always have access to current codes list
        self.optimizer = optimizer

    def codes(self):
        return self.optimizer.codes

    def code(self, pos):
        return self.optimizer.codes[pos]

    def len_codes(self):
        return len(self.optimizer.codes)

    @staticmethod
    def match(code, attrlist=[], negate=False, **kwargs):
        """ Return True if given code matches the specification:

        * contains every argument in attrlist (regardless of value)
        * for each attr=value pair in kwargs, code contains the same pair
        * for each attr=AnyOf pair in kwargs, code's value of attr is any of the values provided."""
        if negate:
            return not CodeMatcher.match(code, attrlist, **kwargs)
        for attr in attrlist:
            if attr not in code:
                return False
        for key, value in kwargs.iteritems():
            if key not in code or code[key] != value:
                return False
        return True

    def match_at(self, pos, attrlist=[], **kwargs):
        return self.match(self.code(pos), attrlist, **kwargs)

    def code_iter(self, start_pos=0, end_pos=None):
        """ Generator that yields all the codes in range that are not marked DELETED. """
        for pos in xrange(start_pos, end_pos or self.len_codes()):
            if not self.match(self.code(pos), type=self.BLANK):
                yield pos

    def gen_seq(self, spec_list, start_pos=0, end_pos=None):
        """ A generator that yields occurences of a sequence of codes matching spec_list (tuples
        produced with code_spec), starting between start_pos and end_pos.

        Note: this is naive matching -- it's probably not worth writing KMP-like tricks here.
        After a match the matched codes are skipped, so no returned occurences overlap.
        Yielded value is a list of code indexes matched for the list. """
        end = min(end_pos or (self.len_codes() - len(spec_list)), self.len_codes() - len(spec_list))
        skip_to = 0
        for pos in xrange(start_pos, end):
            # if we returned a match, skip the matched region
            if pos < skip_to:
                continue
            # collect enough codes for a possible match
            indexes = [c for c in islice(self.code_iter(pos), len(spec_list))]
            if len(indexes) < len(spec_list):
                break
            if all(imap(lambda (idx, (attrs, args)): self.match(self.code(idx), attrs, **args),
                        izip(indexes, spec_list))):
                skip_to = indexes[-1] + 1
                yield indexes

    def _gen_by_iterable(self, iterable, attrlist, **kwargs):
        """ Generate indexes of matching codes according to an arbitrary order defined by iterable.

        attrlist and kwargs specify matching rules as in match(). """
        for pos in iterable:
            if self.match(self.code(pos), attrlist, **kwargs):
                yield pos

    def gen_prev(self, pos, min_pos=0, attrlist=[], **kwargs):
        for c in self._gen_by_iterable(reversed(xrange(min_pos, pos)), attrlist, **kwargs):
            yield c

    def gen_next(self, pos, max_pos=None, attrlist=[], **kwargs):
        for c in self._gen_by_iterable(xrange(pos+1, max_pos or self.len_codes()),
                                       attrlist, **kwargs):
            yield c
