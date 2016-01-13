#!/usr/bin/python2
# -*- coding: utf8 -*-

from bisect import bisect_left

from FuturePrint import debug
from LatteCodes import Codes, Loc


class LatteOptimizer(object):

    INF_PASSES = 100  # number of passes considered 'sufficiently infinite'

    def __init__(self, codes):
        self.codes = codes
        self.labels = {}  # Map from label name to position in codes.
        self.jumps = {}  # Map from label name to positions which jump to it.
        self.print_codes()
        self.scan_labels()

    def run_all(self):
        """ Optimizer main function, which runs the implemented optimizations on the codes. """
        debug('CODES: ', len(self.codes))
        self.run_opt(self.del_unused_results)
        self.run_opt(self.clear_deleted_codes)
        debug('CODES: ', len(self.codes))
        # TODO remove jump to next instr
        # TODO remove unjumped labels
        # TODO free string memory

    def run_opt(self, function, max_passes=1, **kwargs):
        """ A function to run a single optimization, possibly with many passes in a row.

        An optimization is ran again if it returned a non-zero value and it has been started less
        than max_passes times.
        Other keyword arguments are passed to the optimization function. """
        for count in xrange(max_passes):
            debug("--- OPT:", function.__name__, " pass:", count+1)
            ret = function(**kwargs)
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

    def _find_match_by_iterable(self, iterable, attrlist, **kwargs):
        """ Find 'next' matching code according to an arbitrary order defined by iterable.

        attrlist and kwargs specify matching rules as in match(). """
        for pos in iterable:
            if match(self.codes[pos], attrlist, **kwargs):
                return pos
        return None

    def find_prev_match(self, pos, attrlist, **kwargs):
        return self._find_match_by_iterable(reversed(xrange(pos)), attrlist, **kwargs)

    def find_next_match(self, pos, attrlist, **kwargs):
        return self._find_match_by_iterable(xrange(pos+1, len(self.codes)), attrlist, **kwargs)

    def get_jump_before(self, label, pos):
        """ Return position of last code jumping to a label *before* position pos. """
        ret = bisect_left(self.jumps.get(label, []), pos) - 1
        return self.jumps[label][ret] if ret >= 0 else None

    def del_unused_results(self, **kwargs):
        """ Find the stack pops that are marked as popping an unused result, and delete them along
        with the push that causes them. """
        for pos in xrange(len(self.codes)):
            if match(self.codes[pos], type=Codes.ADD, comment=Codes.S_UNUSED_RESULT):
                # Found an unused result, trace back to all pushes that might lead to it.
                # We don't need to trace arbitrary jump sequences that lead there, as for now
                # the sequence should be either just [push, pop] or, for bool expr evaluation:
                # [push 1, jump after, ..., push 0, label after, pop]
                debug('unused result at', pos)
                self.codes[pos]['type'] = Codes.DELETED
                push_off = -1
                # first, try the two-push case: find the one before the jump
                if match(self.codes[pos-1], type=Codes.LABEL):
                    debug('   found label', self.codes[pos-1]['name'], 'before pop')
                    push_off = -2
                    jump_pos = self.get_jump_before(self.codes[pos-1]['name'], pos)
                    if jump_pos is None:
                        debug('   jump to label not found, ignoring')
                    elif match(self.codes[jump_pos-1], type=Codes.PUSH):
                        debug('   found jumped push at', jump_pos-1)
                        self.codes[jump_pos-1]['type'] = Codes.DELETED
                    else:
                        debug('   code at', jump_pos-1, 'is not a push, ignoring')
                # find the other push (or the only one if there was no label)
                if match(self.codes[pos+push_off], type=Codes.PUSH):
                    debug('   found push at', pos+push_off)
                    self.codes[pos+push_off]['type'] = Codes.DELETED
                else:
                    debug('   code at', pos+push_off, 'is not a push, ignoring')
        return 0

    def clear_deleted_codes(self, **kwargs):
        """ Really delete the codes marked DELETED. """
        old_len = len(self.codes)
        self.codes = filter(lambda code: not match(code, type=Codes.DELETED), self.codes)
        debug('pruned %d deleted codes' % (old_len - len(self.codes)))
        # label maps need to be recalculated after deleting
        self.scan_labels()
        return 0


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
