#!/usr/bin/python2
# -*- coding: utf8 -*-

from bisect import bisect_left
from itertools import izip, islice, imap

from FuturePrint import debug
from LatteCodes import Codes as CC, Loc
from LatteErrors import Status


class LatteOptimizer(object):

    INF_PASSES = 100  # number of passes considered 'sufficiently infinite'
    # codes that 'don't do anything', e.g. if there are no other codes between a jump and its
    # label, the jump can be safely deleted.
    NOOPS = (CC.LABEL, CC.EMPTY, CC.DELETED)

    def __init__(self, codes):
        self.codes = codes
        self.labels = {}  # Map from label name to position in codes.
        self.jumps = {}  # Map from label name to positions which jump to it.
        self.print_codes()
        self.scan_labels()

    def run_all(self):
        """ Optimizer main function, which runs the implemented optimizations on the codes. """
        self.run_opt(self.del_unused_results)
        self.run_opt(self.del_jumps_to_next, max_passes=self.INF_PASSES)
        self.run_opt(self.del_unused_labels)
        self.run_opt(self.opt_push_pop, max_passes=self.INF_PASSES)
        # TODO free string memory
        # TODO constant propagation
        # self.run_opt(self.clear_deleted_codes)

    def run_opt(self, function, max_passes=1, **kwargs):
        """ A function to run a single optimization, possibly with many passes in a row.

        An optimization is ran again if it returned a non-zero value and it has been started less
        than max_passes times.
        Other keyword arguments are passed to the optimization function. """
        for count in xrange(max_passes):
            debug('--- OPT:', function.__name__, 'pass %d (of max %d)' % (count, max_passes))
            ret = function(**kwargs)
            debug('--- OPT:', function.__name__, 'returned', ret or 'finish')
            if not ret:
                break

    def print_codes(self):
        """ Debug: print the current codes list. """
        for i in xrange(len(self.codes)):
            code = self.codes[i]
            if (code['type'] == CC.EMPTY):
                debug('\n', no_hdr=True)
                continue
            d = code.copy()
            del d['type']
            debug('[%d]' % i, CC._code_name(code['type']) + '\t' + CC._str_code(d), no_hdr=True)

    def scan_labels(self):
        """ A function that indexes the labels and jumps in the given codes. """
        self.labels = {}
        self.jumps = {}
        for i in xrange(len(self.codes)):
            code = self.codes[i]
            if match(code, type=CC.LABEL):
                self.labels[code['name']] = i
            else:
                if match(code, type=(CC.JUMP, CC.IF_JUMP)):
                    label = code['dest']
                elif match(code, type=CC.CALL):
                    # functions begin with labels -- collect their calls, it might be useful later
                    label = code['name']
                elif match(code, type=CC.PUSH, src=Loc.stringlit(Loc.ANY)):
                    # collect uses of string constants
                    label = code['src'].value
                else:
                    continue
                if label in self.jumps:
                    self.jumps[label].append(i)
                else:
                    self.jumps[label] = [i]
        debug('--- label maps ---')
        for label in self.labels:
            debug('[%d]' % self.labels[label], label, ': ', str(self.jumps.get(label, None)))

    def _find_match_by_iterable(self, iterable, attrlist, **kwargs):
        # TODO maybe rewrite these with some itertools
        """ Find 'next' matching code according to an arbitrary order defined by `iterable`.

        attrlist and kwargs specify matching rules as in match(). """
        for pos in iterable:
            if match(self.codes[pos], attrlist, **kwargs):
                return pos
        return None

    def find_prev_match(self, pos, min_pos=0, attrlist=[], **kwargs):
        return self._find_match_by_iterable(reversed(xrange(min_pos, pos)), attrlist, **kwargs)

    def find_next_match(self, pos, max_pos=None, attrlist=[], **kwargs):
        return self._find_match_by_iterable(xrange(pos+1, max_pos or len(self.codes)),
                                            attrlist, **kwargs)

    def find_jump_before(self, label, pos):
        """ Return position of last code jumping to a label *before* position pos. """
        ret = bisect_left(self.jumps.get(label, []), pos) - 1
        return self.jumps[label][ret] if ret >= 0 else None

    def mark_deleted(self, pos_or_iter, **kwargs):
        """ Mark code for deletion, at a single index or whole iterable. """
        if isinstance(pos_or_iter, int):
            self.codes[pos_or_iter]['_type'] = CC._code_name(self.codes[pos_or_iter]['type'])
            self.codes[pos_or_iter].update(kwargs)  # just for debugging, to put more indicators
            self.codes[pos_or_iter]['type'] = CC.DELETED
        else:
            for pos in pos_or_iter:
                self.mark_deleted(pos)

    def del_unused_results(self, **kwargs):
        """ Find the stack pops that are marked as popping an unused result, and delete them along
        with the push that causes them. """
        for pos in xrange(len(self.codes)):
            if match(self.codes[pos], type=CC.ADD, comment=CC.S_UNUSED_RESULT):
                # Found an unused result, trace back to all pushes that might lead to it.
                # We don't need to trace arbitrary jump sequences that lead there, as for now
                # the sequence should be either just [push, pop] or, for bool expr evaluation:
                # [push 1, jump after, ..., push 0, label after, pop]
                debug('unused result at', pos)
                self.mark_deleted(pos)
                push_off = -1
                # first, try the two-push case: find the one before the jump
                if match(self.codes[pos-1], type=CC.LABEL):
                    debug('   found label', self.codes[pos-1]['name'], 'before pop')
                    push_off = -2
                    jump_pos = self.find_jump_before(self.codes[pos-1]['name'], pos)
                    if jump_pos is None:
                        debug('   jump to label not found, ignoring')
                    elif match(self.codes[jump_pos-1], type=CC.PUSH):
                        debug('   found jumped push at', jump_pos-1)
                        self.mark_deleted(jump_pos-1)
                    else:
                        debug('   code at', jump_pos-1, 'is not a push, ignoring')
                # find the other push (or the only one if there was no label)
                if match(self.codes[pos+push_off], type=CC.PUSH):
                    debug('   found push at', pos+push_off)
                    self.mark_deleted(pos+push_off)
                else:
                    debug('   code at', pos+push_off, 'is not a push, ignoring')
        return 0

    def clear_deleted_codes(self, **kwargs):
        """ Really delete the codes marked DELETED. """
        old_len = len(self.codes)
        self.codes = filter(lambda code: not match(code, type=CC.DELETED), self.codes)
        debug('pruned %d deleted codes' % (old_len - len(self.codes)))
        # label maps need to be recalculated after deleting
        self.scan_labels()
        return 0

    def del_jumps_to_next(self, **kwargs):
        """ Delete jump codes that can be safely omitted (passed through). If the jump is
        conditional, the comparision itself is also deleted. """
        result = 0
        for pos in xrange(len(self.codes)):
            if match(self.codes[pos], type=(CC.JUMP, CC.IF_JUMP)):
                label = self.codes[pos]['dest']
                # check if there is any 'non-noop' code between the jump and its label
                # (keep in mind that the label may be before the jump)
                start = min(pos, self.labels[label])
                stop = max(pos, self.labels[label])
                op_pos = self.find_next_match(start, stop, negate=True, type=self.NOOPS)
                if not op_pos:
                    debug('skipping', self.codes[pos].get('op', 'jmp'), 'to', label, 'at', pos)
                    result += 1
                    self.mark_deleted(pos)
        return result

    def del_unused_labels(self, **kwargs):
        """ Delete unnecessary labels (ones without jumps to them). """
        # First, rescan labels to remove deleted jumps from map
        self.scan_labels()
        for label, pos in self.labels.iteritems():
            # don't consider labels starting function -- unused function should already be deleted
            if label[0] == '.' and label not in self.jumps:
                debug('deleting unused label', label, 'at', pos)
                self.mark_deleted(pos)

    def opt_push_pop(self, **kwargs):
        """ Optimize push-pop sequences. Detailed cases:

        * delete sequences [push X, pop X]
        * combine [push X, pop Y] into [mov X Y] -- the pop destination is always a register """
        result = 0
        # TODO it might be nice to also detect e.g. [push, mov, pop] if mov's args are unrelated
        for indexes in match_seq(self.codes, [code_spec(type=CC.PUSH), code_spec(type=CC.POP)]):
            debug('push-pop sequence:', str(indexes))
            p_push, p_pop = indexes
            src, dest = self.codes[p_push]['src'], self.codes[p_pop]['dest']
            if src == dest:
                debug('   deleting push-pop with same attrs at', str(indexes))
                self.mark_deleted(indexes)
                result += 1
            else:
                debug('   combining [push, pop] to mov at', str(indexes))
                self.mark_deleted(p_push, _type='[push,pop]', dest=dest)
                self.codes[p_pop] = CC.mkcode(CC.MOV, src=src, dest=dest,
                                              comment='combined from [push,pop]')
                result += 1
        return result


def match(code, attrlist=[], negate=False, **kwargs):
    """ Return True if given code matches the specification:

    * contains every argument in attrlist (regardless of value)
    * for each attr=value pair in kwargs, code contains the same pair
    * for each attr=tuple pair in kwargs, code's value of attr is in tuple (tuples are not for code
      attr values) """
    if negate:
        return not match(code, attrlist, **kwargs)
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


def code_spec(attrlist=[], **kwargs):
    """ Helper function to provide arguments to match_seq() the same way as to match(). """
    return (attrlist, kwargs)


def code_iter(codes, start_pos, end_pos=None):
    """ Generator that yields all the codes in range that are not marked DELETED. """
    for pos in xrange(start_pos, end_pos or len(codes)):
        if not match(codes[pos], type=CC.DELETED):
            yield pos


def match_seq(codes, spec_list, start_pos=0, end_pos=None):
    """ A generator that yields occurences of a sequence of codes matching spec_list (tuples
    produced with code_spec), starting between start_pos and end_pos.

    Note: this is naive matching -- it's probably not worth writing KMP-like tricks here.
    After a match the matched codes are skipped, so no returned occurences overlap.
    Yielded value is a list of code indexes matched for the list. """
    end = min(end_pos or len(codes) - len(spec_list), len(codes) - len(spec_list))
    skip_to = 0
    for pos in xrange(start_pos, end):
        # if we returned a match, skip the matched region
        if pos < skip_to:
            continue
        # collect enough codes for a possible match
        indexes = [c for c in islice(code_iter(codes, pos), len(spec_list))]
        if len(indexes) < len(spec_list):
            break
        if all(imap(lambda (idx, (attrs, args)): match(codes[idx], attrs, **args),
                    izip(indexes, spec_list))):
            skip_to = indexes[-1] + 1
            yield indexes
