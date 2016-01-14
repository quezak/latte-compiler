#!/usr/bin/python2
# -*- coding: utf8 -*-

from bisect import bisect_left
from itertools import izip, islice, imap

from FuturePrint import debug
from LatteCodes import Codes as CC, Loc
from LatteErrors import Status, LatteError
from Utils import Flags


class AnyOf(object):
    """ Helper class to express match alternatives, passed to match() as argument of any kwarg. """
    def __init__(self, *args):
        self.options = args

    def __eq__(self, other):
        return any(map(lambda x: _match_multi(x, other), self.options))

    def __ne__(self, other):
        return not self.__eq__(other)


class LatteOptimizer(object):

    INF_PASSES = 100  # number of passes considered 'sufficiently infinite'
    # Codes that 'don't do anything', e.g. if there are no other codes between a jump and its
    # label, the jump can be safely deleted.
    NOOPS = AnyOf(CC.LABEL, CC.EMPTY, CC.DELETED)
    # Codes that do an operation but no flow control or stack operations, so in [push, <op>, pop]
    # push and pop can be combined into mov if the operation's arguments are unrelated.
    NO_STACK_OPS = AnyOf(CC.MOV, CC.ADD, CC.SUB, CC.MUL, CC.NEG)
    # All locations considered constant.
    CONST_LOCS = AnyOf(Loc.const(Loc.ANY), Loc.stringlit(Loc.ANY))

    def __init__(self, codes):
        self.codes = codes
        self.labels = {}  # Map from label name to position in codes.
        self.jumps = {}  # Map from label name to positions which jump to it.
        self.print_codes()
        self.scan_labels()
        self.opt_counters = {}

    def run_all(self):
        """ Optimizer main function, which runs the implemented optimizations on the codes. """
        self.run_opt(self.del_unused_results)
        self.run_opt(self.del_jumps_to_next, max_passes=self.INF_PASSES)
        self.run_opt(self.del_unused_labels)
        self.run_opt(self.reduce_push_pop, max_passes=self.INF_PASSES)
        self.run_opt(self.propagate_constants)
        # TODO free string memory
        #self.run_opt(self.clear_deleted_codes)
        if Flags.optimizer_summary:
            Status.add_note(LatteError('optimizer case counters:'))
            for name, count in self.opt_counters.iteritems():
                Status.add_note(LatteError(name + ': ' + str(count)))

    def run_opt(self, function, max_passes=1, **kwargs):
        """ A function to run a single optimization, possibly with many passes in a row.

        An optimization is ran again if it returned a non-zero value and it has been started less
        than max_passes times.
        Other keyword arguments are passed to the optimization function. """
        for count in xrange(max_passes):
            name = function.__name__
            debug('--- OPT:', name, 'pass %d (of max %d)' % (count, max_passes))
            ret = function(**kwargs)
            # sum the optimization results, assuming value returned is number of cases resolved
            self.opt_counters[name] = self.opt_counters.get(name, 0) + ret
            debug('--- OPT:', name, 'returned', ret or 'finish')
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
                self.labels[code['label']] = i
            else:
                if match(code, type=AnyOf(CC.JUMP, CC.IF_JUMP, CC.CALL)):
                    # functions begin with labels -- collect their calls, it might be useful later
                    label = code['label']
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

    def _gen_match_by_iterable(self, iterable, attrlist, **kwargs):
        """ Generate indexes of matching codes according to an arbitrary order defined by iterable.

        attrlist and kwargs specify matching rules as in match(). """
        # TODO maybe rewrite these with some itertools
        for pos in iterable:
            if match(self.codes[pos], attrlist, **kwargs):
                yield pos

    def gen_prev_match(self, pos, min_pos=0, attrlist=[], **kwargs):
        for c in self._gen_match_by_iterable(reversed(xrange(min_pos, pos)), attrlist, **kwargs):
            yield c

    def gen_next_match(self, pos, max_pos=None, attrlist=[], **kwargs):
        for c in self._gen_match_by_iterable(xrange(pos+1, max_pos or len(self.codes)),
                                             attrlist, **kwargs):
            yield c

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
        result = 0
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
                    debug('   found label', self.codes[pos-1]['label'], 'before pop')
                    push_off = -2
                    jump_pos = self.find_jump_before(self.codes[pos-1]['label'], pos)
                    if jump_pos is None:
                        debug('   jump to label not found, ignoring')
                    elif match(self.codes[jump_pos-1], type=CC.PUSH):
                        debug('   found jumped push at', jump_pos-1)
                        self.mark_deleted(jump_pos-1)
                        result += 1
                    else:
                        debug('   code at', jump_pos-1, 'is not a push, ignoring')
                # find the other push (or the only one if there was no label)
                if match(self.codes[pos+push_off], type=CC.PUSH):
                    debug('   found push at', pos+push_off)
                    self.mark_deleted(pos+push_off)
                    result += 1
                else:
                    debug('   code at', pos+push_off, 'is not a push, ignoring')
        return result

    def clear_deleted_codes(self, **kwargs):
        """ Really delete the codes marked DELETED. """
        old_len = len(self.codes)
        self.codes = filter(lambda code: not match(code, type=CC.DELETED), self.codes)
        debug('pruned %d deleted codes' % (old_len - len(self.codes)))
        # label maps need to be recalculated after deleting
        self.scan_labels()
        return old_len - len(self.codes)

    def del_jumps_to_next(self, **kwargs):
        """ Delete jump codes that can be safely omitted (passed through). If the jump is
        conditional, the comparision itself is also deleted. """
        result = 0
        start_pos = 0
        for pos in self.gen_next_match(start_pos, type=AnyOf(CC.JUMP, CC.IF_JUMP)):
            start_pos = pos
            label = self.codes[pos]['label']
            # check if there is any 'non-noop' code between the jump and its label
            # (keep in mind that the label may be before the jump)
            start = min(pos, self.labels[label])
            stop = max(pos, self.labels[label])
            try:
                op_pos = self.gen_next_match(start, stop, negate=True, type=self.NOOPS).next()
            except StopIteration:
                debug('skipping', self.codes[pos].get('op', 'jmp'), 'to', label, 'at', pos)
                result += 1
                self.mark_deleted(pos)
        return result

    def del_unused_labels(self, **kwargs):
        """ Delete unnecessary labels (ones without jumps to them). """
        # First, rescan labels to remove deleted jumps from map
        self.scan_labels()
        result = 0
        for label, pos in self.labels.iteritems():
            # don't consider labels starting function -- unused function should already be deleted
            if label[0] == '.' and label not in self.jumps:
                debug('deleting unused label', label, 'at', pos)
                self.mark_deleted(pos)
                result += 1
        return result

    def reduce_push_pop(self, **kwargs):
        """ Optimize push-pop sequences. Detailed cases:

        * delete sequences [push X, pop X]
        * combine [push X, pop Y] into [mov X Y] -- the pop destination is always a register """
        result = 0
        for indexes in match_seq(self.codes, [code_spec(type=CC.PUSH), code_spec(type=CC.POP)]):
            debug('push-pop sequence:', str(indexes))
            result += self._do_push_pop_reduction(indexes)
        for indexes in match_seq(self.codes, [code_spec(type=CC.PUSH),
                                              code_spec(type=self.NO_STACK_OPS),
                                              code_spec(type=CC.POP)]):
            debug('push-op-pop sequence:', str(indexes))
            p_push, p_op, p_pop = indexes
            src, dest = self.codes[p_push]['src'], self.codes[p_pop]['dest']
            # do the reduction only if op's arguments do nothing to src and dest locations
            if src not in self.codes[p_op].values() and dest not in self.codes[p_op].values():
                result += self._do_push_pop_reduction([p_push, p_pop])
        return result

    def _do_push_pop_reduction(self, indexes):
        """ Child function of reduce_push_pop, that does the actual reduction. Separate function just
        because for code reuse. `indexes` should be a two-element position list."""
        p_push, p_pop = indexes
        src, dest = self.codes[p_push]['src'], self.codes[p_pop]['dest']
        if src == dest:
            debug('   deleting push-pop with same attrs at', str(indexes))
            self.mark_deleted(indexes)
            return 1
        else:
            debug('   combining [push, pop] to mov at', str(indexes))
            self.mark_deleted(p_push, _type='[push,pop]', dest=dest)
            self.codes[p_pop] = CC.mkcode(CC.MOV, src=src, dest=dest,
                                          comment='combined from [push,pop]')
            return 1
        return 0

    def propagate_constants(self, **kwargs):
        """ Propagate constants moved to registers to the place where they're used. """
        # Note: some codes, e.g. division, require arguments in registers.
        # After deleting a [mov const, reg], hold the const value in pocket and paste it into all
        # reg occurences until it's assigned something else.
        # Note: remember to empty your pockets when jumping :) (and calling)
        # In particular: before a label or jump assign the value to the register anyway.
        # TODO this can be sometimes avoided if we consider the whole jump graph and live vars...
        # Also remember that division requires both arguments in registers.
        # For result, count when a register is replaced with a value from pocket.
        # TODO another level: propagate up operators and if_jumps if both operands are constants.
        self.print_codes()
        result = 0
        pocket = {}
        apply_needed = False
        for pos in code_iter(self.codes):
            code = self.codes[pos]
            if len(pocket):
                # [0] On integer division, drop the first operand back to %eax and second to src.
                # Also, invalidate %eax and %edx values in pocket as idivl stores result there.
                if code['type'] == CC.DIV:
                    dropped = {reg: val for reg, val in pocket.iteritems()
                               if reg in [Loc.reg('a'), code['src']]}
                    debug('div instruction at %d, applying regs %s' % (
                        pos, str(map(str, dropped.keys()))))
                    if len(dropped):
                        self._add_to_apply_pocket(pos, dropped)
                        apply_needed = True
                    for reg in [Loc.reg('a'), Loc.reg('d')]:
                        if reg in pocket:
                            del pocket[reg]
                    continue
                # [1] Apply values from pocket first, in case of e.g. [mov $1 %eax, mov %eax %edx].
                # Only attrs 'src', 'lhs' can use a const value.
                for attr in set(['src', 'lhs']).intersection(code.keys()):
                    if not code[attr].is_reg() or code[attr] not in pocket:
                        continue
                    debug('attr %s is reg %s at %d, applying %s from pocket' % (
                        attr, code[attr].value, pos, pocket[code[attr]].value))
                    code[attr] = pocket[code[attr]]
                    result += 1
                # [2] If a register is assigned something, delete its entry in pocket.
                # Only attrs modifying their location are 'dest', 'rhs'.
                # TODO hack note: 'rhs' needs to be reviewed *first*, in case both rhs and dest are
                # the same register -- it would get deleted too early. Fix this when rewriting this
                # ugly function.
                for attr in reversed(sorted(set(['rhs', 'dest']).intersection(code.keys()))):
                    if not code[attr].is_reg() or code[attr] not in pocket:
                        continue
                    value = pocket[code[attr]].value
                    # NEG is a special case here: 'dest' is both source and destination -- but the
                    # value remains constant, so remove the code and adjust value in pocket.
                    if code['type'] == CC.NEG:
                        new_value = value[1:] if '-' in value else '-' + value
                        debug('NEG reg %s with const at %d, adjusting pocket value to %s' % (
                            code[attr].value, pos, new_value))
                        # Delete and re-insert value, to maintain hash properties.
                        loc = pocket[code[attr]]
                        del pocket[code[attr]]
                        loc.value = new_value
                        pocket[code[attr]] = loc
                        self.mark_deleted(pos, comment=CC.S_PROPAGATED)
                        result += 1
                    else:  # otherwise, just forget the register's value from pocket.
                        # but the right operand for cmpl also needs to be dropped.
                        debug('reg %s is %s at %d, forgetting from pocket' % (
                            code[attr].value, attr, pos))
                        if attr == 'rhs' and code['type'] in [CC.IF_JUMP, CC.BOOL_OP]:
                            self._add_to_apply_pocket(pos, {code['rhs']: pocket[code['rhs']]})
                            apply_needed = True
                            debug('   ^ but applying reg %s' % code['rhs'].value)
                        del pocket[code[attr]]
                # [3] On function call, empty the pocket.
                if len(pocket) and code['type'] == CC.CALL:
                    debug('function call at %d, emptying pocket' % pos)
                    pocket = {}
                # [4] On function exit, drop %eax and empty pocket.
                if len(pocket) and code['type'] == CC.LEAVE:
                    if Loc.reg('a') in pocket.keys():
                        self._add_to_apply_pocket(pos, {Loc.reg('a'): pocket[Loc.reg('a')]})
                        apply_needed = True
                        debug('LEAVE at %d, dropping reg a' % pos)
                    pocket = {}
                # [5] On jump instructions (both in-/out-bound) assign the pocket values anyway.
                if len(pocket) and code['type'] in [CC.JUMP, CC.IF_JUMP, CC.LABEL]:
                    debug('%s at %d, reassigning pocket values' % (CC._code_name(code['type']),
                                                                   pos))
                    # We can't insert into a list while iterating, so save the pocket for now
                    # TODO we could later skip at least some of pocket's values, if we check that
                    # a value is the same at label and all jumps to it, or the register is not live.
                    self._add_to_apply_pocket(pos, pocket.copy())
                    apply_needed = True
                    pocket = {}
            # [6] Finally, when moving a constant to a register, stow it in the pocket instead.
            #if code['type'] == CC.MOV:
                #import ipdb; ipdb.set_trace() 
            if (match(code, type=CC.MOV, src=self.CONST_LOCS, dest=Loc.reg(Loc.ANY)) and
                    not match(code, comment=CC.S_PROPAGATED)):
                debug('mov const %s -> reg %s found at %d' % (code['src'].value,
                                                              code['dest'].value, pos))
                #import ipdb; ipdb.set_trace() 
                pocket[code['dest']] = code['src']
                self.mark_deleted(pos, comment=CC.S_PROPAGATED)
        # Turn the pocket indicators into assignments
        if apply_needed:
            self._insert_apply_pockets()
        return result

    def _add_to_apply_pocket(self, pos, pocket):
        debug('  _add_to_apply_pocket at %d:' % pos)
        for reg, val in pocket.iteritems():
            debug('\t', reg.value, '->', val.value)
        if 'apply_pocket' in self.codes[pos]:
            self.codes[pos]['apply_pocket'].update(pocket)
        else:
            self.codes[pos]['apply_pocket'] = pocket

    def _insert_apply_pockets(self):
        """ Child function of propagate_constants, used to insert assignments before pocket
        indicators, because they can't be inserted before when iterating through the list. """
        start_pos = 0
        debug('APPLY POCKETS')
        for pos in self.gen_next_match(start_pos, attrlist=['apply_pocket']):
            # Generate the move instructions and insert them inside codes list.
            moves = map(lambda (reg, val): CC.mkcode(CC.MOV, src=val, dest=reg,
                                                     comment=CC.S_PROPAGATED),
                        self.codes[pos]['apply_pocket'].iteritems())
            debug('apply pocket: insert %d moves at %d' % (len(moves), pos))
            del self.codes[pos]['apply_pocket']
            self.codes[pos:pos] = moves
            start_pos = pos + len(moves) - 1


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
        if key not in code or code[key] != value:
            return False
    return True


# TODO remove after debugging
def _match_multi(a, b):
    result = a == b
    #debug('_match_multi', result, 'a:', str(a), 'b:', str(b))
    return result


def code_spec(attrlist=[], **kwargs):
    """ Helper function to provide arguments to match_seq() the same way as to match(). """
    return (attrlist, kwargs)


def code_iter(codes, start_pos=0, end_pos=None):
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
