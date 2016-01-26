#!/usr/bin/python2
# -*- coding: utf8 -*-
""" Class for generation and manipulation of intermediate language instructions, including the final
transformation to assembly. """

from LatteErrors import InternalError
from Utils import switch, Flags


# code tokens ###################################################################################
class Codes(object):
    """ Some text constants and code generations helpers.

    Objects generated by this class act as an intermediate language before outputting the final
    assembly code. """

    # Instruction types.
    PUSH = 0
    POP = 1
    MOV = 2
    LEA = 3

    JUMP = 10  # bare jump to a label
    IF_JUMP = 11  # all kinds of conditional jumps
    LABEL = 12
    CALL = 13
    FUNC = 14  # function start: asm annotations, label, prologue, stack space for local vars
    ENDFUNC = 15  # standard function epilogue: pop local vars, asm 'leave' and 'ret'

    ADD = 20
    SUB = 21
    MUL = 22
    DIV = 23
    NEG = 24  # integer negation
    BOOL_OP = 25  # evaluate a boolean operator (as above)
    MOD = 26

    CHILD = 90  # a special instruction saying "insert i-th child code here"
    ASM = 91  # used for all special asm lines like .file, .text, .string, .globl, etc
    EMPTY = 92  # empty output line, just to improve assembly readability
    DELETED = 93  # assigned by optimizer to deleted actions (because deleting from list is linear)
    SCOPE = 94  # special token to annotate scope beginning
    ENDSCOPE = 95

    # String constants (to avoid typos)
    S_UNUSED_RESULT = 'unused result'  # to mark stack pops of unused data
    S_PROPAGATED = 'propagated'  # to mark propagated constants assigned back before a jump

    _CODE_NAMES = {
        0: ['PUSH', 'POP', 'MOV', 'LEA'],
        1: ['JUMP', 'IF_JUMP', 'LABEL', 'CALL', 'FUNC', 'ENDFUNC'],
        2: ['ADD', 'SUB', 'MUL', 'DIV', 'NEG', 'BOOL_OP', 'MOD'],
        9: ['CHILD', 'ASM', 'EMPTY', 'DELETED', 'SCOPE', 'ENDSCOPE'],
    }

    @classmethod
    def _code_name(cls, type):
        return cls._CODE_NAMES[type / 10][type % 10]

    var_size = 4  # every type uses 4 bytes for now

    _labels = 1

    @classmethod
    def new_label(cls):
        cls._labels += 1
        return '.L%d' % cls._labels

    @classmethod
    def is_child(cls, instr):
        return instr['type'] == cls.CHILD

    @classmethod
    def mkcode(cls, type, **kwargs):
        """ Construct a dict representing the intermediate code. Some arg naming conventions:

         * lhs: Loc containing value of left operand (so location will only be read)
         * rhs: Loc containing value of right operand AND where operator's result will be stored
         * src: Loc containing source of an operation (so location will only be read)
         * dest: Loc containing location where result will be stored (so it will *not* be read)
         * label: plaintext name of a label (for jumps, calls and label placement)
         * parts: special arg for the ASM code, contains the asm line to output as a list of words
         * comment: if present, will be appended to the output assembly line
         * tree: the corresponding LatteCode object, passed to FUNC and ENDFUNC for convenience. """
        d = kwargs.copy()
        d['type'] = type
        return d

    _strings = {}

    @classmethod
    def string_literal_label(cls, string):
        """ Return a label for a string literal ('.LCx'), but store each constant only once. """
        if string.value not in cls._strings:
            label = '.LC%d' % len(cls._strings)
            cls._strings[string.value] = label
            return label
        return cls._strings[string.value]

    @classmethod
    def gen_asm(cls, code_list):
        """ Generator of assembly instructions from a list of intermediate codes. """
        for code in code_list:
            for line in cls._asm_instr(code):
                yield line

    _scope_depth = 0

    @classmethod
    def _asm_instr(cls, code):
        """ Generator of assembly instructions from a single intermediate code. """
        for case in switch(code['type']):
            if case(cls.PUSH):
                yield cls._str_asm('pushl', [str(code['src'])], code)
                return
            if case(cls.POP):
                yield cls._str_asm('popl', [str(code['dest'])], code)
                return
            if case(cls.MOV):
                yield cls._str_asm('movl', [str(code['src']), str(code['dest'])], code)
                return
            if case(cls.LEA):
                yield cls._str_asm('leal', [str(code['src']), str(code['dest'])], code)
                return
            if case(cls.JUMP):
                yield cls._str_asm('jmp', [code['label']], code)
                return
            if case(cls.IF_JUMP):
                yield cls._str_asm('cmpl', [str(code['lhs']), str(code['rhs'])], code)
                yield cls._str_asm(code['op'], [code['label']], code)
                return
            if case(cls.LABEL):
                yield cls._str_asm(code['label'] + ':', [], code)
                return
            if case(cls.CALL):
                yield cls._str_asm('call', [code['label']], code)
                return
            if case(cls.FUNC):
                # asm declaration
                yield cls._str_asm('.globl', [code['label']], code)
                yield cls._str_asm('.type', [code['label'], '@function'], code)
                # function label
                yield cls._str_asm(code['label'] + ':', [], code)
                # standard prologue
                yield cls._str_asm('pushl', ['%ebp'], code)
                yield cls._str_asm('movl', ['%esp', '%ebp'], code)
                if code['tree'].var_count:
                    var_space = Loc.const((code['tree'].var_count) * cls.var_size)
                    yield cls._str_asm('subl', [str(var_space), '%esp'], code)
                return
            if case(cls.ENDFUNC):
                if code['tree'].var_count:
                    var_space = Loc.const((code['tree'].var_count) * cls.var_size)
                    yield cls._str_asm('addl', [str(var_space), '%esp'], code)
                yield cls._str_asm('leave', [], code)
                yield cls._str_asm('ret', [], {'comment': 'function ' + code['label']})
                return
            if case(cls.ADD, cls.SUB, cls.MUL):
                op = {cls.ADD: 'addl', cls.SUB: 'subl', cls.MUL: 'imull'}[code['type']]
                yield cls._str_asm(op, [str(code['lhs']), str(code['rhs'])], code)
                return
            if case(cls.DIV, cls.MOD):
                if not code['lhs'].is_reg() or code['lhs'].value in ['a', 'd']:
                    # the operand must be in a register other than %eax, %edx
                    yield cls._str_asm('movl', [str(code['lhs']), str(Loc.reg('c'))], code)
                    code['lhs'] = Loc.reg('c')
                if code['rhs'] != Loc.reg('a'):  # the first operand must be in %eax
                    yield cls._str_asm('movl', [str(code['rhs']), str(Loc.reg('a'))], code)
                yield cls._str_asm('cdq', [], code)
                yield cls._str_asm('idivl', [str(code['lhs'])], code)
                return
            if case(cls.NEG):
                yield cls._str_asm('negl', [str(code['rhs'])], code)
                return
            if case(cls.BOOL_OP):
                yield cls._str_asm('cmpl', [str(code['lhs']), str(code['rhs'])], code)
                yield cls._str_asm(code['op'], [str(Loc.reg('cmp'))], code)
                yield cls._str_asm('movzbl', [str(Loc.reg('cmp')), str(code['dest'])], code)
                return
            if case(cls.ASM):
                yield cls._str_asm(code['parts'][0], code['parts'][1:], code)
                return
            if case(cls.EMPTY):
                yield ''
                return
            if case(cls.DELETED):
                if Flags.debug:
                    d = code.copy()
                    del d['type']
                    yield '\t# [deleted] ' + cls._str_code(d)
                return
            if case(cls.SCOPE):
                if Flags.debug:
                    yield '# ' + (' ' * cls._scope_depth * 2) + '{'
                    cls._scope_depth += 1
                return
            if case(cls.ENDSCOPE):
                if Flags.debug:
                    cls._scope_depth -= 1
                    yield '# ' + (' ' * cls._scope_depth * 2) + '}'
                return
            if case(cls.CHILD):
                raise InternalError('code type %s not allowed here', cls._code_name(code['type']))
            if case():
                raise InternalError('invalid code: ' + str(code))

    @classmethod
    def _str_code(cls, code):
        """ Transform the intermediate instruction to string, for debug purposes. """
        res = []
        for key, value in sorted(code.iteritems()):
            val_str = str(value) if not isinstance(value, str) else value
            res.append(key + ': ' + val_str)
        return ", ".join(res)

    @classmethod
    def _str_asm(cls, instr, args, code):
        """ Transform an asm instruction into single asm line. """
        # indent every instruction apart from '.dot-directives' and 'labels:'
        if (instr[:1] != '.' or instr.startswith('.string')) and instr[-1:] != ':':
            instr = '\t' + instr
        # add comma-separated arguments
        result = instr + '\t' + ', '.join(args)
        # append optional data, if any
        if 'append' in code:
            result += code['append']
        # append comment at the end
        if 'comment' in code:
            result += '  # ' + code['comment']
        return result


class Loc(object):
    """ Helper class for Codes to represent various locations where a value can be. """
    reg_a = '%eax'
    reg_d = '%edx'
    reg_c = '%ecx'

    # Note: cdecl needs EBP, ESI, EDI, EBX preserved.
    top = '%esp'
    ebp = '%ebp'
    cmp = '%al'

    # type tokens
    CONST = 'const'
    REG = 'reg'
    MEM = 'mem'
    STRINGLIT = 'stringlit'

    # special token for value matching
    ANY = '__any__'

    def __init__(self, type, value):
        self.type = type
        self.value = value

    # factory methods
    @classmethod
    def const(cls, n):
        return cls(cls.CONST, n)

    @classmethod
    def reg(cls, which):
        return cls(cls.REG, which)

    @classmethod
    def mem(cls, addr, offset=None, idx=None, mult=None):
        """ Either copy full address from addr or construct memory location from
        addr, offset, idx, mult. """
        if addr == cls.ANY:
            return cls(cls.MEM, cls.ANY)
        if len(addr) and addr[-1] == ')':  # already computed address
            return cls(cls.MEM, addr)
        return cls(cls.MEM, cls.mkaddr(addr, offset, idx, mult))

    @classmethod
    def sym(cls, symbol):
        loc = cls.mem(symbol.pos)
        loc.sym = symbol
        return loc

    @classmethod
    def stringlit(cls, obj):
        if obj == cls.ANY:
            return cls(cls.STRINGLIT, cls.ANY)
        return cls(cls.STRINGLIT, Codes.string_literal_label(obj))

    # helper methods
    @classmethod
    def arg_addr(cls, n):
        """ Address of n-th function argument. """
        return cls.mkaddr(cls.ebp, Codes.var_size * (n+2))

    @classmethod
    def var_addr(cls, n):
        """ Address of n-th local variable on stack. """
        return cls.mkaddr(cls.ebp, Codes.var_size * (-1-n))

    @staticmethod
    def mkaddr(base, offset=None, idx=None, mult=None):
        """ Construct a memory location: offset(base, idx, mult) """
        return '%s(%s%s%s)' % (
            str(offset) if offset else '',
             base,
             ', %s' % idx if idx else '',
             ', %d' % mult if mult else '',
        )

    def is_constant(self):
        return self.type == self.CONST or self.type == self.STRINGLIT

    def is_reg(self):
        return self.type == self.REG

    def __str__(self):
        for case in switch(self.type):
            if case(self.CONST):
                return '$' + str(self.value)
            if case(self.REG):
                r = self.value
                if r in ['a', 'b', 'c', 'd']:
                    return '%e' + r + 'x'
                if r == 'cmp':
                    return '%al'
                if r == 'top':
                    return '%esp'
                if r in ['edi', 'esi', 'ebp']:
                    return '%' + r
                if r == self.ANY:
                    return r  # TODO remove after debugging
                raise InternalError('invalid register name: `%s`' % r)
            if case(self.MEM):
                return self.value
            if case(self.STRINGLIT):
                return '$' + self.value
            raise InternalError('invalid loc type: ' + self.type)

    def __eq__(self, other):
        """ Location matching, including possible `ANY` values. """
        if isinstance(other, Loc):
            if self.type != other.type:
                return False
            return self.value == self.ANY or other.value == self.ANY or self.value == other.value
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented:
            return result
        return not result

    def __hash__(self):
        # Warning: this class should not be hashable. Remember to not edit instances in a dict.
        return hash(str(self))
