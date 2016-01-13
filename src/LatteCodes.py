#!/usr/bin/python2
# -*- coding: utf8 -*-

from LatteErrors import InternalError
from Utils import switch


# code tokens ###################################################################################
class Codes(object):
    """ Some text constants and code generations helpers.

    Objects generated by this class act as an intermediate language before outputting the final
    assembly code. """

    # Instruction types.
    PUSH = 0
    POP = 1
    MOV = 2

    JUMP = 10  # bare jump to a label
    IF_JUMP = 11  # all kinds of conditional jumps
    LABEL = 12
    CALL = 13
    ENTER = 14  # standard function prologue, not the asm 'enter' instruction
    LEAVE = 15  # standard function epilogue: asm 'leave' and 'ret'

    ADD = 20
    SUB = 21
    MUL = 22
    DIV = 23
    NEG = 24  # integer negation
    BOOL_OP = 25  # evaluate a boolean operator (as above)

    CHILD = 90  # a special instruction saying "insert i-th child code here"
    ASM = 91  # used for all special asm lines like .file, .text, .string, .globl, etc
    EMPTY = 92  # empty output line, just to improve assembly readability
    DELETED = 93  # assigned by optimizer to deleted actions (because deleting from list is linear)

    # String constants (to avoid typos)
    S_UNUSED_RESULT = 'unused result'

    _CODE_NAMES = {
        0: ['PUSH', 'POP', 'MOV'],
        1: ['JUMP', 'IF_JUMP', 'LABEL', 'CALL', 'ENTER', 'LEAVE'],
        2: ['ADD', 'SUB', 'MUL', 'DIV', 'NEG', 'BOOL_OP'],
        9: ['CHILD', 'ASM', 'EMPTY'],
    }

    @classmethod
    def _code_name(cls, type):
        return cls._CODE_NAMES[type / 10][type % 10]

    var_size = 4  # every type uses 4 bytes for now
    strcat_function = 'concatString'  # runtime library functions for '+' string operator

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

    @classmethod
    def _asm_instr(cls, code):
        """ Generator of assembly instructions from a single intermediate code. """
        for case in switch(code['type']):
            if case(cls.PUSH):
                yield cls._str_asm('pushl', [str(code['src'])], code)
                return
            if case(cls.POP):
                yield cls._str_asm('popl', [str(Loc.reg(code['reg']))], code)
                return
            if case(cls.MOV):
                yield cls._str_asm('movl', [str(code['src']), str(code['dest'])], code)
                return
            if case(cls.JUMP):
                yield cls._str_asm('jmp', [code['dest']], code)
                return
            if case(cls.IF_JUMP):
                yield cls._str_asm('cmpl', [str(code['lhs']), str(code['rhs'])], code)
                yield cls._str_asm(code['op'], [code['dest']], code)
                return
            if case(cls.LABEL):
                yield cls._str_asm(code['name'] + ':', [], code)
                return
            if case(cls.CALL):
                yield cls._str_asm('call', [code['name']], code)
                return
            if case(cls.ENTER):
                yield cls._str_asm('pushl', ['%ebp'], code)
                yield cls._str_asm('movl', ['%esp', '%ebp'], code)
                var_space = Loc.const((code['var_count'] + 1) * Codes.var_size)
                yield cls._str_asm('subl', [str(var_space), '%esp'], code)
                return
            if case(cls.LEAVE):
                yield cls._str_asm('leave', [], code)
                yield cls._str_asm('ret', [], code)
                return
            if case(cls.ADD, cls.SUB, cls.MUL):
                op = {cls.ADD: 'addl', cls.SUB: 'subl', cls.MUL: 'imull'}[code['type']]
                yield cls._str_asm(op, [str(code['lhs']), str(code['rhs'])], code)
                return
            if case(cls.DIV):
                yield cls._str_asm('cdq', [], code)
                yield cls._str_asm('idivl', [str(code['lhs'])], code)
                return
            if case(cls.NEG):
                yield cls._str_asm('negl', [str(code['dest'])], code)
                return
            if case(cls.BOOL_OP):
                yield cls._str_asm('cmpl', [str(code['lhs']), str(code['rhs'])], code)
                yield cls._str_asm(code['op'], [Loc.regcmp], code)
                yield cls._str_asm('movzbl', [Loc.regcmp, str(code['dest'])], code)
                return
            if case(cls.ASM):
                yield cls._str_asm(code['parts'][0], code['parts'][1:], code)
                return
            if case(cls.EMPTY):
                yield ''
                return
            if case(cls.DELETED):  # TODO remove this, DELETED codes should be really deleted
                d = code.copy()
                del d['type']
                yield '\t# [deleted] ' + cls._str_code(d)
                return
            if case(cls.CHILD):
                raise InternalError('code type %s not allowed here', cls._code_name(code['type']))
            if case():
                raise InternalError('invalid code: ' + str(code))

    @classmethod
    def _str_code(cls, code):
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
    regcmp = '%al'

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
    def mem(cls, addr, offset=None):
        if addr == cls.ANY:
            return cls(cls.MEM, cls.ANY)
        if addr[-1] == ')':  # already computed address
            return cls(cls.MEM, addr)
        return cls(cls.MEM, cls.mkaddr(addr, offset))

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
    def mkaddr(pos, offset=None):
        return '%s(%s)' % (str(offset or ''), pos)

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
                return false
            return self.value == self.ANY or other.value == self.ANY or self.value == other.value
        return NotImplemented

