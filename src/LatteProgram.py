#!/usr/bin/python2
# -*- coding: utf8 -*-

import abc
import LatteParser as LP
from FuturePrint import debug
from LatteParser import Builtins
from LatteUtils import Symbol, FunSymbol
from LatteErrors import InternalError
from LatteNodes import ExprTree, BinopTree
from Utils import switch, Flags


### code tokens ###################################################################################
class Codes(object):
    """ Some text constants and code generations helpers. """
    # Note: cdecl needs EBP, ESI, EDI, EBX preserved.
    reg_a = '%eax'
    reg_d = '%edx'
    reg_c = '%ecx'
    pop_a = ['popl', '%eax']
    pop_d = ['popl', '%edx']
    pop_c = ['popl', '%ecx']
    push_a = ['pushl', '%eax']
    push_d = ['pushl', '%edx']
    push_c = ['pushl', '%ecx']
    top = '%esp'
    ebp = '%ebp'
    regcmp = '%al'
    var_size = 4 # every type uses 4 bytes for now
    strcat_function = 'concatString' # runtime library functions for '+' string operator

    @classmethod
    def arg_addr(cls, n):
        """ Address of n-th function argument. """
        return cls.addr(cls.ebp, cls.var_size * (n+2))

    @classmethod
    def var_addr(cls, n):
        """ Address of n-th local variable on stack. """
        return cls.addr(cls.ebp, cls.var_size * (-1-n))

    @staticmethod
    def child(n): 
        return ['CHILD', n]

    _labels = 1
    @classmethod
    def label(cls):
        cls._labels += 1
        return '.L%d' % cls._labels

    @staticmethod
    def addr(pos, offset=None):
        return '%s(%s)' % (str(offset or ''), pos)

    @staticmethod
    def const(n):
        return '$' + str(n)

    _strings = {}

    @classmethod
    def string_literal_label(cls, string):
        """ Return a label for a string literal ('.LCx'), but store each constant only once. """
        if string.value not in cls._strings:
            label = '.LC%d' % len(cls._strings)
            cls._strings[string.value] = label
            return label
        return cls._strings[string.value]



### code ABC ######################################################################################
class LatteCode(object):
    """ Abstract base class for code-generation nodes. """
    __metaclass__ = abc.ABCMeta
    
    def __init__(self, tree, **kwargs):
        super(LatteCode, self).__init__()
        self.tree = tree
        self.children = []
        self.instr = []
        self.parent = None

    def add_child(self, code):
        self.children.append(code)
        code.set_parent(self)

    def set_parent(self, code):
        self.parent = code
        
    def add_instr(self, ins):
        """ Appends a list of instructions. """
        self.instr.append(ins)

    @abc.abstractmethod
    def gen_code(self, **kwargs):
        """ Recursively generates all subtree's code. """
        pass

    def add_child_code(self, num, **kwargs):
        """ Generates code for the n-th child and inserts it in the node's code. """
        self.children[num].gen_code(**kwargs)
        self.add_instr(Codes.child(num))


    @staticmethod
    def _gen_instr(instr):
        instr = instr.replace(', #', '  #')
        """ Some minimal output formatting. """
        if (instr[:1] != '.' or instr.startswith('.string')) and instr[-1:] != ':':
            instr = '\t' + instr
        debug(instr)
        return instr

    def instructions(self):
        """ A generator that yields the asm instructions. """
        debug('-> ' + type(self).__name__)
        for instr in self.instr:
            if not instr:
                yield '' # A blank line as None for output readability.
                continue
            for case in switch(instr[0]):
                if case('CHILD'):
                    for child_instr in self.children[instr[1]].instructions():
                        yield child_instr
                    break
                if case('LABEL'):
                    yield self._gen_instr(instr[1] + ':')
                    break
                if case():
                    if len(instr) > 1:
                        args = '\t' + ', '.join(instr[1:])
                    else:
                        args = ''
                    yield self._gen_instr(instr[0] + args)
        debug('<- ' + type(self).__name__)

    def get_cur_block(self):
        return self.parent.get_cur_block() if self.parent else None

    def get_cur_fun(self):
        return self.parent.get_cur_fun() if self.parent else None

### program #######################################################################################
class ProgCode(LatteCode):
    def __init__(self, tree, **kwargs):
        super(ProgCode, self).__init__(tree, **kwargs)
        for funtree in tree.children:
            self.add_fun_code(funtree)

    def add_fun_code(self, funtree):
        self.add_child(FunCode(funtree))

    def gen_code(self, **kwargs):
        # source file info
        if not Flags.input_from_stdin(): self.add_instr(['.file', '"%s"' % Flags.input_file])
        self.add_instr([])
        # program code
        self.add_instr(['.text'])
        for i in xrange(len(self.children)):
            self.add_instr([])
            self.add_child_code(i)
        # string constants (after children, so their labels are allocated)
        self.add_instr([])
        self.add_instr(['.section', '.rodata'])
        for string, label in Codes._strings.iteritems():
            self.add_instr(['LABEL', label])
            self.add_instr(['.string', string])

### function ######################################################################################
class FunCode(LatteCode):
    def __init__(self, tree, **kwargs):
        super(FunCode, self).__init__(tree, **kwargs)
        self.ret_type = tree.ret_type
        self.name = tree.name
        self.args = tree.args
        self.add_child(BlockCode(tree.children[0]))
        self.ret_label = Codes.label()
        for i in xrange(len(self.args)):
            arg = self.args[i]
            self.tree.add_symbol(Symbol(arg.name, arg.type, Codes.arg_addr(i)))
        self.var_count = self.children[0].count_local_vars()
        debug("fun ", self.name, "VAR COUNT: ", self.var_count)
        self.used_vars = 0 # how many local variables are currently allocated

    def get_cur_fun(self):
        return self

    def next_var_num(self):
        """ Returns the index of next free local variable on stack. """
        self.used_vars += 1
        return self.used_vars - 1

    def gen_code(self, **kwargs):
        self.add_instr(['.globl', self.name])
        self.add_instr(['.type', self.name, '@function'])
        self.add_instr(['LABEL', self.name])
        # prologue
        self.add_instr(['pushl', Codes.ebp])
        self.add_instr(['movl', Codes.top, Codes.ebp])
        # first local variable is at -4(%ebp), hence var_count+1
        self.add_instr(['subl', Codes.const((self.var_count+1) * Codes.var_size), Codes.top])
        #self.add_instr(['andl', '$-16', Codes.top]) TODO do we need this?
        # insert block
        self.add_child_code(0)
        # epilogue
        self.add_instr(['LABEL', self.ret_label])
        self.add_instr(['leave'])
        self.add_instr(['ret'])


### statement #####################################################################################
class StmtCode(LatteCode):
    # List of statement types that introduce a new context
    context_stmts = [LP.BLOCK, LP.IF, LP.WHILE]

    def __init__(self, tree, **kwargs):
        super(StmtCode, self).__init__(tree, **kwargs)
        self.type = tree.type
        if 'no_children' not in kwargs:
            for child in tree.children:
                self.add_child(StmtFactory(child))
        for case in switch(self.type.type):
            if case(LP.IF):
                self.label_after = Codes.label()
                # if there are less blocks, just evaluate condition and jump to label_after
                self.label_then = Codes.label() if len(self.children) > 1 else self.label_after
                self.label_else = Codes.label() if len(self.children) > 2 else self.label_after
                break
            if case(LP.WHILE):
                self.label_after = Codes.label()
                self.label_cond = Codes.label()
                self.label_block = Codes.label() if len(self.children) > 1 else self.label_cond
                break

    def gen_code(self, **kwargs):
        for case in switch(self.type.type):
            if case(LP.RETURN):
                if self.children:
                    # Evaluate the expression and pop the result to eax for returning.
                    self.add_child_code(0)
                    self.add_instr(Codes.pop_a)
                # Jump to the return section
                self.add_instr(['jmp', self.get_cur_fun().ret_label])
                break
            if case(LP.IF):
                # children: cond, (then-block)?, (else-block)?
                self.add_child_code(0, on_true=self.label_then, on_false=self.label_else)
                if len(self.children) > 1: # there is a then-block
                    self.add_instr(['LABEL', self.label_then])
                    self.add_child_code(1)
                if len(self.children) > 2: # there is an else-block
                    self.add_instr(['jmp', self.label_after]) # first jump out of then-block
                    self.add_instr(['LABEL', self.label_else])
                    self.add_child_code(2)
                self.add_instr(['LABEL', self.label_after])
                break
            if case(LP.WHILE):
                # children: cond, (block)?
                if len(self.children) > 1: # there is a loop block
                    self.add_instr(['jmp', self.label_cond])
                    self.add_instr(['LABEL', self.label_block])
                    self.add_child_code(1)
                self.add_instr(['LABEL', self.label_cond])
                self.add_child_code(0, on_true=self.label_block, on_false=self.label_after)
                self.add_instr(['LABEL', self.label_after])
                break
            if case(LP.ASSIGN):
                # compute assigned value on stack
                self.add_child_code(1)
                self.add_instr(Codes.pop_a)
                # put the value into destination address
                dest_addr = self.tree.symbol(self.children[0].value).pos
                self.add_instr(['movl', Codes.reg_a, dest_addr])
                break
            if case(LP.INCR, LP.DECR):
                op = 'addl' if self.type.type == LP.INCR else 'subl'
                addr = self.tree.symbol(self.children[0].value).pos
                self.add_instr([op, Codes.const(1), addr])
                break
            if case():
                raise NotImplementedError('unknown statement type: %s' % str(self.type))

    def count_local_vars(self):
        """ Calculate stack space needed to allocate all local variables in subtree.
        
        This is the total size of declarations directly in a block plus maximum size needed by any
        sub-block (because local variables from different sub-block can occupy the same addresses).
        Obviously not thread-safe, but we don't have threading so we can save stack space here.
        """
        max_subblock = 0
        sum_decls = 0
        for i in xrange(len(self.children)):
            if self.children[i].tree.type.type in self.context_stmts:
                max_subblock = max(max_subblock, self.children[i].count_local_vars())
            elif self.children[i].tree.type.type == LP.DECL:
                sum_decls += self.children[i].count_local_vars()
        debug('in block max_subblock: ', max_subblock, ' sum_decls: ', sum_decls)
        self.var_count = sum_decls + max_subblock
        return self.var_count



### code block ####################################################################################
class BlockCode(StmtCode):
    def __init__(self, tree, **kwargs):
        super(BlockCode, self).__init__(tree, no_children=True, **kwargs)
        for stmttree in tree.children:
            self.add_child(StmtFactory(stmttree))

    def get_cur_block(self):
        return self

    def gen_code(self, **kwargs):
        for i in xrange(len(self.children)):
            self.add_child_code(i)
        fun = self.get_cur_fun()
        fun.used_vars -= self.var_count # free local variables as the context is closed

### declaration ###################################################################################
class DeclCode(StmtCode):
    def __init__(self, tree, **kwargs):
        super(DeclCode, self).__init__(tree, no_children=True, **kwargs)
        self.decl_type = tree.decl_type
        self.items = []
        for item in tree.items:
            self.add_item(item)

    def add_item(self, item):
        self.items.append(item)
        self.items[-1].expr_child = len(self.children)
        if item.expr:
            self.add_child(ExprFactory(item.expr))

    def count_local_vars(self):
        return len(self.items)

    def gen_code(self, **kwargs):
        fun = self.get_cur_fun()
        block = self.get_cur_block()
        # For each declared item, compute its address on stack (and assign the value if needed).
        for item in self.items:
            addr = Codes.var_addr(fun.next_var_num())
            block.tree.add_symbol(Symbol(item.name, self.decl_type.type, addr))
            if item.expr:
                self.add_child_code(item.expr_child)
                self.add_instr(Codes.pop_a)
                self.add_instr(['movl', Codes.reg_a, addr])


### expression ####################################################################################
class ExprCode(StmtCode):
    # niezmiennik: po wyliczeniu wyrażenia jego wynik (i tylko on) zostaje na stosie
    def __init__(self, tree, **kwargs):
        super(ExprCode, self).__init__(tree, no_children=True, **kwargs)
        self.value_type = tree.value_type

    def is_constant(self):
        # TODO do we need this?
        return False

    def check_unused_result(self):
        # TODO optimize this out -- just don't push the value before...
        if self.tree.unused_result:
            debug('POP UNUSED RESULT', self.tree.pos)
            self.add_instr(['addl', Codes.const(Codes.var_size), Codes.reg_a, '# unused result'])

    @staticmethod
    def has_jump_codes(d):
        """ Check if a dictionary contains jump codes for lazy boolean evaluation """
        return 'on_true' in d and 'on_false' in d


### literal #######################################################################################
class LiteralCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(LiteralCode, self).__init__(tree, **kwargs)
        self.value = tree.value
        for case in switch(self.type.type):
            if case(LP.BOOLEAN):
                # We already checked that a boolean is a boolean, now we use numbers.
                if self.value == 'true':
                    self.value = 1
                else:
                    self.value = 0
                break

    def gen_code(self, **kwargs):
        if self.has_jump_codes(kwargs):
            # bool literal as part of condition evaluation -- jump basing on value
            for case in switch(self.type.type):
                if case(LP.BOOLEAN):
                    label = {0: kwargs['on_false'], 1: kwargs['on_true']}[self.value]
                    self.add_instr(['jmp', label])
                    break
                if case(LP.IDENT) and self.tree.symbol(self.value).type == LP.BOOLEAN:
                    self.add_instr(['movl', self.tree.symbol(self.value).pos, Codes.reg_a])
                    # note: comparing with 0, so on equality jump to false!
                    self.add_instr(['cmpl', Codes.const(0), Codes.reg_a])
                    self.add_instr(['je', kwargs['on_false']])
                    self.add_instr(['jmp', kwargs['on_true']])
                    break
                if case():
                    raise InternalError('jump-expr codes for non-bool %s literal at %s!' % (
                        str(self.type), self.tree.pos))
            return
        for case in switch(self.type.type):
            if case(LP.BOOLEAN, LP.INT):
                self.add_instr(['pushl', Codes.const(self.value)])
                break
            if case(LP.IDENT):
                self.add_instr(['movl', self.tree.symbol(self.value).pos, Codes.reg_a])
                self.add_instr(Codes.push_a)
                break
            if case(LP.STRING):
                self.add_instr(['pushl', '$' + Codes.string_literal_label(self)])
        self.check_unused_result()

    def is_constant(self):
        return self.type.type != LP.IDENT


### unary operator ################################################################################
class UnopCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(UnopCode, self).__init__(tree, **kwargs)
        self.add_child(ExprFactory(tree.children[0]))

    def gen_code(self, **kwargs):
        for case in switch(self.type.type):
            if case(LP.NEG): # integer negation
                self.add_child_code(0)
                self.add_instr(Codes.pop_a)
                self.add_instr(['negl', Codes.reg_a])
                self.add_instr(Codes.push_a)
                break
            if case(LP.NOT): # logical not TODO lazy evaluation tricks
                if self.has_jump_codes(kwargs):
                    # called as part of condition evaluation -- just forward the jump labels
                    self.add_child_code(0, on_true=kwargs['on_false'], on_false=kwargs['on_true'])
                    return
                self.add_child_code(0)
                self.add_instr(Codes.pop_a)
                self.add_instr(['cmpl', Codes.const(0), Codes.reg_a])
                self.add_instr(['sete', Codes.regcmp])
                self.add_instr(['movzbl', Codes.regcmp, Codes.reg_a])
                self.add_instr(Codes.push_a)
                break
            if case():
                raise InternalError('wrong unop value type')
        self.check_unused_result()
        

### binary operator ###############################################################################
class BinopCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(BinopCode, self).__init__(tree, **kwargs)
        self.add_child(ExprFactory(tree.children[0]))
        self.add_child(ExprFactory(tree.children[1]))

    def gen_code(self, **kwargs):
        # detect operator type
        for case in switch(self.value_type.type):
            if case(LP.INT):
                self._gen_code_intop()
                break
            if case(LP.BOOLEAN):
                if self.type.type in BinopTree._rel_ops:
                    self._gen_code_relop(**kwargs) # comparision
                else:
                    self._gen_code_boolop(**kwargs) # logical operation
                break
            if case(LP.STRING):
                self._gen_code_stringop()
                break
            if case():
                raise InternalError('wrong binop value type %s')
        self.check_unused_result()

    def _gen_code_intop(self):
        self.add_child_code(0)
        self.add_child_code(1)
        for case in switch(self.type.type):
            if case(LP.PLUS, LP.MINUS, LP.MULT):
                opcode = { LP.PLUS: 'addl', LP.MINUS: 'subl', LP.MULT: 'imull' }[self.type.type]
                self.add_instr(Codes.pop_d)
                self.add_instr(Codes.pop_a)
                self.add_instr([opcode, Codes.reg_d, Codes.reg_a])
                self.add_instr(Codes.push_a)
                break
            if case(LP.DIV, LP.MOD):
                self.add_instr(Codes.pop_c)
                self.add_instr(Codes.pop_a)
                self.add_instr(['cdq'])
                self.add_instr(['idivl', Codes.reg_c]) # quotient in eax, remainder in edx
                self.add_instr(Codes.push_a if self.type.type == LP.DIV else Codes.push_d)
                break
            if case():
                raise InternalError('wrong int op type %s' % str(self.type))

    def _gen_code_relop(self, **kwargs):
        self.add_child_code(0)
        self.add_child_code(1)
        self.add_instr(Codes.pop_d)
        self.add_instr(Codes.pop_a)
        self.add_instr(['cmpl', Codes.reg_d, Codes.reg_a])
        try:
            if self.has_jump_codes(kwargs):
                # part of condition evaluation -- select the conditional jump instruction
                self.label_true = kwargs['on_true']
                self.label_false = kwargs['on_false']
                jmp_code = { LP.EQ: 'je', LP.NEQ: 'jne', LP.GT: 'jg', LP.GEQ: 'jge',
                        LP.LT: 'jl', LP.LEQ: 'jle' }[self.type.type]
                debug('relop %s in cond' % jmp_code)
                self.add_instr([jmp_code, self.label_true])
                self.add_instr(['jmp', self.label_false])
            else:
                # expression returning bool -- select the comparision set instruction
                set_code = { LP.EQ: 'sete', LP.NEQ: 'setne', LP.GT: 'setg', LP.GEQ: 'setge',
                        LP.LT: 'setl', LP.LEQ: 'setle' }[self.type.type]
                debug('relop %s in expr' % set_code)
                self.add_instr([set_code, Codes.regcmp])
                self.add_instr(['movzbl', Codes.regcmp, Codes.reg_a])
                self.add_instr(Codes.push_a)
        except KeyError:
            raise InternalError('wrong rel op type %s' % str(self.type))

    def _gen_code_boolop(self, **kwargs):
        self.label_true = kwargs.get('on_true', Codes.label())
        self.label_false = kwargs.get('on_false', Codes.label())
        self.label_right = Codes.label() # additional label to jump straight to the right operand
        for case in switch(self.type.type):
            if case(LP.AND):
                self.add_child_code(0, on_true=self.label_right, on_false=self.label_false)
                self.add_instr(['LABEL', self.label_right])
                self.add_child_code(1, on_true=self.label_true, on_false=self.label_false)
                break
            if case(LP.OR):
                self.add_child_code(0, on_true=self.label_true, on_false=self.label_right)
                self.add_instr(['LABEL', self.label_right])
                self.add_child_code(1, on_true=self.label_true, on_false=self.label_false)
                break
            if case():
                raise InternalError('wrong bool op type %s' % str(self.type))
        # if no jump keywords were given, the result will be used as a value -- push it
        if not self.has_jump_codes(kwargs):
            self.label_after = Codes.label()
            self.add_instr(['LABEL', self.label_true])
            self.add_instr(['pushl', Codes.const(1)])
            self.add_instr(['jmp', self.label_after])
            self.add_instr(['LABEL', self.label_false])
            self.add_instr(['pushl', Codes.const(0)])
            self.add_instr(['LABEL', self.label_after])

    def _gen_code_stringop(self):
        if self.type.type != LP.PLUS:
            raise InternalError('wrong string op type %s' % str(self.type))
        # only + (concatenation) for now
        # add children in reversed order, so they are on stack ready to call the concat lib function
        self.add_child_code(1)
        self.add_child_code(0)
        self.add_instr(['call', Codes.strcat_function])
        self.add_instr(['addl', Codes.const(2 * Codes.var_size), Codes.top])
        self.add_instr(Codes.push_a)
        # TODO free memory later


### function call #################################################################################
class FuncallCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(FuncallCode, self).__init__(tree, **kwargs)
        self.fname = tree.fname
        self.fsym = tree.symbol(tree.fname)
        for exprtree in tree.children:
            self.add_child(ExprFactory(exprtree))

    def gen_code(self, **kwargs):
        fun = self.get_cur_fun()
        # [1] Compute memory usage for arguments.
        # TODO do we need to 16-align the stack? probably not, seems to work fine
        argmem = Codes.var_size * len(self.children)
        # [2] Push arguments in reversed order.
        # TODO fix this: we can't evaluate the args in reverse order, even though it's convenient
        #      mayba add a 'result_dest' kwarg to expr codes?
        for i in reversed(xrange(len(self.children))):
            self.add_child_code(i) # Leaves the value on stack.
        # [3] Call and pop arguments.
        self.add_instr(['call', self.fname])
        if argmem > 0: self.add_instr(['addl', Codes.const(argmem), Codes.top])
        # [4] finish depending on how we were called:
        if self.has_jump_codes(kwargs):
            if self.fsym.ret_type.type != LP.BOOLEAN:
                raise InternalError('jump-expr codes for non-bool function %s %s at %s!' % (
                    self.fname, str(self.fsym), self.tree.pos))
            # [4a] bool function as part of condition evaluation -- jump basing on the result
            # note: comparing with 0, so on equality jump to false!
            self.add_instr(['cmpl', Codes.const(0), Codes.reg_a])
            self.add_instr(['je', kwargs['on_false']])
            self.add_instr(['jmp', kwargs['on_true']])
        else:
            # [4b] normal expression -- push the return value on stack if needed
            if self.fsym.ret_type.type != LP.VOID:
                self.add_instr(['pushl', Codes.reg_a])
            self.check_unused_result()


### factories #####################################################################################
def _stmt_constructor(tree, **kwargs):
    if isinstance(tree, ExprTree):
        return ExprFactory
    for case in switch(tree.type.type):
        if case(LP.BLOCK):
            return BlockCode
        if case(LP.DECL):
            return DeclCode
    return StmtCode

def StmtFactory(tree, **kwargs):
    cstr = _stmt_constructor(tree, **kwargs)
    return cstr(tree, **kwargs)

def _expr_constructor(tree, **kwargs):
    for case in switch(tree.type.type):
        if case(LP.INT, LP.STRING, LP.BOOLEAN, LP.IDENT):
            return LiteralCode
        if case(LP.NOT, LP.NEG):
            return UnopCode
        if case(LP.MULT, LP.DIV, LP.MOD, LP.PLUS, LP.MINUS, LP.LT, LP.LEQ, LP.GT, LP.GEQ,
                LP.EQ, LP.NEQ, LP.AND, LP.OR):
            return BinopCode
        if case(LP.FUNCALL):
            return FuncallCode
    raise InternalError('wrong binop code for construction')


def ExprFactory(tree, **kwargs):
    cstr = _expr_constructor(tree, **kwargs)
    return cstr(tree, **kwargs)
