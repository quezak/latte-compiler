#!/usr/bin/python2
# -*- coding: utf8 -*-

import abc
import LatteParser as LP
import LatteCode as CC
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
    regA = '%eax'
    regD = '%edx'
    regC = '%ecx'
    popA = ['popl', '%eax']
    popD = ['popl', '%edx']
    popC = ['popl', '%ecx']
    pushA = ['pushl', '%eax']
    pushD = ['pushl', '%edx']
    pushC = ['pushl', '%ecx']
    top = '%esp'
    ebp = '%ebp'
    regcmp = '%al'
    var_size = 4 # every type uses 4 bytes for now
    strcat_function = 'concatString' # runtime library functions for '+' string operator

    @classmethod
    def argAddr(cls, n):
        """ Address of n-th function argument. """
        return cls.addr(cls.ebp, cls.var_size * (n+2))

    @classmethod
    def varAddr(cls, n):
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
    def stringLiteralLabel(cls, string):
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

    def addChild(self, code):
        self.children.append(code)
        code.setParent(self)

    def setParent(self, code):
        self.parent = code
        
    def addInstr(self, ins):
        """ Appends a list of instructions. """
        self.instr.append(ins)

    @abc.abstractmethod
    def genCode(self, **kwargs):
        """ Recursively generates all subtree's code. """
        pass

    def addChildCode(self, num, **kwargs):
        """ Generates code for the n-th child and inserts it in the node's code. """
        self.children[num].genCode(**kwargs)
        self.addInstr(Codes.child(num))


    @staticmethod
    def _genInstr(instr):
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
                    yield self._genInstr(instr[1] + ':')
                    break
                if case():
                    if len(instr) > 1:
                        args = '\t' + ', '.join(instr[1:])
                    else:
                        args = ''
                    yield self._genInstr(instr[0] + args)
        debug('<- ' + type(self).__name__)

    def getCurBlock(self):
        return self.parent.getCurBlock() if self.parent else None

    def getCurFun(self):
        return self.parent.getCurFun() if self.parent else None

### program #######################################################################################
class ProgCode(LatteCode):
    def __init__(self, tree, **kwargs):
        super(ProgCode, self).__init__(tree, **kwargs)
        for funtree in tree.children:
            self.addFunCode(funtree)

    def addFunCode(self, funtree):
        self.addChild(FunCode(funtree))

    def genCode(self, **kwargs):
        # source file info
        if not Flags.input_from_stdin(): self.addInstr(['.file', '"%s"' % Flags.input_file])
        self.addInstr([])
        # program code
        self.addInstr(['.text'])
        for i in xrange(len(self.children)):
            self.addInstr([])
            self.addChildCode(i)
        # string constants (after children, so their labels are allocated)
        self.addInstr([])
        self.addInstr(['.section', '.rodata'])
        for string, label in Codes._strings.iteritems():
            self.addInstr(['LABEL', label])
            self.addInstr(['.string', string])

### function ######################################################################################
class FunCode(LatteCode):
    def __init__(self, tree, **kwargs):
        super(FunCode, self).__init__(tree, **kwargs)
        self.ret_type = tree.ret_type
        self.name = tree.name
        self.args = tree.args
        self.addChild(BlockCode(tree.children[0]))
        self.ret_label = Codes.label()
        for i in xrange(len(self.args)):
            arg = self.args[i]
            self.tree.addSymbol(Symbol(arg.name, arg.type, Codes.argAddr(i)))
        self.var_count = self.children[0].countLocalVars()
        debug("fun ", self.name, "VAR COUNT: ", self.var_count)
        self.used_vars = 0 # how many local variables are currently allocated

    def getCurFun(self):
        return self

    def nextVarNum(self):
        """ Returns the index of next free local variable on stack. """
        self.used_vars += 1
        return self.used_vars - 1

    def genCode(self, **kwargs):
        self.addInstr(['.globl', self.name])
        self.addInstr(['.type', self.name, '@function'])
        self.addInstr(['LABEL', self.name])
        # prologue
        self.addInstr(['pushl', Codes.ebp])
        self.addInstr(['movl', Codes.top, Codes.ebp])
        # first local variable is at -4(%ebp), hence var_count+1
        self.addInstr(['subl', Codes.const((self.var_count+1) * Codes.var_size), Codes.top])
        #self.addInstr(['andl', '$-16', Codes.top]) TODO do we need this?
        # insert block
        self.addChildCode(0)
        # epilogue
        self.addInstr(['LABEL', self.ret_label])
        self.addInstr(['leave'])
        self.addInstr(['ret'])


### statement #####################################################################################
class StmtCode(LatteCode):
    # List of statement types that introduce a new context
    context_stmts = [LP.BLOCK, LP.IF, LP.WHILE]

    def __init__(self, tree, **kwargs):
        super(StmtCode, self).__init__(tree, **kwargs)
        self.type = tree.type
        if 'no_children' not in kwargs:
            for child in tree.children:
                self.addChild(StmtFactory(child))
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

    def genCode(self, **kwargs):
        for case in switch(self.type.type):
            if case(LP.RETURN):
                if self.children:
                    # Evaluate the expression and pop the result to eax for returning.
                    self.addChildCode(0)
                    self.addInstr(Codes.popA)
                # Jump to the return section
                self.addInstr(['jmp', self.getCurFun().ret_label])
                break
            if case(LP.IF):
                # children: cond, (then-block)?, (else-block)?
                self.addChildCode(0, jump_true=self.label_then, jump_false=self.label_else)
                if len(self.children) > 1: # there is a then-block
                    self.addInstr(['LABEL', self.label_then])
                    self.addChildCode(1)
                if len(self.children) > 2: # there is an else-block
                    self.addInstr(['jmp', self.label_after]) # first jump out of then-block
                    self.addInstr(['LABEL', self.label_else])
                    self.addChildCode(2)
                self.addInstr(['LABEL', self.label_after])
                break
            if case(LP.WHILE):
                # children: cond, (block)?
                if len(self.children) > 1: # there is a loop block
                    self.addInstr(['jmp', self.label_cond])
                    self.addInstr(['LABEL', self.label_block])
                    self.addChildCode(1)
                self.addInstr(['LABEL', self.label_cond])
                self.addChildCode(0, label_true=self.label_block, label_false=self.label_after)
                self.addInstr(['LABEL', self.label_after])
                break
            if case(LP.ASSIGN):
                # compute assigned value on stack
                self.addChildCode(1)
                self.addInstr(Codes.popA)
                # put the value into destination address
                dest_addr = self.tree.symbol(self.children[0].value).pos
                self.addInstr(['movl', Codes.regA, dest_addr])
                break
            if case(LP.INCR, LP.DECR):
                op = 'addl' if self.type.type == LP.INCR else 'subl'
                addr = self.tree.symbol(self.children[0].value).pos
                self.addInstr([op, Codes.const(1), addr])
                break
            if case():
                raise NotImplementedError('unknown statement type: %s' % str(self.type))

    def countLocalVars(self):
        """ Calculate stack space needed to allocate all local variables in subtree.
        
        This is the total size of declarations directly in a block plus maximum size needed by any
        sub-block (because local variables from different sub-block can occupy the same addresses).
        Obviously not thread-safe, but we don't have threading so we can save stack space here.
        """
        max_subblock = 0
        sum_decls = 0
        for i in xrange(len(self.children)):
            if self.children[i].tree.type.type in self.context_stmts:
                max_subblock = max(max_subblock, self.children[i].countLocalVars())
            elif self.children[i].tree.type.type == LP.DECL:
                sum_decls += self.children[i].countLocalVars()
        debug('in block max_subblock: ', max_subblock, ' sum_decls: ', sum_decls)
        self.var_count = sum_decls + max_subblock
        return self.var_count



### code block ####################################################################################
class BlockCode(StmtCode):
    def __init__(self, tree, **kwargs):
        super(BlockCode, self).__init__(tree, no_children=True, **kwargs)
        for stmttree in tree.children:
            self.addChild(StmtFactory(stmttree))

    def getCurBlock(self):
        return self

    def genCode(self, **kwargs):
        for i in xrange(len(self.children)):
            self.addChildCode(i)
        fun = self.getCurFun()
        fun.used_vars -= self.var_count # free local variables as the context is closed

### declaration ###################################################################################
class DeclCode(StmtCode):
    def __init__(self, tree, **kwargs):
        super(DeclCode, self).__init__(tree, no_children=True, **kwargs)
        self.decl_type = tree.decl_type
        self.items = []
        for item in tree.items:
            self.addItem(item)

    def addItem(self, item):
        self.items.append(item)
        self.items[-1].expr_child = len(self.children)
        if item.expr:
            self.addChild(ExprFactory(item.expr))

    def countLocalVars(self):
        return len(self.items)

    def genCode(self, **kwargs):
        fun = self.getCurFun()
        block = self.getCurBlock()
        # For each declared item, compute its address on stack (and assign the value if needed).
        for item in self.items:
            addr = Codes.varAddr(fun.nextVarNum())
            block.tree.addSymbol(Symbol(item.name, self.decl_type.type, addr))
            if item.expr:
                self.addChildCode(item.expr_child)
                self.addInstr(Codes.popA)
                self.addInstr(['movl', Codes.regA, addr])


### expression ####################################################################################
class ExprCode(StmtCode):
    # niezmiennik: po wyliczeniu wyrażenia jego wynik (i tylko on) zostaje na stosie
    def __init__(self, tree, **kwargs):
        super(ExprCode, self).__init__(tree, no_children=True, **kwargs)
        self.value_type = tree.value_type

    def isConstant(self):
        return False

    def checkUnusedResult(self):
        if self.tree.unused_result:
            debug('POP UNUSED RESULT', self.tree.pos)
            self.addInstr(['addl', Codes.const(Codes.var_size), Codes.regA, '# unused result'])


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

    def genCode(self, **kwargs):
        for case in switch(self.type.type):
            if case(LP.BOOLEAN, LP.INT):
                self.addInstr(['pushl', Codes.const(self.value)])
                break
            if case(LP.IDENT):
                self.addInstr(['movl', self.tree.symbol(self.value).pos, Codes.regA])
                self.addInstr(Codes.pushA)
                break
            if case(LP.STRING):
                self.addInstr(['pushl', '$' + Codes.stringLiteralLabel(self)])
        self.checkUnusedResult()

    def isConstant(self):
        return self.type.type != LP.IDENT


### unary operator ################################################################################
class UnopCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(UnopCode, self).__init__(tree, **kwargs)
        self.addChild(ExprFactory(tree.children[0]))

    def genCode(self, **kwargs):
        self.addChildCode(0)
        for case in switch(self.type.type):
            if case(LP.NEG): # binary negation
                self.addInstr(Codes.popA)
                self.addInstr(['negl', Codes.regA])
                self.addInstr(Codes.pushA)
                break
            if case(LP.NOT): # logical not TODO lazy evaluation tricks
                self.addInstr(Codes.popA)
                self.addInstr(['cmpl', Codes.const(0), Codes.regA])
                self.addInstr(['sete', Codes.regcmp])
                self.addInstr(['movzbl', Codes.regcmp, Codes.regA])
                self.addInstr(Codes.pushA)
                break
            if case():
                raise InternalError('wrong unop value type')
        self.checkUnusedResult()
        

### binary operator ###############################################################################
class BinopCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(BinopCode, self).__init__(tree, **kwargs)
        self.addChild(ExprFactory(tree.children[0]))
        self.addChild(ExprFactory(tree.children[1]))
        if self.type.type in [LP.AND, LP.OR]:
            self.label_jump = Codes.label()
            self.label_nojump = Codes.label()

    def genCode(self, **kwargs):
        self.addChildCode(0)
        self.addChildCode(1)
        # detect operator type
        for case in switch(self.value_type.type):
            if case(LP.INT):
                self._genCodeIntop()
                break
            if case(LP.BOOLEAN):
                if self.type.type in BinopTree._rel_ops:
                    self._genCodeRelop() # comparision
                else:
                    self._genCodeBoolop() # logical operation
                break
            if case(LP.STRING):
                self._genCodeStringop()
                break
            if case():
                raise InternalError('wrong binop value type %s')
        self.checkUnusedResult()

    def _genCodeIntop(self):
        for case in switch(self.type.type):
            if case(LP.PLUS, LP.MINUS, LP.MULT):
                opcode = { LP.PLUS: 'addl', LP.MINUS: 'subl', LP.MULT: 'imull' }[self.type.type]
                self.addInstr(Codes.popD)
                self.addInstr(Codes.popA)
                self.addInstr([opcode, Codes.regD, Codes.regA])
                self.addInstr(Codes.pushA)
                break
            if case(LP.DIV, LP.MOD):
                self.addInstr(Codes.popC)
                self.addInstr(Codes.popA)
                self.addInstr(['cdq'])
                self.addInstr(['idivl', Codes.regC]) # quotient in eax, remainder in edx
                self.addInstr(Codes.pushA if self.type.type == LP.DIV else Codes.pushD)
                break
            if case():
                raise InternalError('wrong int op type %s' % str(self.type))

    def _genCodeRelop(self):
        # TODO rewrite to finish lazy evaluation
        try:
            opcode = { LP.EQ: 'sete', LP.NEQ: 'setne', LP.GT: 'setg', LP.GEQ: 'setge',
                    LP.LT: 'setl', LP.LEQ: 'setle' }[self.type.type]
        except KeyError:
            raise InternalError('wrong rel op type %s' % str(self.type))
        self.addInstr(Codes.popD)
        self.addInstr(Codes.popA)
        self.addInstr(['cmpl', Codes.regD, Codes.regA])
        self.addInstr([opcode, Codes.regcmp])
        self.addInstr(['movzbl', Codes.regcmp, Codes.regA])
        self.addInstr(Codes.pushA)

    def _genCodeBoolop(self):
        # TODO rewrite to finish lazy evaluation
        if self.type.type == LP.AND:
            jval = Codes.const(0)
            nval = Codes.const(1)
        elif self.type.type == LP.OR:
            jval = Codes.const(1)
            nval = Codes.const(0)
        else:
            raise InternalError('wrong bool op type %s' % str(self.type))
        self.addInstr(Codes.popA)
        self.addInstr(Codes.popD)
        self.addInstr(['cmpl', jval, Codes.regD])
        self.addInstr(['je', self.label_jump])
        self.addInstr(['cmpl', jval, Codes.regA])
        self.addInstr(['je', self.label_jump])
        self.addInstr(['pushl', nval])
        self.addInstr(['jmp', self.label_nojump])
        self.addInstr(['LABEL', self.label_jump])
        self.addInstr(['pushl', jval])
        self.addInstr(['LABEL', self.label_nojump])

    def _genCodeStringop(self):
        # only + (concatenation) for now
        # note: the generic binop code pushes left operand first, so for convenience our library
        # concatenation function accepts arguments in reversed order.
        if self.type.type != LP.PLUS:
            raise InternalError('wrong string op type %s' % str(self.type))
        self.addInstr(['call', Codes.strcat_function])
        self.addInstr(['addl', Codes.const(2 * Codes.var_size), Codes.top])
        self.addInstr(Codes.pushA)
        # TODO free memory later


### function call #################################################################################
class FuncallCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(FuncallCode, self).__init__(tree, **kwargs)
        self.fname = tree.fname
        self.fsym = tree.symbol(tree.fname)
        for exprtree in tree.children:
            self.addChild(ExprFactory(exprtree))

    def genCode(self, **kwargs):
        fun = self.getCurFun()
        # [1] Compute memory usage for arguments.
        # TODO do we need to 16-align the stack? probably not, seems to work fine
        argmem = Codes.var_size * len(self.children)
        # [2] Push arguments in reversed order.
        for i in reversed(xrange(len(self.children))):
            self.addChildCode(i) # Leaves the value on stack.
        # [3] Call and pop arguments.
        self.addInstr(['call', self.fname])
        self.addInstr(['addl', Codes.const(argmem), Codes.top])
        # [4] Push the result if any.
        if self.fsym.ret_type.type != LP.VOID:
            self.addInstr(['pushl', Codes.regA])
        self.checkUnusedResult()


### factories #####################################################################################
def _StmtConstructor(tree, **kwargs):
    if isinstance(tree, ExprTree):
        return ExprFactory
    for case in switch(tree.type.type):
        if case(LP.BLOCK):
            return BlockCode
        if case(LP.DECL):
            return DeclCode
    return StmtCode

def StmtFactory(tree, **kwargs):
    cstr = _StmtConstructor(tree, **kwargs)
    return cstr(tree, **kwargs)

def _ExprConstructor(tree, **kwargs):
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
    cstr = _ExprConstructor(tree, **kwargs)
    return cstr(tree, **kwargs)
