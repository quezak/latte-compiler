#!/usr/bin/python2
# -*- coding: utf8 -*-

import abc
import LatteParser as LP
import LatteCode as CC
from FuturePrint import debug
from LatteParser import Builtins
from LatteUtils import Symbol, FunSymbol, switch
from LatteErrors import InternalError
from LatteNodes import ExprTree


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
    top = '%esp'
    ebp = '%ebp'
    regcmp = '%al'

    @classmethod
    def passArg(cls, n):
        if n == 0: return '(' + cls.top + ')'
        return str(4 * n) + '(' + cls.top + ')'

    @classmethod
    def getArg(cls, n):
        return str(4 * (n+2)) + '(' + cls.ebp + ')'

    @staticmethod
    def child(n): 
        return ['CHILD', n]

    _labels = 1
    @classmethod
    def label(cls):
        cls._labels += 1
        return '.L%d' % cls._labels

    @staticmethod
    def addr(pos, off=None):
        return '%s(%s)' % ((off or ''), pos)


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
    def _genCode(self):
        """ Generates code for the current node, assuming the children are already generated. """
        pass

    def genCode(self):
        """ Recursively generates all subtree's code. """
        for child in self.children:
            child.genCode()
        self._genCode()

    @staticmethod
    def _genInstr(instr):
        """ Some minimal output formatting. """
        if instr[:1] != '.' and instr[-1:] != ':':
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

    def _genCode(self):
        # TODO .file
        # TODO string constants
        self.addInstr(['.text'])
        for i in xrange(0, len(self.children)):
            self.addInstr([])
            self.addInstr(Codes.child(i))


### function ######################################################################################
class FunCode(LatteCode):
    def __init__(self, tree, **kwargs):
        super(FunCode, self).__init__(tree, **kwargs)
        self.ret_type = tree.ret_type
        self.name = tree.name
        self.args = tree.args
        self.addChild(BlockCode(tree.children[0]))
        self.ret_label = Codes.label()
        for i in xrange(0, len(self.args)):
            arg = self.args[i]
            self.tree.addSymbol(Symbol(arg.name, arg.type, Codes.getArg(i)))

    def getCurFun(self):
        return self

    def _genCode(self):
        self.addInstr(['.globl', self.name])
        self.addInstr(['.type', self.name, '@function'])
        self.addInstr(['LABEL', self.name])
        # prologue
        self.addInstr(['pushl', Codes.ebp])
        self.addInstr(['movl', Codes.top, Codes.ebp])
        self.addInstr(['andl', '$-16', Codes.top])
        # insert block
        self.addInstr(Codes.child(0))
        # epilogue
        self.addInstr(['LABEL', self.ret_label])
        self.addInstr(['leave'])
        self.addInstr(['ret'])


### statement #####################################################################################
class StmtCode(LatteCode):
    def __init__(self, tree, **kwargs):
        super(StmtCode, self).__init__(tree, **kwargs)
        self.type = tree.type
        if 'no_children' not in kwargs:
            for child in tree.children:
                self.addChild(StmtFactory(child))
        for case in switch(self.type.type):
            if case(LP.IF):
                self.label_true = Codes.label()
                self.label_after = Codes.label()

    def _genCode(self):
        for case in switch(self.type.type):
            if case(LP.RETURN):
                if self.children:
                    # Evaluate the expression and pop the result to eax for returning.
                    self.addInstr(Codes.child(0))
                    self.addInstr(Codes.popA)
                # Jump to the return section
                self.addInstr(['jmp', self.getCurFun().ret_label])
                break
            if case(LP.IF):
                # children: cond, if-block, (else-block?)
                # TODO rewrite lazy condition evaluation
                self.addInstr(Codes.child(0))
                self.addInstr(['cmpl', '$0', Codes.addr(Codes.top)])
                self.addInstr(['jne', self.label_true]) # true -- skok do bloku true
                if len(self.children) >= 3: # false -- ew. blok false, potem skok za if
                    self.addInstr(Codes.child(2))
                self.addInstr(['jmp', self.label_after])
                self.addInstr(['LABEL', self.label_true]) # blok true
                self.addInstr(Codes.child(1)) # przepuszcza flow do label_after
                self.addInstr(['LABEL', self.label_after])
                break
            # TODO while
            if case():
                raise NotImplementedError('stmt') # TODO


### code block ####################################################################################
class BlockCode(StmtCode):
    def __init__(self, tree, **kwargs):
        super(BlockCode, self).__init__(tree, no_children=True, **kwargs)
        for stmttree in tree.children:
            self.addChild(StmtFactory(stmttree))

    def getCurBlock(self):
        return self

    def _genCode(self):
        for i in xrange(0, len(self.children)):
            self.addInstr(Codes.child(i))


### declaration ###################################################################################
#class DeclCode(StmtCode):
    #def __init__(self, tree, **kwargs):
        #super(DeclCode, self).__init__(tree, no_children=True, **kwargs)
        #self.decl_type = tree.decl_type
        #self.items = []
        #for item in tree.items:
            #self.addItem(item)
        #cpos = 0
        #for item in self.items:
            #if item.expr:
                #expr = self.children[cpos]
                #cpos += 1
            #else:
                #expr = None
            #self.addInstr((CC.decl,
                #{ 'type': self.decl_type, 'name': item.name, 'tree': self.tree, 'expr': expr }
            #))

    #def addItem(self, item):
        #self.items.append(item)
        #if item.expr:
            #self.addChild(ExprFactory(item.expr))

    #def _genCode(self):
        #cpos = 0
        #for item in self.items:
            #if item.expr:
                #self.addInstr(Codes.child(i))
            #else:
                #if self.decl_type.type in [LP.BOOLEAN, LP.INT]:
#TODO                    self.addInstr('


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
            # TODO just change esp
            self.addInstr(['popl', Codes.regA + ' ; unused'])


### literal #######################################################################################
class LiteralCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(LiteralCode, self).__init__(tree, **kwargs)
        self.value = tree.value
        for case in switch(self.type.type):
            if case(LP.BOOLEAN):
                # We already checked that a boolean is a boolean, now we use numbers.
                if self.value == 'true':
                    self.value = '1'
                else:
                    self.value = '0'
                break

    def _genCode(self):
        for case in switch(self.type.type):
            if case(LP.BOOLEAN, LP.INT):
                self.addInstr(['pushl', '$' + self.value])
                break
            if case(LP.IDENT):
                self.addInstr(['pushl', self.tree.symbol(self.value).pos])
                break
            if case(LP.STRING):
                raise NotImplementedError('string literal') # TODO
        self.checkUnusedResult()

    def isConstant(self):
        return self.type.type != LP.IDENT


### unary operator ################################################################################
class UnopCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(UnopCode, self).__init__(tree, **kwargs)
        self.addChild(ExprFactory(tree.children[0]))

    def _genCode(self):
        self.addInstr(Codes.child(0))
        for case in switch(self.type.type):
            if case(LP.NEG):
                self.addInstr(['negl', Codes.addr(Codes.top)])
                break
            if case(LP.NOT):
                self.addInstr(['cmpl', '$0', Codes.addr(Codes.top)])
                self.addInstr(['sete', Codes.regcmp])
                self.addInstr(['movzbl', Codes.regcmp, Codes.addr(Codes.top)])
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

    def _genCode(self):
        self.addInstr(Codes.child(0))
        self.addInstr(Codes.child(1))
        for case in switch(self.children[0].value_type.type):
            if case(LP.INT, LP.BOOLEAN):
                if self.children[0].value_type.type == LP.BOOLEAN:
                    if self.type.type in [LP.AND, LP.OR]:
                        self._genCodeBoolop()
                    else:
                        self._genCodeRelop()
                else:
                    self._genCodeBinop()
                break
            if case(LP.STRING):
                raise NotImplementedError('string binop')
            if case():
                raise InternalError('wrong binop value type %s')
        self.checkUnusedResult()

    def _genCodeBinop(self):
        for case in switch(self.type.type):
            if case(LP.PLUS, LP.MINUS):
                opcode = { LP.PLUS: 'addl', LP.MINUS: 'subl' }[self.type.type]
                self.addInstr(Codes.popA)
                # TODO is the order good?
                self.addInstr([opcode, Codes.regA, Codes.addr(Codes.top)])
                # TODO where do we push the result?
                break
            if case(LP.MULT):
                # TODO join with plus/minus
                self.addInstr(Codes.popD)
                self.addInstr(Codes.popA)
                self.addInstr(['imull', Codes.regD, Codes.regA])
                self.addInstr(['pushl', Codes.regA])
                break
            if case(LP.DIV, LP.MOD):
                self.addInstr(Codes.popC) # dzielnik
                self.addInstr(Codes.popA) # dzielna
                self.addInstr(['cdq'])
                self.addInstr(['idivl', Codes.regC]) # wynik w eax, reszta w edx
                res_reg = (Codes.regA if self.type.type == LP.DIV else Codes.regD)
                self.addInstr(['pushl', res_reg])
                break

    def _genCodeRelop(self):
        opcode = { LP.EQ: 'sete', LP.NEQ: 'setne', LP.GT: 'setg', LP.GEQ: 'setge', LP.LT: 'setl',
                LP.LEQ: 'setle' }[self.type.type]
        self.addInstr(Codes.popA)
        self.addInstr(['cmpl', Codes.addr(Codes.top), Codes.regA])
        self.addInstr([opcode, Codes.regcmp])
        self.addInstr(['movzbl', Codes.regcmp, Codes.addr(Codes.top)])

    def _genCodeBoolop(self):
        if self.type.type == LP.AND:
            jval = '$0'
            nval = '$1'
        elif self.type.type == LP.OR:
            jval = '$1'
            nval = '$0'
        else:
            raise InternalError('wrong boolop type')
        self.addInstr(Codes.popA)
        self.addInstr(['cmpl', jval, Codes.addr(Codes.top)])
        self.addInstr(['je', self.label_jump])
        self.addInstr(['cmpl', jval, Codes.regA])
        self.addInstr(['je', self.label_jump])
        self.addInstr(['movl', nval, Codes.addr(Codes.top)])
        self.addInstr(['jmp', self.label_nojump])
        self.addInstr(['LABEL', self.label_jump])
        self.addInstr(['movl', jval, Codes.addr(Codes.top)])
        self.addInstr(['LABEL', self.label_nojump])


### function call #################################################################################
class FuncallCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(FuncallCode, self).__init__(tree, **kwargs)
        self.fname = tree.fname
        self.fsym = tree.symbol(tree.fname)
        for exprtree in tree.children:
            self.addChild(ExprFactory(exprtree))

    def _genCode(self):
        fun = self.getCurFun()
        # [1] Compute memory usage for arguments.
        argmem = 4 * len(self.children)
        # TODO split this to subl and pushes
        self.addInstr(['subl', '$%d' % argmem, Codes.top])
        # [2] Push arguments.
        for i in xrange(0, len(self.children)):
            self.addInstr(Codes.child(i))
            # TODO lol fix these redundant moves
            self.addInstr(Codes.popA)
            self.addInstr(['movl', Codes.regA, Codes.passArg(i)])
        # [3] Call and pop arguments.
        self.addInstr(['call', self.fname])
        self.addInstr(['addl', '$%d' % argmem, Codes.top])
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
