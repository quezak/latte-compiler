#!/usr/bin/python2
# -*- coding: utf8 -*-

import abc
import LatteParser as LP
from FuturePrint import debug
from LatteParser import Builtins
from LatteUtils import Symbol, FunSymbol, switch
from LatteErrors import Status, TypecheckError


### node ABC ######################################################################################
class LatteTree(object):
    __metaclass__ = abc.ABCMeta
    _ltcounter = 0

    def __init__(self, **kwargs):
        super(LatteTree, self).__init__()
        self.ltnum = LatteTree._ltcounter;
        self._ltcounter += 1
        self.symbols = dict()
        self.parent = None
        self.children = []
        self.level = None
        if 'pos' in kwargs:
            self.pos = kwargs['pos']
            debug('kwarg ready pos=%s' % self.pos)
        elif 'pos_off' in kwargs:
            debug('kwarg pos_off=%d' % kwargs['pos_off'])
            self.savePos(kwargs['pos_off'])
        else:
            self.savePos()
        if 'children' in kwargs:
            for child in kwargs['children']:
                self.addChild(child)

    def addChild(self, tree):
        self.children.append(tree)
        tree.setParent(self)

    def setParent(self, tree):
        self.parent = tree

    def savePos(self, offset=None):
        if offset:
            self.pos = Status.getCurPos(offset)
        else:
            self.savePos(-1)
            #if not self.pos or self.pos == '0:0':
                #self.savePos(-2)
            #if not self.pos or self.pos == '0:0':
                #self.savePos(-3)


    def getCounter(self):
        return self.counter

    #def posStr(self):
        #if not self.ast_node or not self.ast_node.token: return None
        #token = self.ast_node.token
        #return '%d:%d' % (token.line, token.charPositionInLine)

    def addSymbol(self, symbol):
        name = symbol.name
        # nie self.hasSymbol(name) -- kolizji szukamy tylko w bieżącym bloku!
        if name in self.symbols:
            Status.addError(TypecheckError('conflicting declaration of "%s" as "%s"' % (
                name, str(symbol)), symbol.pos))
            if self.symbol(name).pos:
                msg = 'previously declared here as "%s"'
            else:
                msg = 'declared built-in as "%s"'
            Status.addNote(TypecheckError(msg %
                str(self.symbol(name)), self.symbol(name).pos))
        elif self.hasSymbol(name):
            debug('%s: shadowing symbol "%s %s"' % (symbol.pos, str(symbol), name))
        self.symbols[name] = symbol

    def hasSymbol(self, name):
        if name in self.symbols:
            return True
        return (self.parent) and (self.parent.hasSymbol(name))

    def symbol(self, name):
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.symbol(name)
        return None

    @abc.abstractmethod
    def printTree(self):
        pass

    def printChildren(self):
        for child in self.children:
            child.printTree()

    def printT(self, msg):
        debug((' ' * 2 * self.level) + msg)

    def setLevel(self, level):
        self.level = level
        for child in self.children:
            child.setLevel(level+1)

    def checkChildrenTypes(self):
        for child in self.children:
            child.checkTypes()

    def checkTypes(self):
        self.checkChildrenTypes()

    # zejście w górę do definicji funkcji
    def getCurFun(self):
        if not self.parent:
            return None
        return self.parent.getCurFun()

    # zejście w górę do najbliższego bloku
    def getCurBlock(self):
        if not self.parent:
            return None
        return self.parent.getCurBlock()

    # wyczyszczenie tablicy symboli
    def clearSymbols(self):
        self.symbols = dict()
        for child in self.children:
            child.clearSymbols()


### program #######################################################################################
class ProgTree(LatteTree):
    def __init__(self, **kwargs):
        super(ProgTree, self).__init__(**kwargs)
        self.addBuiltinSymbols()

    def addBuiltinSymbols(self):
        debug('addBuildinSymbols')
        self._addBuiltinFun(Builtins.PRINT_INT, LP.VOID, [Symbol('', LP.INT)])
        self._addBuiltinFun(Builtins.PRINT_STRING, LP.VOID, [Symbol('', LP.STRING)])
        self._addBuiltinFun(Builtins.READ_INT, LP.INT, [])
        self._addBuiltinFun(Builtins.READ_STRING, LP.STRING, [])
        self._addBuiltinFun(Builtins.ERROR, LP.VOID, [])

    def _addBuiltinFun(self, name, ret_type, args):
        debug('addBuiltin %s' % name)
        sym = FunSymbol(name, Symbol('', ret_type), args, None)
        self.addSymbol(sym)

    def addFunTree(self, fun_tree):
        debug('addFunTree name=%s ret=%d argcount=%d pos=%s' %
                (fun_tree.name, fun_tree.ret_type.type, len(fun_tree.args), fun_tree.pos))
        self.addChild(fun_tree)
        self.addSymbol(fun_tree.getFunSymbol())

    def printTree(self):
        self.printT('>PROG')
        self.printChildren()
        self.printT('<PROG')

    def checkTypes(self):
        if self.hasSymbol(Builtins.MAIN):
            main_sym = self.symbol(Builtins.MAIN)
            main_exp = FunSymbol(Builtins.MAIN, Symbol('', LP.INT), [], None)
            if main_sym != main_exp:
                Status.addError(TypecheckError('"%s" has wrong type: %s' % 
                    (Builtins.MAIN, str(main_sym)), main_sym.pos))
                Status.addNote(TypecheckError('expected "%s" type: %s' %
                    (Builtins.MAIN, str(main_exp)), main_sym.pos))
        else:
            Status.addError('"%s" function not defined' % Builtins.MAIN)
        self.checkChildrenTypes()

    # wyjątel na czyszczenie tablicy symboli -- tu są funkcje i mają zostać
    def clearSymbols(self):
        for child in self.children:
            child.clearSymbols()


### function ######################################################################################
class FunTree(LatteTree):
    def __init__(self, **kwargs):
        super(FunTree, self).__init__(**kwargs)
        self.name = None
        self.ret_type = None
        self.args = []

    def setName(self, name):
        self.name = name
        if self.ret_type:
            self.ret_type.name = name
        self.savePos()

    def setRetType(self, ret_type):
        self.ret_type = Symbol(self.name, ret_type, Status.getCurPos())
        
    def setBlock(self, block):
        self.addChild(block)

    def addArg(self, arg):
        debug('fun addArg=%s type=%d at=%s' % (arg.name, arg.type, arg.pos))
        sym = Symbol(arg.name, arg.type, pos=arg.pos)
        self.addSymbol(sym)
        self.args.append(sym)

    def getFunSymbol(self):
        block = self.children[0] if self.children else None
        return FunSymbol(self.name, self.ret_type, self.args, block, self.pos)

    def printTree(self):
        self.printT('>FUN %s %s' % (self.name, str(self.getFunSymbol())))
        self.printChildren()
        self.printT('<FUN %s' % self.name)

    def getCurFun(self):
        return self

    def noReturnError(self, pos):
            Status.addError(TypecheckError('no return statement in function "%s" returning "%s"' %
                (self.name, str(self.ret_type)), pos))

    def checkTypes(self):
        self.checkChildrenTypes()
        # po sprawdzeniu dzieci sprawdzamy czy był return
        if self.ret_type.type != LP.VOID:
            if self.children:
                self.children[-1].checkReturn()
            else:
                self.noReturnError(self.pos)


### statement #####################################################################################
class StmtTree(LatteTree):
    def __init__(self, type=None, **kwargs):
        super(StmtTree, self).__init__(**kwargs)
        self.type = Symbol('', type, Status.getCurPos())
        self.has_return = False

    def printTree(self):
        self.printT('>%s' % str(self.type))
        if self.children:
            self.printChildren()
            self.printT('<%s' % str(self.type))

    # sprawdza, czy instrukcja ma na końcu/ach return (a właściwie wypisuje błąd, jeśli nie ma)
    def checkReturn(self):
        for case in switch(self.type.type):
            if case(LP.RETURN):
                return
            if case(LP.WHILE):
                # sprawdzamy czy instrukcja/blok ma returna
                self.children[1].checkReturn()
                return
            if case(LP.IF, LP.WHILE):
                # sprawdzamy oba bloki, jeśli nie ma else to też źle
                self.children[1].checkReturn()
                if len(self.children) >= 3:
                    self.children[2].checkReturn()
                else:
                    fun = self.getCurFun()
                    fun.noReturnError(self.pos)
                return
        # blok ma osobną klasę, więc wpp zwracamy błąd
        self.getCurFun().noReturnError(self.pos)


    def checkTypes(self):
        for case in switch(self.type.type):
            if case(LP.ASSIGN):
                # children: ident, expr
                self.children[1].expectType(self.children[0].getType())
                break
            if case(LP.INCR, LP.DECR):
                # child: ident
                Symbol('', LP.INT).checkWith(self.children[0].getType(), self.pos)
                break
            if case(LP.RETURN):
                self.has_return = True
                fun = self.getCurFun()
                if not self.children:
                    # zwracamy void
                    Symbol('', LP.VOID).checkWith(fun.ret_type, self.pos)
                else:
                    # child: expr, zwracamy jego wartość
                    self.children[0].expectType(fun.ret_type)
                break
            if case(LP.IF, LP.WHILE):
                # children: cond, stmt, (stmt?)
                self.children[0].expectType(Symbol('', LP.BOOLEAN))
                break
        self.checkChildrenTypes()
        # sprawdzamy czy za instrukcję nie robi wyrażenie zostawiające nieużyty wynik
        # (w bloku -- dowolne dziecko, w if/while nie będące warunkiem)
        for i in xrange(0, len(self.children)):
            ch = self.children[i]
            if (isinstance(ch, ExprTree) and ch.value_type.type != LP.VOID and
                    (self.type.type == LP.BLOCK or 
                        (self.type.type in [LP.IF, LP.WHILE] and i > 0)
                )):
                Status.addWarning(TypecheckError('unused result of expression', ch.pos))
                ch.unused_result = True


### code block ####################################################################################
class BlockTree(StmtTree):
    def __init__(self, **kwargs):
        super(BlockTree, self).__init__(LP.BLOCK, **kwargs)

    def addStmt(self, tree):
        # pos bloku = pos pierwszego dziecka
        if not self.children:
            self.pos = tree.pos
        self.addChild(tree)

    def getCurBlock(self):
        return self

    def checkReturn(self):
        if self.children:
            self.children[-1].checkReturn()
        else:
            self.getCurFun().noReturnError(self.pos)


### declaration ###################################################################################
class DeclTree(StmtTree):
    def __init__(self, dtype, **kwargs):
        super(DeclTree, self).__init__(LP.DECL, **kwargs)
        self.decl_type = Symbol('', dtype, Status.getCurPos())
        self.items = []

    def addItem(self, item):
        self.items.append(item)
        if item.expr:
            self.addChild(item.expr)

    def printTree(self):
        self.printT('>DECL %s' % str(self.decl_type))
        for item in self.items:
            self.printT('* %s' % item.name)
            if item.expr:
                item.expr.printTree()

    def checkTypes(self):
        block = self.getCurBlock()
        for item in self.items:
            dsym = Symbol(item.name, self.decl_type.type, item.pos)
            block.addSymbol(dsym)
            if item.expr:
                item.expr.expectType(self.decl_type)
        self.checkChildrenTypes()


### expression ####################################################################################
class ExprTree(StmtTree):
    def __init__(self, type=None, **kwargs):
        super(ExprTree, self).__init__(type, **kwargs)
        # niezmiennik:
        # * po wyjściu z checkTypes() value_type ma być ustawiony
        # * jeśli są ustawione value_type ORAZ expected_type, to się zgadzają lub poszedł error
        self.value_type = None
        self.expected_type = None
        self.unused_result = None

    def expectType(self, sym):
        self.expected_type = sym
        # jeśli mamy już typ zwracany, sprawdzamy od razu, wpp sprawdzi się po wyliczeniu
        if self.value_type:
            self.expected_type.checkWith(self.value_type, self.pos)

    def setValueType(self, sym):
        # kopiujemy symbol, żeby nie nadpisać innemu pozycji przez referencję
        self.value_type = Symbol('', sym.type, self.pos)
        # jeśli mamy już typ oczekiwany, sprawdzamy od razu, wpp sprawdzi się po zgłoszeniu
        if self.expected_type:
            self.expected_type.checkWith(self.value_type, self.pos)


### literal #######################################################################################
class LiteralTree(ExprTree):
    @classmethod
    def _getRealType(cls, type):
        for case in switch(type):
            if case(LP.NUMBER): return LP.INT
            if case(LP.STRINGLIT): return LP.STRING
            if case(LP.TRUE, LP.FALSE): return LP.BOOLEAN
        return type

    def __init__(self, type, value, **kwargs):
        real_type = self._getRealType(type)
        super(LiteralTree, self).__init__(real_type, **kwargs)
        self.value = value
        debug('literal %s: pos %s' % (self.value, self.pos))

    def printTree(self):
        self.printT('= %s %s' % (str(self.type), self.value))

    def getType(self):
        for case in switch(self.type.type):
            if case(LP.IDENT):
                if not self.hasSymbol(self.value):
                    Status.addError(TypecheckError('use of undefined variable "%s"' % self.value,
                        self.pos))
                    # TODO oznaczyć żeby nie dawał więcej błędów?
                    return None
                self.setValueType(self.symbol(self.value))
                break
            if case():
                self.setValueType(self.type)
        return self.value_type

    def checkTypes(self):
        # nic ciekawego, tylko zapisujemy
        self.getType()


### unary operator ################################################################################
class UnopTree(ExprTree):
    @classmethod
    def _getTypeidForOp(cls, op):
        for case in switch(op.type):
            if case(LP.NOT): return LP.BOOLEAN
            if case(LP.NEG): return LP.INT
        return None

    def __init__(self, type, expr, **kwargs):
        super(UnopTree, self).__init__(type, children=[expr], **kwargs)
        self.pos = Status.getCurPos(-2)

    def checkTypes(self):
        # child: expr
        # prosto, bo operator jednoznacznie mówi jakiego typu ma być wyrażenie
        optype = self._getTypeidForOp(self.type)
        self.setValueType(Symbol('', optype, self.pos))
        self.children[0].expectType(self.value_type)
        self.checkChildrenTypes()


### binary operator ###############################################################################
class BinopTree(ExprTree):
    _int_ops = [LP.MULT, LP.DIV, LP.MOD, LP.PLUS, LP.MINUS,
            LP.LT, LP.LEQ, LP.GT, LP.GEQ, LP.EQ, LP.NEQ]
    _boolean_ops = [LP.AND, LP.OR, LP.EQ, LP.NEQ]
    _string_ops = [LP.PLUS, LP.EQ, LP.NEQ]
    _rel_ops = [LP.LT, LP.LEQ, LP.GT, LP.GEQ, LP.EQ, LP.NEQ]
    @classmethod
    def _getPossibleOps(cls, type):
        for case in switch(type.type):
            if case(LP.INT): return cls._int_ops
            if case(LP.BOOLEAN): return cls._boolean_ops
            if case(LP.STRING): return cls._string_ops
        return []

    def __init__(self, type, expa, expb, **kwargs):
        super(BinopTree, self).__init__(type, children=[expa, expb], **kwargs)

    def isRelOp(self):
        return self.type.type in self._rel_ops

    def checkTypes(self):
        # children: expa, expb
        self.children[0].checkTypes()
        # operatory logiczne zwracają boolean, wpp pierwsze wyrażenie niech definiuje typ całego
        if self.isRelOp():
            self.setValueType(Symbol('', LP.BOOLEAN, self.pos))
        else:
            self.setValueType(self.children[0].value_type)
        # sprawdzamy czy operator działa dla tego typu
        ops = self._getPossibleOps(self.children[0].value_type)
        if self.type.type in ops:
            # drugie wyrażenie musi mieć taki sam typ
            self.children[1].expectType(self.children[0].value_type)
        else:
            Status.addError(TypecheckError('operator cannot accept "%s" argument' %
                str(self.children[0].value_type), self.pos))
        self.children[1].checkTypes()


### function call #################################################################################
class FuncallTree(ExprTree):
    def __init__(self, fname, **kwargs):
        super(FuncallTree, self).__init__(LP.FUNCALL, **kwargs)
        self.fname = fname

    def printTree(self):
        self.printT('>FUNCALL %s' % self.fname)
        self.printChildren()
        self.printT('<FUNCALL %s' % self.fname)

    def checkTypes(self):
        # najpierw sprawdzamy czy w ogóle jest taka funkcja
        if not self.hasSymbol(self.fname):
            Status.addError(TypecheckError('call to undefined function "%s"' % self.fname, self.pos))
            return
        fsym = self.symbol(self.fname)
        if not fsym.isFunction():
            Status.addError(TypecheckError('cannot call symbol "%s" of type "%s"' %
                (self.fname, str(fsym)), self.pos))
            return
        # sprawdzamy liczbę i zgodność typów
        self.setValueType(fsym.ret_type)
        if len(self.children) != len(fsym.args):
            Status.addError(TypecheckError('%d arguments given, function "%s" takes %d' %
                (len(self.children), self.fname, len(fsym.args)), self.pos))
            if fsym.pos: # nie ma pozycji to zapewne funkcja wbudowana -> komunikat byłby bez sensu
                Status.addNote(TypecheckError('as declared here', fsym.pos))
        for i in xrange(0, min(len(self.children), len(fsym.args))):
            self.children[i].expectType(fsym.args[i])
        self.checkChildrenTypes()


