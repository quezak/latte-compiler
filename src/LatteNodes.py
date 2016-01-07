#!/usr/bin/python2
# -*- coding: utf8 -*-

import abc
import LatteParser as LP
from FuturePrint import debug
from LatteParser import Builtins
from LatteUtils import Symbol, FunSymbol
from LatteErrors import Status, TypecheckError
from Utils import switch


### node ABC ######################################################################################
class LatteTree(object):
    """ Abstract base class for all node kinds. """
    __metaclass__ = abc.ABCMeta
    _ltcounter = 0

    def __init__(self, **kwargs):
        """ Initializations common for all nodes, e.g. position and children saving. """
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
            # TODO test if we still need those
            #if not self.pos or self.pos == '0:0':
                #self.savePos(-2)
            #if not self.pos or self.pos == '0:0':
                #self.savePos(-3)


    def getCounter(self):
        return self._ltcounter

    def addSymbol(self, symbol):
        """ Add a symbol in the current context. """
        name = symbol.name
        # TODO czy aby na pewno?
        # not self.hasSymbol(name) -- we search for conflicts only on the current level
        if name in self.symbols:
            Status.addError(TypecheckError('conflicting declaration of "%s" as "%s"' % (
                name, str(symbol)), symbol.pos))
            if self.symbol(name).pos:
                msg = 'previously declared here as "%s"'
            else:
                msg = 'previously declared built-in as "%s"'
            Status.addNote(TypecheckError(msg %
                str(self.symbol(name)), self.symbol(name).pos))
        elif self.hasSymbol(name):
            debug('%s: shadowing symbol "%s %s"' % (symbol.pos, str(symbol), name))
        self.symbols[name] = symbol

    def hasSymbol(self, name):
        """ Check if there is a symbol with a given name declared. """
        if name in self.symbols:
            return True
        return (self.parent) and (self.parent.hasSymbol(name))

    def symbol(self, name):
        """ Return the symbol with given name or None, if there is none. """
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.symbol(name)
        return None

    @abc.abstractmethod
    def printTree(self):
        """ Prints the whole (sub)tree, properly indenting children. """
        pass

    def _printChildren(self):
        """ Internal function used by printTree(). """
        for child in self.children:
            child.printTree()

    def _printIndented(self, msg):
        """ Internal function used by printTree(). """
        debug((' ' * 2 * self.level) + msg)

    def _setLevel(self, level):
        """ Internal function used by printTree(). """
        self.level = level
        for child in self.children:
            child._setLevel(level+1)

    def checkChildrenTypes(self):
        for child in self.children:
            child.checkTypes()

    def checkTypes(self):
        """ Checks all the types and returns in the subtree. """
        self.checkChildrenTypes()

    def getCurFun(self):
        """ Returns the node of the current function. """
        if not self.parent:
            return None
        return self.parent.getCurFun()

    def getCurBlock(self):
        """ Returns the closest (up the tree) block node. """
        if not self.parent:
            return None
        return self.parent.getCurBlock()

    def _clearSymbols(self):
        """ Clears the symbol tables. """
        self.symbols = dict()
        for child in self.children:
            child._clearSymbols()


### program #######################################################################################
class ProgTree(LatteTree):
    """ Node representing the whole program. """
    def __init__(self, **kwargs):
        super(ProgTree, self).__init__(**kwargs)
        self.addBuiltinSymbols()

    def addBuiltinSymbols(self):
        """ Adds declarations of builtin symbol to the main program node. """
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
        """ Adds a function node to the program tree. """
        debug('addFunTree name=%s ret=%d argcount=%d pos=%s' %
                (fun_tree.name, fun_tree.ret_type.type, len(fun_tree.args), fun_tree.pos))
        self.addChild(fun_tree)
        self.addSymbol(fun_tree.getFunSymbol())

    def printTree(self):
        if not self.level:
            self._setLevel(0)
        self._printIndented('>PROG')
        self._printChildren()
        self._printIndented('<PROG')

    def checkTypes(self):
        """ Checks all the types, including existence and type of main function. """
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
        # Checking the symbols builds symbol tables, so we need to clear them before continuing.
        self._clearSymbols()

    def _clearSymbols(self):
        """ Clears symbol tables except for the function declarations. """
        for child in self.children:
            child._clearSymbols()


### function ######################################################################################
class FunTree(LatteTree):
    """ Node representing a single function definition. """
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

    # TODO chyba nie powinnismy czyscic argumentow z tablicy symboli?
    def addArg(self, arg):
        debug('fun addArg=%s type=%d at=%s' % (arg.name, arg.type, arg.pos))
        sym = Symbol(arg.name, arg.type, pos=arg.pos)
        self.addSymbol(sym)
        self.args.append(sym)

    def getFunSymbol(self):
        block = self.children[0] if self.children else None
        return FunSymbol(self.name, self.ret_type, self.args, block, self.pos)

    def printTree(self):
        self._printIndented('>FUN %s %s' % (self.name, str(self.getFunSymbol())))
        self._printChildren()
        self._printIndented('<FUN %s' % self.name)

    def getCurFun(self):
        """ Returns the node of the current function. """
        return self

    def noReturnError(self, pos):
            Status.addError(TypecheckError('no return statement in function "%s" returning "%s"' %
                (self.name, str(self.ret_type)), pos))

    def checkTypes(self):
        self.checkChildrenTypes()
        # After checking types in child nodes, check if a value is returned where needed (in the
        # last statement of non-void functions).
        if self.ret_type.type != LP.VOID:
            if self.children:
                self.children[-1].checkReturn()
            else:
                self.noReturnError(self.pos)


### statement #####################################################################################
class StmtTree(LatteTree):
    """ Node representing one statement. """
    def __init__(self, type=None, **kwargs):
        super(StmtTree, self).__init__(**kwargs)
        self.type = Symbol('', type, Status.getCurPos())
        self.has_return = False

    def printTree(self):
        if self.children:
            self._printIndented('>%s' % str(self.type))
            self._printChildren()
            self._printIndented('<%s' % str(self.type))
        else:
            self._printIndented('|%s' % str(self.type))

    # sprawdza, czy instrukcja ma na końcu/ach return (a właściwie wypisuje błąd, jeśli nie ma)
    def checkReturn(self):
        """ Checks if each branch returns a value if needed. Launched by after typechecking
        for the last statement of each function. """
        for case in switch(self.type.type):
            if case(LP.RETURN): # return -- obvious case.
                return
            if case(LP.WHILE): # while -- check the underlying block/instrunction.
                self.children[1].checkReturn()
                return
            if case(LP.IF):
                # if -- check the underlying blocks. In particular, fail if there is no else block,
                # since this is the last function's statement and a false condition would result
                # in no value returned.
                self.children[1].checkReturn()
                # TODO make a 'hasElse()' function in IF?
                if len(self.children) >= 3:
                    self.children[2].checkReturn()
                else:
                    fun = self.getCurFun()
                    fun.noReturnError(self.pos)
                return
        # A block has its own node subclass, so in fail in other cases.
        self.getCurFun().noReturnError(self.pos)


    def checkTypes(self):
        for case in switch(self.type.type):
            if case(LP.ASSIGN): # Children: ident, expr, check if types match.
                self.children[1].expectType(self.children[0].getType())
                break
            if case(LP.INCR, LP.DECR): # Child: ident, expected type: int.
                Symbol('', LP.INT).checkWith(self.children[0].getType(), self.pos)
                break
            if case(LP.RETURN): # Check if returned value type matches the function declaration.
                self.has_return = True
                fun = self.getCurFun()
                if not self.children: # No value returned: check if function returns void.
                    Symbol('', LP.VOID).checkWith(fun.ret_type, self.pos)
                else: # Check the returned expression.
                    self.children[0].expectType(fun.ret_type)
                break
            if case(LP.IF, LP.WHILE): # Children: cond, stmt+. Check if condition is boolean.
                self.children[0].expectType(Symbol('', LP.BOOLEAN))
                break
        self.checkChildrenTypes()
        # Warn about unused results -- cases where an non-void expression is used as a statement.
        # For if/while -- don't check the condition.
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
    """ Node representing a block of instructions. """
    def __init__(self, **kwargs):
        super(BlockTree, self).__init__(LP.BLOCK, **kwargs)

    def addStmt(self, tree):
        # A block has the position of its first statement.
        if not self.children:
            self.pos = tree.pos
        self.addChild(tree)

    def getCurBlock(self):
        """ Returns the closest (up the tree) block node. """
        return self

    def checkReturn(self):
        """ Checks if the block returns a value if needed. Launched by after typechecking
        for the last statement of each function. """
        if self.children:
            self.children[-1].checkReturn()
        else:
            self.getCurFun().noReturnError(self.pos)


### declaration ###################################################################################
class DeclTree(StmtTree):
    """ Node representing a declaration (possibly of many variables of the same type). """
    def __init__(self, dtype, **kwargs):
        super(DeclTree, self).__init__(LP.DECL, **kwargs)
        self.decl_type = Symbol('', dtype, Status.getCurPos())
        self.items = []

    def addItem(self, item):
        self.items.append(item)
        if item.expr:
            self.addChild(item.expr)

    def printTree(self):
        self._printIndented('>DECL %s' % str(self.decl_type))
        for item in self.items:
            self._printIndented('* %s' % item.name)
            if item.expr:
                item.expr.printTree()
        self._printIndented('<DECL %s' % str(self.decl_type))

    def checkTypes(self):
        """ Check types of the expressions assigned to the declared variables, if any. """
        block = self.getCurBlock()
        for item in self.items:
            dsym = Symbol(item.name, self.decl_type.type, item.pos)
            block.addSymbol(dsym)
            if item.expr:
                item.expr.expectType(self.decl_type)
        self.checkChildrenTypes()


### expression ####################################################################################
class ExprTree(StmtTree):
    """ Node representing one expression.
    
    Typechecking invariants:
        * when checkTypes() returns value_type must be set
        * if value_type and expected_type are both set, they match xor an error was reported. """
    def __init__(self, type=None, **kwargs):
        super(ExprTree, self).__init__(type, **kwargs)
        self.value_type = None
        self.expected_type = None
        self.unused_result = None

    def expectType(self, sym):
        self.expected_type = sym
        if self.value_type:
            self.expected_type.checkWith(self.value_type, self.pos)

    def setValueType(self, sym):
        # Note: don't just assign the symbol, not to overwrite its position.
        self.value_type = Symbol('', sym.type, self.pos)
        if self.expected_type:
            self.expected_type.checkWith(self.value_type, self.pos)


### literal #######################################################################################
class LiteralTree(ExprTree):
    """ Node representing a literal expression. """
    @classmethod
    def _getRealType(cls, type):
        """ Translate lexer's literal types to actual data types. """
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
        self._printIndented('= %s %s' % (str(self.type), self.value))

    def getType(self):
        for case in switch(self.type.type):
            if case(LP.IDENT):
                if not self.hasSymbol(self.value):
                    Status.addError(TypecheckError('use of undefined variable "%s"' % self.value,
                        self.pos))
                    # TODO add a fake symbol to prevent more errors?
                    return None
                self.setValueType(self.symbol(self.value))
                break
            if case():
                self.setValueType(self.type)
        return self.value_type

    def checkTypes(self):
        # Only save the type.
        self.getType()


### unary operator ################################################################################
class UnopTree(ExprTree):
    """ Node for unary expressions. """
    @classmethod
    def _getTypeidForOp(cls, op):
        """ Return a type that can be used with the operator (only one for now). """
        for case in switch(op.type):
            if case(LP.NOT): return LP.BOOLEAN
            if case(LP.NEG): return LP.INT
        return None

    def __init__(self, type, expr, **kwargs):
        super(UnopTree, self).__init__(type, children=[expr], **kwargs)
        self.pos = Status.getCurPos(-2)

    def checkTypes(self):
        # Simple case, because the operator unambiguously defines the type.
        optype = self._getTypeidForOp(self.type)
        self.setValueType(Symbol('', optype, self.pos))
        self.children[0].expectType(self.value_type)
        self.checkChildrenTypes()


### binary operator ###############################################################################
class BinopTree(ExprTree):
    """ Node for binary expressions. """
    # Possible operator lists for each type.
    _int_ops = [LP.MULT, LP.DIV, LP.MOD, LP.PLUS, LP.MINUS,
            LP.LT, LP.LEQ, LP.GT, LP.GEQ, LP.EQ, LP.NEQ]
    _boolean_ops = [LP.AND, LP.OR, LP.EQ, LP.NEQ]
    _string_ops = [LP.PLUS, LP.EQ, LP.NEQ]
    _rel_ops = [LP.LT, LP.LEQ, LP.GT, LP.GEQ, LP.EQ, LP.NEQ]
    @classmethod
    def _getPossibleOps(cls, type):
        """ Return a list of operators that can be used with a given type. """
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
        # [1] Compute the type of first subexpression.
        self.children[0].checkTypes()
        # [1] Set the type: boolean for logical operators, otherwise the first expr's type.
        if self.isRelOp():
            self.setValueType(Symbol('', LP.BOOLEAN, self.pos))
        else:
            self.setValueType(self.children[0].value_type)
        # [2] Check if the operator works for the selected type.
        ops = self._getPossibleOps(self.children[0].value_type)
        if self.type.type in ops:
            # [3] Mark that the second subexpression should have the same type.
            self.children[1].expectType(self.children[0].value_type)
        else:
            Status.addError(TypecheckError('operator cannot accept "%s" argument' %
                str(self.children[0].value_type), self.pos))
        # [4] Check the second subexpression.
        self.children[1].checkTypes()


### function call #################################################################################
class FuncallTree(ExprTree):
    """ Node for function calls. """
    def __init__(self, fname, **kwargs):
        super(FuncallTree, self).__init__(LP.FUNCALL, **kwargs)
        self.fname = fname

    def printTree(self):
        self._printIndented('>FUNCALL %s' % self.fname)
        self._printChildren()
        self._printIndented('<FUNCALL %s' % self.fname)

    def checkTypes(self):
        # [1] Check if the called name exists and is a function.
        if not self.hasSymbol(self.fname):
            Status.addError(TypecheckError('call to undefined function "%s"' % self.fname, self.pos))
            return
        fsym = self.symbol(self.fname)
        if not fsym.isFunction():
            Status.addError(TypecheckError('cannot call symbol "%s" of type "%s"' %
                (self.fname, str(fsym)), self.pos))
            return
        # [2] Check the number of arguments.
        self.setValueType(fsym.ret_type)
        if len(self.children) != len(fsym.args):
            Status.addError(TypecheckError('%d arguments given, function "%s" takes %d' %
                (len(self.children), self.fname, len(fsym.args)), self.pos))
            if fsym.pos: # Without position it's probably a builtin and the note wouldn't help.
                Status.addNote(TypecheckError('as declared here', fsym.pos))
        # [3] Check the types of arguments.
        for i in xrange(0, min(len(self.children), len(fsym.args))):
            self.children[i].expectType(fsym.args[i])
        self.checkChildrenTypes()


