#!/usr/bin/python2
# -*- coding: utf8 -*-

from antlr3.tree import CommonTree, CommonTreeAdaptor
from FuturePrint import debug, message, warning, error, note
from LatteErrors import Status, TypecheckError
import LatteParser as LP


class Symbol(object):
    """ Class representing a symbol (name, type and possibly location of declaration). """
    def __init__(self, name, type, pos=None):
        super(Symbol, self).__init__()
        self.name = name
        self.pos = pos
        self.type = type

    def __eq__(self, other):
        """ Type matching. """
        if isinstance(other, Symbol):
            return self.type == other.type
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented: return result
        return not result

    def __str__(self):
        return LP.tokenNames[self.type].lower()

    def isFunction(self):
        return False

    def checkWith(self, other, pos):
        """ Check if two symbols have matching type. """
        if not other:
            debug('checkWith on %s with None' % (str(self)))
            return # Assuming that None here means an error was already reported.
        if not self == other:
            Status.addError(TypecheckError('expression has type "%s", expected "%s"' %
                (str(other), str(self)), pos))


class FunSymbol(Symbol):
    """ A special kind of symbol for function declarations. """

    def __init__(self, name, ret_type, args, block, pos=None):
        """ `args` is a list of Symbol instances -- function's argument types. """
        super(FunSymbol, self).__init__(name, LP.FUNDEF, pos)
        self.ret_type = ret_type
        self.args = args
        self.block = block
        self.defined = False

    def __eq__(self, other):
        """ If the other symbol is also a function, check type and argument types. """
        if isinstance(other, FunSymbol):
            return self.ret_type == other.ret_type and self.args == other.args
        # Otherwise fall back to Symbol's logic -- it will return false or NotImplemented.
        return super(FunSymbol, self).__eq__(other)

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented: return result
        return not result

    def defined(self):
        return self.defined

    def markDefined(self):
        self.defined = True

    def __str__(self):
        args = ', '.join(map(str, self.args))
        return 'function (' + args + ') -> ' + str(self.ret_type)
    
    def isFunction(self):
        return True
    

# A switch-like construction from z http://code.activestate.com/recipes/410692
# This class provides the functionality we want. You only need to look at
# this if you want to know how this works. It only needs to be defined
# once, no need to muck around with its internals.
class switch(object):
    def __init__(self, value):
        self.value = value
        self.fall = False

    def __iter__(self):
        """Return the match method once, then stop"""
        yield self.match
        raise StopIteration
    
    def match(self, *args):
        """Indicate whether or not to enter a case suite"""
        if self.fall or not args:
            return True
        elif self.value in args: # changed for v1.5, see below
            self.fall = True
            return True
        else:
            return False


### function argument #############################################################################
class FunArg(object):
    """ A simple class for a funcion argument, before it gets registered as a symbol. """
    def __init__(self, type, name):
        self.type = type
        self.name = name
        self.pos = Status.getCurPos(-2)


### single declaration ############################################################################
class DeclArg(object):
    """ A simple class for a declared item, before it gets registered as a symbol. """
    def __init__(self, name, pos, expr=None):
        self.name = name
        self.expr = expr
        self.pos = pos
