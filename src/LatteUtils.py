#!/usr/bin/python2
# -*- coding: utf8 -*-

from antlr3.tree import CommonTree, CommonTreeAdaptor
from FuturePrint import debug, message, warning, error, note
from LatteErrors import Status, TypecheckError
import LatteParser as LP


class Symbol(object):

    def __init__(self, name, type, pos=None):
        super(Symbol, self).__init__()
        self.name = name
        self.pos = pos
        self.type = type

    def __eq__(self, other):
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
        if not other:
            debug('checkWith on %s with None' % (str(self)))
            return # zakładamy że skoro jest None to błąd już był wcześniej
        if not self == other:
            Status.addError(TypecheckError('expression has type "%s", expected "%s"' %
                (str(other), str(self)), pos))


class FunSymbol(Symbol):
    """
    args - lista typów argumentów, [Symbol]
    """

    def __init__(self, name, ret_type, args, block, pos=None):
        super(FunSymbol, self).__init__(name, LP.FUNDEF, pos)
        self.ret_type = ret_type
        self.args = args
        self.block = block
        self.defined = False

    def __eq__(self, other):
        if isinstance(other, FunSymbol):
            return self.ret_type == other.ret_type and self.args == other.args
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
    

#class LatteAST(CommonTree):
    #def __init__(self, payload):
        #super(LatteAST, self).__init__(payload)
        #self.first_token = self.getTokenStartIndex()
        #self.pos = Status.getPos(self.first_token)
        ##debug('%s: %s' % (self.pos, (LP.tokenNames[self.token.type] if self.token else '??')))

    #def dupNode(self):
        #return LatteAST(self)

    #def posasdf(self):
        #return self.pos


#class LatteASTAdaptor(CommonTreeAdaptor):
    #def createWithPayload(self, payload):
        #return LatteAST(payload)

# switch z http://code.activestate.com/recipes/410692
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
    def __init__(self, type, name):
        self.type = type
        self.name = name
        self.pos = Status.getCurPos(-2)


### single declaration ############################################################################
class DeclArg(object):
    def __init__(self, name, pos, expr=None):
        self.name = name
        self.expr = expr
        self.pos = pos
