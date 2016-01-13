#!/usr/bin/python2
# -*- coding: utf8 -*-

from FuturePrint import debug
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
        if result is NotImplemented:
            return result
        return not result

    def __str__(self):
        return LP.tokenNames[self.type].lower()

    def is_function(self):
        return False

    def check_with(self, other, pos):
        """ Check if two symbols have matching type. """
        if not other:
            debug('check_with on %s with None' % (str(self)))
            return  # Assuming that None here means an error was already reported.
        # If either type is TYPE_ERROR, it means an error was already reported.
        if (not self == other) and self.type != LP.TYPE_ERROR and other.type != LP.TYPE_ERROR:
            Status.add_error(TypecheckError('expression has type `%s`, expected `%s`' %
                                            (str(other), str(self)), pos))


class FunSymbol(Symbol):
    """ A special kind of symbol for function declarations. """

    def __init__(self, name, ret_type, args, block, pos=None):
        """ `args` is a list of Symbol instances -- function's argument types. """
        super(FunSymbol, self).__init__(name, LP.FUNDEF, pos)
        self.is_builtin = block is None or name == LP.Builtins.MAIN
        self.ret_type = ret_type
        self.args = args
        self.block = block
        self.call_counter = 0

    def __eq__(self, other):
        """ If the other symbol is also a function, check type and argument types. """
        if isinstance(other, FunSymbol):
            return self.ret_type == other.ret_type and self.args == other.args
        # Otherwise fall back to Symbol's logic -- it will return false or NotImplemented.
        return super(FunSymbol, self).__eq__(other)

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented:
            return result
        return not result

    def __str__(self):
        args = ', '.join(map(str, self.args))
        return 'function (' + args + ') -> ' + str(self.ret_type)

    def is_function(self):
        return True


class FunArg(object):
    """ A simple class for a funcion argument, before it gets registered as a symbol. """
    def __init__(self, type, name):
        self.type = type
        self.name = name
        self.pos = Status.get_cur_pos(-2)


class DeclArg(object):
    """ A simple class for a declared item, before it gets registered as a symbol. """
    def __init__(self, name, pos, expr=None):
        self.name = name
        self.expr = expr
        self.pos = pos
