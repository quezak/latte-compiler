#!/usr/bin/python2
# -*- coding: utf8 -*-
""" Helper classes for LatteNodes: symbols for the symbol tables. """

from FuturePrint import debug
from LatteErrors import Status, TypecheckError, InternalError
import LatteParser as LP


class DataType(object):
    """ Class representing data type in Latte. """

    # list of parser's typeids
    PLAIN_TYPES = [LP.INT, LP.STRING, LP.BOOLEAN, LP.VOID, LP.TYPE_ERROR]

    def __init__(self, type, subtype=None):
        # Check if called with LP.* typeids or another DataType
        self.id = DataType.get_typeid(type)
        self.subtype = None
        if isinstance(type, DataType):
            self.subtype = type.subtype
        if subtype:
            self.subtype = subtype
        assert(self.id is None or isinstance(self.id, int))
        assert(self.subtype is None or
               (self.id == LP.ARRAY and isinstance(self.subtype, DataType)) or
               (self.id == LP.OBJECT and isinstance(self.subtype, str)))
    
    @classmethod
    def get_typeid(cls, type):
        """ Return the typeid from either a DataType object or int. """
        if isinstance(type, DataType):
            return type.id
        else:
            return type

    @classmethod
    def mkarray(cls, subtype):
        """ Factory method returning array types. """
        return cls(LP.ARRAY, cls(subtype))

    @classmethod
    def mkobject(cls, classname):
        """ Factory method returning class types. """
        if isinstance(classname, DataType):
            return cls(LP.OBJECT, classname.subtype)
        return cls(LP.OBJECT, classname)

    def __eq__(self, other):
        """ Type matching -- allow comparision with LP.* typeids. """
        if isinstance(other, int):
            return self.id == other
        if isinstance(other, DataType):
            return self.id == other.id and self.subtype == other.subtype
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented:
            return result
        return not result

    def __str__(self):
        s = LP.tokenNames[self.id].lower()
        if self.id == LP.ARRAY:
            s = '%s[]' % str(self.subtype)
        elif self.id == LP.OBJECT:
            s += ' %s' % self.subtype
        return s


class Symbol(object):

    prog = None  # to be set from ProgNode

    """ Class representing a symbol (name, type and possibly location of declaration). """
    def __init__(self, name, type, pos=None, cls=None):
        super(Symbol, self).__init__()
        self.name = name
        self.pos = pos
        self.type = DataType(type)
        self.cls = cls
        if self.type == LP.OBJECT and (not cls):
            self.cls = Symbol.prog.get_class(self.type.subtype)

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
        return str(self.type)

    def full_name(self):
        return (self.cls.name + '::' if self.cls else '') + self.name

    def is_function(self):
        return False

    def is_object(self):
        return self.type == LP.OBJECT

    def _check_with(self, other):
        if self.type == LP.OBJECT and other.type == LP.OBJECT:
            debug('OBJ CMP: %s | %s' % (str(self.type), str(other.type)))
        if self == other:
            if self.type == LP.OBJECT and other.type == LP.OBJECT:
                debug('      -> TRUE')
            return True
        if self.type == LP.OBJECT and other.type == LP.OBJECT and other.cls.base:
            return self._check_with(Symbol(other.name,
                                           DataType(LP.OBJECT, subtype=other.cls.base.name),
                                           other.pos, other.cls.base))
                                           
        return False

    def check_with(self, other, pos):
        """ Check if two symbols have matching type. """
        if not other:
            debug('check_with on %s with None' % (str(self)))
            return  # Assuming that None here means an error was already reported.
        # If either type is TYPE_ERROR, it means an error was already reported.
        if (self.type != LP.TYPE_ERROR and other.type != LP.TYPE_ERROR):
            if not self._check_with(other):
                Status.add_error(TypecheckError('expression has type `%s`, expected `%s`' %
                                                (str(other), str(self)), pos))


class FunSymbol(Symbol):
    """ A special kind of symbol for function declarations. """

    def __init__(self, name, ret_type, args, tree, pos=None, cls=None):
        """ `args` is a list of Symbol instances -- function's argument types. """
        super(FunSymbol, self).__init__(name, LP.FUNDEF, pos, cls)
        self.is_builtin = (not tree) or name == LP.Builtins.MAIN
        self.ret_type = ret_type
        self.args = args
        self.tree = tree
        self.call_counter = 0

    def call_name(self):
        if self.cls:
            return self.tree.name
        return self.name

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
