#!/usr/bin/python2
# -*- coding: utf8 -*-
""" Nodes of the main code tree, which is build from LatteParser's AST. They also contain methods
for checking expression and return types, function calls, returns on every branch and more static
checks. """

import abc
import LatteParser as LP
from FuturePrint import debug
from LatteParser import Builtins
from LatteUtils import Symbol, FunSymbol, DataType, DeclArg
from LatteErrors import Status, TypecheckError, InternalError
from Utils import switch


# node ABC ######################################################################################
class LatteTree(object):
    """ Abstract base class for all node kinds. """
    __metaclass__ = abc.ABCMeta
    _ltcounter = 0

    def __init__(self, **kwargs):
        """ Initializations common for all nodes, e.g. position and children saving. """
        super(LatteTree, self).__init__()
        self.ltnum = LatteTree._ltcounter
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
            self.save_pos(kwargs['pos_off'])
        else:
            self.save_pos()
        if 'children' in kwargs:
            for child in kwargs['children']:
                self.add_child(child)

    def add_child(self, tree):
        self.children.append(tree)
        tree.set_parent(self)

    def add_child_front(self, tree):
        self.children.insert(0, tree);
        tree.set_parent(self)

    def set_parent(self, tree):
        self.parent = tree

    def save_pos(self, offset=None):
        if offset:
            self.pos = Status.get_cur_pos(offset)
        else:
            self.save_pos(-1)

    def get_counter(self):
        return self._ltcounter

    def add_symbol(self, symbol):
        """ Add a symbol in the current context. """
        name = symbol.name
        # not self.has_symbol(name) -- we search for conflicts only on the current level
        if name in self.symbols:
            Status.add_error(TypecheckError('conflicting declaration of `%s` as `%s`' % (
                name, str(symbol)), symbol.pos))
            if self.symbol(name).pos:
                msg = 'previously declared here as `%s`'
            else:
                msg = 'previously declared built-in as `%s`'
            Status.add_note(TypecheckError(msg % str(self.symbol(name)), self.symbol(name).pos))
        elif self.has_symbol(name):
            debug('%s: shadowing symbol `%s %s`' % (symbol.pos, str(symbol), name))
        self.symbols[name] = symbol

    def has_symbol(self, name):
        """ Check if there is a symbol with a given name declared. """
        if name in self.symbols:
            return True
        return (self.parent) and (self.parent.has_symbol(name))

    def symbol(self, name):
        """ Return the symbol with given name or None, if there is none. """
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.symbol(name)
        return None

    @abc.abstractmethod
    def print_tree(self):
        """ Prints the whole (sub)tree, properly indenting children. """
        pass

    def _print_children(self):
        """ Internal function used by print_tree(). """
        for child in self.children:
            child.print_tree()

    def _print_indented(self, msg):
        """ Internal function used by print_tree(). """
        debug((' ' * 2 * self.level) + msg)

    def _set_level(self, level):
        """ Internal function used by print_tree(). """
        self.level = level
        for child in self.children:
            child._set_level(level+1)

    def check_children_types(self):
        for child in self.children:
            child.check_types()

    def check_types(self):
        """ Checks all the types and returns in the subtree. """
        self.check_children_types()

    def get_cur_fun(self):
        """ Returns the node of the current function. """
        if not self.parent:
            return None
        return self.parent.get_cur_fun()

    def get_cur_block(self):
        """ Returns the closest (up the tree) block node. """
        if not self.parent:
            return None
        return self.parent.get_cur_block()

    def _clear_symbols(self):
        """ Clears the symbol tables. """
        self.symbols = dict()
        for child in self.children:
            child._clear_symbols()


# program #######################################################################################
class ProgTree(LatteTree):
    """ Node representing the whole program. """
    def __init__(self, **kwargs):
        super(ProgTree, self).__init__(**kwargs)
        self.add_builtin_symbols()

    def add_builtin_symbols(self):
        """ Adds declarations of builtin symbol to the main program node. """
        debug('add_buildin_symbols')
        self._add_builtin_fun(Builtins.PRINT_INT, LP.VOID, [Symbol('', LP.INT)])
        self._add_builtin_fun(Builtins.PRINT_STRING, LP.VOID, [Symbol('', LP.STRING)])
        self._add_builtin_fun(Builtins.READ_INT, LP.INT, [])
        self._add_builtin_fun(Builtins.READ_STRING, LP.STRING, [])
        self._add_builtin_fun(Builtins.ERROR, LP.VOID, [])

    def _add_builtin_fun(self, name, ret_type, args):
        debug('add_builtin %s' % name)
        sym = FunSymbol(name, Symbol('', ret_type), args, None)
        self.add_symbol(sym)

    def add_fun_tree(self, fun_tree):
        """ Adds a function node to the program tree. """
        debug('add_fun_tree name=%s ret=%s argcount=%d pos=%s' %
              (fun_tree.name, str(fun_tree.ret_type), len(fun_tree.args), fun_tree.pos))
        self.add_child(fun_tree)
        self.add_symbol(fun_tree.get_fun_symbol())

    def print_tree(self):
        if not self.level:
            self._set_level(0)
        self._print_indented('>PROG')
        self._print_children()
        self._print_indented('<PROG')

    def check_types(self):
        """ Checks all the types, including existence and type of main function. """
        if self.has_symbol(Builtins.MAIN):
            main_sym = self.symbol(Builtins.MAIN)
            main_exp = FunSymbol(Builtins.MAIN, Symbol('', LP.INT), [], None)
            if main_sym != main_exp:
                Status.add_error(TypecheckError('`%s` has wrong type: `%s`' %
                                                (Builtins.MAIN, str(main_sym)), main_sym.pos))
                Status.add_note(TypecheckError('expected `%s` typeeype: `%s`' %
                                               (Builtins.MAIN, str(main_exp)), main_sym.pos))
        else:
            Status.add_error(TypecheckError('`%s` function not defined' % Builtins.MAIN, None))
        self.check_children_types()
        # Warn about unused non-builtin functions
        for sym in self.symbols.values():
            if sym.is_function() and not sym.is_builtin and sym.call_counter == 0:
                Status.add_warning(TypecheckError(
                    'function `%s` defined but never called' % sym.name, sym.pos))
        # Checking the symbols builds symbol tables, so we need to clear them before continuing.
        self._clear_symbols()

    def _clear_symbols(self):
        """ Clears symbol tables except for the function declarations. """
        for child in self.children:
            child._clear_symbols()


# function ######################################################################################
class FunTree(LatteTree):
    """ Node representing a single function definition. """
    def __init__(self, **kwargs):
        super(FunTree, self).__init__(**kwargs)
        self.name = None
        self.ret_type = None
        self.args = []

    def set_name(self, name):
        self.name = name
        if self.ret_type:
            self.ret_type.name = name
        self.save_pos()

    def set_ret_type(self, ret_type):
        self.ret_type = Symbol(self.name, ret_type, Status.get_cur_pos())

    def set_block(self, block):
        self.add_child(block)

    def add_arg(self, arg):
        debug('fun add_arg=%s type=%s at=%s' % (arg.name, str(arg.type), arg.pos))
        sym = Symbol(arg.name, arg.type, pos=arg.pos)
        self.add_symbol(sym)
        self.args.append(sym)

    def get_fun_symbol(self):
        block = self.children[0] if self.children else None
        return FunSymbol(self.name, self.ret_type, self.args, block, self.pos)

    def print_tree(self):
        self._print_indented('>FUN %s %s' % (self.name, str(self.get_fun_symbol())))
        self._print_children()
        self._print_indented('<FUN %s' % self.name)

    def get_cur_fun(self):
        """ Returns the node of the current function. """
        return self

    def no_return_error(self, pos):
        Status.add_error(TypecheckError(
            'missing return statement in function `%s` returning `%s`' %
            (self.name, str(self.ret_type)), pos if pos != '0:0' else self.pos))

    def check_types(self):
        for arg in self.args:
            if arg.type == LP.VOID:
                Status.add_error(TypecheckError('`void` function argument', arg.pos))
        self.check_children_types()
        # For non-void functions, check if the block always returns.
        if self.ret_type.type != LP.VOID:
            ret_stmt = self.children[0].check_return()
            if not ret_stmt:
                self.no_return_error(self.children[0].pos)


# statement #####################################################################################
class StmtTree(LatteTree):
    """ Node representing one statement. """
    def __init__(self, type=None, **kwargs):
        super(StmtTree, self).__init__(**kwargs)
        self.type = Symbol('', type, Status.get_cur_pos())

    def print_tree(self):
        if self.children:
            self._print_indented('>%s' % str(self.type))
            self._print_children()
            self._print_indented('<%s' % str(self.type))
        else:
            self._print_indented('|%s' % str(self.type))

    def check_return(self):
        """ Checks if the current statement always returns, and returns the return statement. """
        for case in switch(self.type.type):
            # TODO maybe consider call to error() a returning statement?
            if case(LP.RETURN):  # return -- obvious case.
                return self
            if case(LP.WHILE):
                if len(self.children) < 2:  # no loop block, nothing to check
                    return None
                # if the condition is a constant, warn if the block is unreachable or unexitable
                if self.children[0].type.type == LP.BOOLEAN:
                    if self.children[0].value == 'true':
                        block_ret = self.children[1].check_return()
                        if not block_ret:
                            Status.add_warning(TypecheckError(
                                'infinite loop without guaranteed return', self.pos))
                            return InfiniteLoopMark(self.pos)
                        return block_ret
                    else:  # while(false)
                        self.children[1].warn_unreachable_code(reason=TypecheckError(
                            'loop has false condition', self.pos))
                        return None
                # otherwise, condition is an expression
                return self.children[1].check_return()
            if case(LP.IF):
                # if the condition is a constant, check the executed branch, and warn that the
                # other is unreachable
                if self.children[0].type.type == LP.BOOLEAN:
                    try:
                        check = {'true': 1, 'false': 2}[self.children[0].value]
                        unreachable = {'true': 2, 'false': 1}[self.children[0].value]
                    except KeyError:
                        raise InternalError('invalid boolean constant `%s` at %s' %
                                            self.children[0].value, self.children[0].pos)
                    if len(self.children) > unreachable:
                        self.children[unreachable].warn_unreachable_code(reason=TypecheckError(
                            'constant condition prevents entering one branch', self.pos))
                    if len(self.children) > check:
                        return self.children[check].check_return()
                    return None
                else:  # otherwise, condition is an expression, check both branches
                    # both branches should return if we want to say this statement returns
                    then_ret = None if len(self.children) <= 1 else self.children[1].check_return()
                    else_ret = None if len(self.children) <= 2 else self.children[2].check_return()
                    return then_ret and else_ret
        # in all other cases
        return None

    def warn_unreachable_code(self, reason=None):
        """ Prints a warning that code in current node is unreachable (e.g. after if(false)).

        `reason` can be an additional exception explaining why. """
        # Prevent multiple issues of the warning when there are many statements after return.
        if not hasattr(self, 'unreachable_code_reported'):
            Status.add_warning(TypecheckError('statement is unreachable', self.pos))
            if reason:
                Status.add_note(reason)
            self.unreachable_code_reported = True

    def check_types(self):
        for case in switch(self.type.type):
            if case(LP.ASSIGN):  # Children: var, expr, check if types match.
                self.children[0].check_settable()
                self.children[1].expect_type(self.children[0].get_type())
                break
            if case(LP.INCR, LP.DECR):  # Child: var, expected type: int.
                self.children[0].check_settable()
                Symbol('', LP.INT).check_with(self.children[0].get_type(), self.pos)
                break
            if case(LP.RETURN):  # Check if returned value type matches the function declaration.
                fun = self.get_cur_fun()
                if not self.children:  # No value returned: check if function returns void.
                    fun.ret_type.check_with(Symbol('', LP.VOID), self.pos)
                else:  # Check the returned expression.
                    if fun.ret_type.type == LP.VOID:
                        Status.add_error(TypecheckError(
                            'return with a value in function returning `void`', self.pos))
                    self.children[0].expect_type(fun.ret_type)
                break
            if case(LP.IF, LP.WHILE):  # Children: cond, stmt+. Check if condition is boolean.
                self.children[0].expect_type(Symbol('', LP.BOOLEAN))
                # disallow declaration as the only statement
                for child in self.children:
                    if child.type.type == LP.DECL:
                        Status.add_error(TypecheckError(
                            'variable declaration not allowed here', child.pos))
                break
        self.check_children_types()
        # Warn about unused results -- cases where an non-void expression is used as a statement.
        # For if/while -- don't check the condition.
        for i in xrange(len(self.children)):
            ch = self.children[i]
            if (isinstance(ch, ExprTree) and ch.value_type and ch.value_type.type != LP.VOID and
                    (self.type.type == LP.BLOCK or
                     (self.type.type in [LP.IF, LP.WHILE] and i > 0)
                     )):
                Status.add_warning(TypecheckError('unused result of expression', ch.pos))
                ch.unused_result = True

    def is_null(self):
        """ Return true if the statement is a NULL value (happens only for declared, but unassigned
        objects -- since there is no explicit NULL in Latte). """
        return False


# code block ####################################################################################
class BlockTree(StmtTree):
    """ Node representing a block of instructions. """
    def __init__(self, **kwargs):
        super(BlockTree, self).__init__(LP.BLOCK, **kwargs)

    def add_stmt(self, tree):
        # A block has the position of its first statement.
        if not self.children:
            self.pos = tree.pos
        self.add_child(tree)

    def get_cur_block(self):
        """ Returns the closest (up the tree) block node. """
        return self

    def check_return(self):
        """ Checks if the current statement always returns, and returns the return statement. """
        for i in xrange(len(self.children)):
            ret_stmt = self.children[i].check_return()
            # if a non-last child is a returning statement, warn that further code is unreachable
            if ret_stmt:
                if i < len(self.children)-1:
                    if ret_stmt.type.type == InfiniteLoopMark.type:
                        err = TypecheckError('reach ended by infinite loop here', ret_stmt.pos)
                    else:
                        err = TypecheckError('reach ended by return statement here', ret_stmt.pos)
                    self.children[i+1].warn_unreachable_code(reason=err)
                return ret_stmt
        return None

# foreach loop ##################################################################################
class ForTree(BlockTree):
    """ Node representing a foreach loop -- deriving from Block to have its own scope.
    
    This node is used only for construction, and then transforms itself into an equivalent while
    loop, e.g. `for (int i : t)` stmt becomes: `{ int _c; while (_c < t.length) { stmt; _c++; } }`
    """
    def __init__(self, **kwargs):
        super(BlockTree, self).__init__(LP.FOR, **kwargs)  # super-super intentional
        # children will be: decl, expr (the array), stmt (loop body)

    def check_return(self):
        # Only the loop body needs to be checked for returns.
        if len(self.children) > 2:
            return self.children[2].check_return()
        return None

    def add_stmt(self, tree):
        super(ForTree, self).add_stmt(tree)

    def check_types(self):
        # Children after the morph: [array decl, loop_var decl, counter decl, while]
        # Expect that the child expr has type array(type of the decl child)
        dtype = self.children[1].decl_type.type.id
        self.array_expr.expect_type(Symbol('', DataType.mkarray(dtype)))
        self.array_expr.check_types()
        self.check_children_types()

    def morph_into_block(self):
        """ Convert the current node into a BlockTree with an equivalent while loop inside. """
        old_children = list(self.children)
        self.children = []
        self.array_expr = old_children[1]
        loop_var = old_children[0].items[0].name
        # [0] Evaluate the array expresion once.
        array_decl = DeclTree(DataType.mkarray(old_children[0].decl_type.type.id))
        array_decl.add_item(DeclArg(Builtins.FOR_ARRAY, self.pos, self.array_expr))
        self.add_stmt(array_decl)
        # [1] Add declarations for the loop counter and loop value.
        self.add_stmt(old_children[0])
        counter_decl = DeclTree(LP.INT)
        counter_decl.add_item(DeclArg(Builtins.FOR_COUNTER, self.pos))  # = 0 by default
        self.add_stmt(counter_decl)
        # [2a] condition: counter < array.length
        while_cond = BinopTree(LP.LT, VarTree(LP.IDENT, Builtins.FOR_COUNTER),
                              VarTree(LP.ATTR, Builtins.LENGTH, children=[
                                  VarTree(LP.IDENT, Builtins.FOR_ARRAY)]))
        # [2b] assign loop_var = array[counter]
        while_body = BlockTree()
        while_body.add_stmt(StmtTree(LP.ASSIGN, children=[
            VarTree(LP.IDENT, loop_var),
            VarTree(LP.ELEM, None, children=[VarTree(LP.IDENT, Builtins.FOR_ARRAY),
                                             VarTree(LP.IDENT, Builtins.FOR_COUNTER)])
        ]))
        # [2c] insert for loop body
        if len(old_children) > 2:
            while_body.add_stmt(old_children[2])
        # [2d] increment counter
        while_body.add_stmt(StmtTree(LP.INCR, children=[VarTree(LP.IDENT, Builtins.FOR_COUNTER)]))
        # [3] Add the loop to the block.
        self.add_stmt(StmtTree(LP.WHILE, children=[while_cond, while_body]))
        self.type = Symbol('', LP.BLOCK, self.pos)


# declaration ###################################################################################
class DeclTree(StmtTree):
    """ Node representing a declaration (possibly of many variables of the same type). """
    def __init__(self, dtype, **kwargs):
        super(DeclTree, self).__init__(LP.DECL, **kwargs)
        self.decl_type = Symbol('', dtype, Status.get_cur_pos())
        self.items = []

    def add_item(self, item):
        self.items.append(item)
        if not item.expr:
            for case in switch(self.decl_type.type):
                if case(LP.INT):
                    item.expr = LiteralTree(LP.INT, 0)
                    break
                if case(LP.BOOLEAN):
                    item.expr = LiteralTree(LP.BOOLEAN, 'false')
                    break
                if case(LP.STRING):
                    item.expr = LiteralTree(LP.STRING, '""')
                    break
                if case(LP.VOID):
                    return  # just to avoid errors
                if case(LP.ARRAY):
                    item.expr = LiteralTree(self.decl_type.type, None)
                    break
                if case():
                    raise InternalError('no default value for type %s' % str(self.decl_type))
        self.add_child(item.expr)

    def print_tree(self):
        self._print_indented('>DECL %s' % str(self.decl_type))
        for item in self.items:
            self._print_indented('* %s' % item.name)
            if item.expr:
                item.expr.print_tree()
        self._print_indented('<DECL %s' % str(self.decl_type))

    def check_types(self):
        """ Check types of the expressions assigned to the declared variables, if any. """
        if self.decl_type.type == LP.VOID:
            Status.add_error(TypecheckError('`void` variable declaration', self.pos))
            return
        block = self.get_cur_block()
        for item in self.items:
            dsym = Symbol(item.name, self.decl_type.type, item.pos)
            block.add_symbol(dsym)
            if item.expr:
                item.expr.expect_type(self.decl_type)
        self.check_children_types()


# expression ####################################################################################
class ExprTree(StmtTree):
    """ Node representing one expression.

    Typechecking invariants:
        * when check_types() returns value_type must be set
        * if value_type and expected_type are both set, they match xor an error was reported. """
    def __init__(self, type=None, **kwargs):
        super(ExprTree, self).__init__(type, **kwargs)
        self.value_type = None
        self.expected_type = None
        self.unused_result = None

    def expect_type(self, sym):
        self.expected_type = sym
        if self.value_type:
            self.expected_type.check_with(self.value_type, self.pos)

    def set_value_type(self, sym):
        # Note: don't just assign the symbol, not to overwrite its position.
        self.value_type = Symbol('', sym.type, self.pos)
        if self.expected_type:
            self.expected_type.check_with(self.value_type, self.pos)

    def settable(self):
        """ Check whether the variable can be assigned (e.g. tab.length can't). """
        for case in switch(self.type.type):
            if case(LP.ATTR):
                return not (self.value_type.type == LP.ARRAY and self.value == Builtins.LENGTH)
            if case(LP.IDENT, LP.ELEM):
                return True
        return False

    def check_settable(self):
        """ Check assignability and post an error if needed. """
        if not self.settable():
            Status.add_error(TypecheckError('expression cannot be assigned to', self.pos))

    def attributable(self):
        """ Check whether the variable can have attributes. """
        for case in switch(self.type.type):
            if case(LP.ATTR):
                return self.obj_type.type == LP.ARRAY
        return False


# literal #######################################################################################
class LiteralTree(ExprTree):
    """ Node representing a literal expression. """
    _int_max = 2147483647

    @classmethod
    def _get_value_typeid(cls, typeid):
        """ Translate lexer's literal types to actual data types. """
        try:
            return {
                LP.NUMBER: LP.INT,
                LP.STRINGLIT: LP.STRING,
                LP.TRUE: LP.BOOLEAN,
                LP.FALSE: LP.BOOLEAN,
            }[typeid]
        except KeyError:
            return typeid

    def __init__(self, typeid, value, **kwargs):
        """ Literal constructor. typeid can be a DataType (e.g. an array or object type) and value
        a reference (in particular, value=None means a (non-explicit) NULL). """
        real_typeid = self._get_value_typeid(typeid)
        super(LiteralTree, self).__init__(real_typeid, **kwargs)
        self.value = value
        debug('literal %s: pos %s' % (self.value, self.pos))

    def print_tree(self):
        self._print_indented('= %s %s' % (str(self.type), self.value))

    def get_type(self):
        # return immediately if the type is already calculated
        if self.value_type:
            return self.value_type
        # otherwise, set the type
        for case in switch(self.type.type):
            if case(LP.INT):
                if int(self.value) > self._int_max:
                    Status.add_error(TypecheckError(
                        'integer constant too large: `%s`' % self.value, self.pos))
                # intentional fall-through
            if case():
                self.set_value_type(self.type)
        return self.value_type

    def check_types(self):
        # Only save the type.
        self.get_type()

    def is_null(self):
        """ Return true if the statement is a NULL value (happens only for declared, but unassigned
        objects -- since there is no explicit NULL in Latte). """
        return self.type not in DataType.PLAIN_TYPES and self.value is None


# variables, fields #############################################################################
class VarTree(LiteralTree):
    def __init__(self, typeid, value, **kwargs):
        super(VarTree, self).__init__(typeid, value, **kwargs)
        # VAR: IDENT in value
        # ELEM: children=[obj, num]
        # ATTR: name in value, children=[obj]

    def print_tree(self):
        for case in switch(self.type.type):
            if case(LP.IDENT):
                self._print_indented('= IDENT %s' % self.value)
                break
            if case(LP.ATTR):
                self._print_indented('. ATTR %s' % (self.value))
                self._print_children()
                self._print_indented('; ATTR %s' % (self.value))
                break
            if case(LP.ELEM):
                self._print_indented('[ ELEM')
                self._print_children()
                self._print_indented('] ELEM')
                break

    def _check_symbol(self, name):
        if not self.has_symbol(name):
            Status.add_error(TypecheckError(
                'use of undeclared variable `%s`' % name, self.pos))
            Status.add_note(TypecheckError(
                'each undeclared identifier is reported only once in each function'))
            # add a dummy type-error symbol to prevent further errors about this variable
            self.get_cur_fun().add_symbol(Symbol(name, LP.TYPE_ERROR, self.pos))
        return self.symbol(name)

    def get_type(self):
        # return immediately if the type is already calculated
        if self.value_type:
            return self.value_type
        # otherwise, set the type as the variable was declared
        for case in switch(self.type.type):
            if case(LP.IDENT):
                self.set_value_type(self._check_symbol(self.value))
                break
            if case(LP.ATTR):
                self.children[0].check_types()
                self.obj_type = self.children[0].value_type
                if not self.attributable():
                    Status.add_error(TypecheckError(
                        'request for member `%s` in non-class expression of type `%s`' % (
                            self.value, str(self.obj_type)), self.pos))
                    break
                if self.obj_type.type == LP.ARRAY:
                    if self.value == Builtins.LENGTH:
                        self.set_value_type(Symbol('', LP.INT, self.pos))
                    else:
                        Status.add_error(TypecheckError(
                            'invalid attribute `%s` for type `%s`' % (
                                self.value, str(self.obj_type)), self.pos))
                elif self.obj_type.type != LP.TYPE_ERROR:
                    raise InternalError('ATTR for non-array type ' + str(self.obj_type))
                break
            if case(LP.ELEM):
                self.children[0].check_types()
                self.obj_type = self.children[0].value_type
                if self.obj_type.type == LP.ARRAY:
                    self.children[1].expect_type(Symbol('', LP.INT))
                    self.set_value_type(Symbol('', self.obj_type.type.subtype, self.pos))
                    self.children[1].check_types()
                elif self.obj_type.type != LP.TYPE_ERROR:
                    Status.add_error(TypecheckError(
                        '`operator[]` for non-array type `%s`' % str(self.obj_type), self.pos))
                break
            if case():
                raise InternalError('invalid variable type %s' % str(self.type.type))
        return self.value_type


# unary operator ################################################################################
class UnopTree(ExprTree):
    """ Node for unary expressions. """
    @classmethod
    def _get_typeid_for_op(cls, op):
        """ Return a type that can be used with the operator (only one for now). """
        try:
            return {LP.NOT: LP.BOOLEAN, LP.NEG: LP.INT, }[op.type.id]
        except KeyError:
            return None

    def __init__(self, type, expr, **kwargs):
        super(UnopTree, self).__init__(type, children=[expr], **kwargs)
        self.pos = Status.get_cur_pos(-2)

    def check_types(self):
        # Simple case, because the operator unambiguously defines the type.
        optype = self._get_typeid_for_op(self.type)
        self.set_value_type(Symbol('', optype, self.pos))
        self.children[0].expect_type(self.value_type)
        self.check_children_types()


# binary operator ###############################################################################
class BinopTree(ExprTree):
    """ Node for binary expressions. """

    # Possible operator lists for each type.
    _int_ops = [LP.MULT, LP.DIV, LP.MOD, LP.PLUS, LP.MINUS,
                LP.LT, LP.LEQ, LP.GT, LP.GEQ, LP.EQ, LP.NEQ]
    _boolean_ops = [LP.AND, LP.OR, LP.EQ, LP.NEQ]
    _string_ops = [LP.PLUS]
    _rel_ops = [LP.LT, LP.LEQ, LP.GT, LP.GEQ, LP.EQ, LP.NEQ]

    @classmethod
    def _get_possible_ops(cls, type):
        """ Return a list of operators that can be used with a given type. """
        try:
            return {
                LP.INT: cls._int_ops,
                LP.BOOLEAN: cls._boolean_ops,
                LP.STRING: cls._string_ops,
            }[type.type.id]
        except KeyError:
            return []

    def __init__(self, type, expa, expb, **kwargs):
        super(BinopTree, self).__init__(type, children=[expa, expb], **kwargs)

    def is_relational(self):
        return self.type.type in self._rel_ops

    def check_types(self):
        # children: expa, expb
        # [1] Compute the type of first subexpression.
        self.children[0].check_types()
        # [1] Set the type: boolean for comparision operators, otherwise the first expr's type.
        if self.is_relational():
            self.set_value_type(Symbol('', LP.BOOLEAN, self.pos))
        else:
            self.set_value_type(self.children[0].value_type)
        # [2] Check if the operator works for the selected type.
        ops = self._get_possible_ops(self.children[0].value_type)
        if self.type.type in ops:
            # [3] Mark that the second subexpression should have the same type.
            self.children[1].expect_type(self.children[0].value_type)
        else:
            Status.add_error(TypecheckError(
                'operator cannot accept `%s` argument' % str(self.children[0].value_type),
                self.pos))
        # [4] Check the second subexpression.
        self.children[1].check_types()


# function call #################################################################################
class FuncallTree(ExprTree):
    """ Node for function calls. """
    def __init__(self, fname, **kwargs):
        super(FuncallTree, self).__init__(LP.FUNCALL, **kwargs)
        self.fname = fname

    def print_tree(self):
        self._print_indented('>FUNCALL %s' % self.fname)
        self._print_children()
        self._print_indented('<FUNCALL %s' % self.fname)

    def check_types(self):
        # [1] Check if the called name exists and is a function.
        if not self.has_symbol(self.fname):
            Status.add_error(TypecheckError(
                'call to undeclared function `%s`' % self.fname, self.pos))
            return
        fsym = self.symbol(self.fname)
        if not fsym.is_function():
            Status.add_error(TypecheckError(
                'cannot call symbol `%s` of type `%s`' % (self.fname, str(fsym)), self.pos))
            return
        fsym.call_counter += 1
        # [2] Check the number of arguments.
        self.set_value_type(fsym.ret_type)
        if len(self.children) != len(fsym.args):
            Status.add_error(TypecheckError(
                '%d arguments given, function `%s` takes %d' %
                (len(self.children), self.fname, len(fsym.args)), self.pos))
            if fsym.pos:  # Without position it's probably a builtin and the note wouldn't help.
                Status.add_note(TypecheckError('as declared here', fsym.pos))
        # [3] Check the types of arguments.
        for i in xrange(min(len(self.children), len(fsym.args))):
            self.children[i].expect_type(fsym.args[i])
        self.check_children_types()


# new object construction #######################################################################
class NewTree(ExprTree):
    """ Node for calls to `new` operator. """
    def __init__(self, value_type, size=None, **kwargs):
        # Size should be set only for arrays.
        if value_type != LP.ARRAY and size is not None:
            raise InternalError('size set for new operator with non-array type ' + str(value_type))
        super(NewTree, self).__init__(LP.NEW, **kwargs)
        sym = Symbol('', value_type, self.pos)
        self.set_value_type(sym)
        if size:
            self.size = size

    def print_tree(self):
        size_str = '' if not self.size else '[%d]' % self.size
        self._print_indented('= new %s%s' % (str(self.value_type), size_str))


# helper class to mark code after an infinite loop unreachable
class InfiniteLoopMark(StmtTree):
    type = 10000

    def __init__(self, pos):
        super(InfiniteLoopMark, self).__init__(type=InfiniteLoopMark.type, pos=pos)
