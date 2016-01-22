#!/usr/bin/python2
# -*- coding: utf8 -*-
""" Tree node classes that act as wrappers for LatteNodes (just to be able to separate these two
already large files). Their methods generate the intermediate language code, first each for itself,
then they are merged into one big list. Also, stack space requirements and variable locations are
computed while generating the codes (at least for now, it should be moved to a later stage). """

import abc
import LatteParser as LP
from LatteCodes import Codes as CC, Loc
from LatteParser import Builtins
from FuturePrint import debug
from LatteUtils import Symbol
from LatteErrors import InternalError
from LatteNodes import ExprTree, BinopTree
from Utils import switch, Flags


# code ABC ######################################################################################
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

    def add_instr(self, type, **kwargs):
        """ Appends a code. """
        self.instr.append(CC.mkcode(type, **kwargs))

    def add_asm_instr(self, parts, **kwargs):
        """ Appends a ASM type instruction (special assembly directives). """
        self.add_instr(CC.ASM, parts=parts, **kwargs)

    @abc.abstractmethod
    def gen_code(self, **kwargs):
        """ Recursively generates all subtree's code. """
        pass

    def add_child_code(self, child, **kwargs):
        """ Generates code for the given child and inserts it in the node's code. """
        child.gen_code(**kwargs)
        self.add_instr(CC.CHILD, child=child)

    def add_child_by_idx(self, idx, **kwargs):
        self.add_child_code(self.children[idx], **kwargs)

    def codes(self):
        """ A generator that yields the intermediate codes. """
        for instr in self.instr:
            if CC.is_child(instr):
                for child_instr in instr['child'].codes():
                    yield child_instr
            else:
                yield instr

    def get_cur_block(self):
        return self.parent.get_cur_block() if self.parent else None

    def get_cur_fun(self):
        return self.parent.get_cur_fun() if self.parent else None


# program #######################################################################################
class ProgCode(LatteCode):
    def __init__(self, tree, **kwargs):
        super(ProgCode, self).__init__(tree, **kwargs)
        for funtree in tree.children:
            if tree.symbol(funtree.name).call_counter > 0 or funtree.name == LP.Builtins.MAIN:
                self.add_fun_code(funtree)
            else:
                debug('skipping uncalled function `%s`' % funtree.name)

    def add_fun_code(self, funtree):
        self.add_child(FunCode(funtree))

    def gen_code(self, **kwargs):
        # source file info
        if not Flags.input_from_stdin():
            self.add_asm_instr(['.file', '"%s"' % Flags.input_file])
        self.add_instr(CC.EMPTY)
        # program code
        self.add_asm_instr(['.text'])
        for child in self.children:
            self.add_instr(CC.EMPTY)
            self.add_child_code(child)
        # string constants (after children, so their labels are allocated)
        self.add_instr(CC.EMPTY)
        self.add_asm_instr(['.section', '.rodata'], comment='string constants defined below')
        for string, label in CC._strings.iteritems():
            self.add_instr(CC.LABEL, label=label, append='.string ' + string)


# function ######################################################################################
class FunCode(LatteCode):
    def __init__(self, tree, **kwargs):
        super(FunCode, self).__init__(tree, **kwargs)
        self.ret_type = tree.ret_type
        self.name = tree.name
        self.args = tree.args
        self.add_child(BlockCode(tree.children[0]))
        self.ret_label = CC.new_label()
        for i in xrange(len(self.args)):
            arg = self.args[i]
            self.tree.add_symbol(Symbol(arg.name, arg.type, Loc.arg_addr(i)))
        self.var_count = self.children[0].count_local_vars()
        debug('fun ', self.name, 'VAR COUNT: ', self.var_count)
        self.used_vars = 0  # how many local variables are currently allocated

    def get_cur_fun(self):
        return self

    def next_var_num(self):
        """ Returns the index of next free local variable on stack. """
        self.used_vars += 1
        return self.used_vars - 1

    def gen_code(self, **kwargs):
        self.add_instr(CC.FUNC, label=self.name, tree=self)
        self.add_child_by_idx(0)
        self.add_instr(CC.LABEL, label=self.ret_label)
        self.add_instr(CC.ENDFUNC, label=self.name, tree=self)


# statement #####################################################################################
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
                self.label_after = CC.new_label()
                # if there are less blocks, just evaluate condition and jump to label_after
                self.label_then = CC.new_label() if len(self.children) > 1 else self.label_after
                self.label_else = CC.new_label() if len(self.children) > 2 else self.label_after
                break
            if case(LP.WHILE):
                self.label_after = CC.new_label()
                self.label_cond = CC.new_label()
                self.label_block = CC.new_label() if len(self.children) > 1 else self.label_cond
                break

    def gen_code(self, **kwargs):
        for case in switch(self.type.type):
            if case(LP.RETURN):
                if self.children:
                    # Evaluate the expression and pop the result to eax for returning.
                    self.add_child_by_idx(0)
                    self.add_instr(CC.POP, dest=Loc.reg('a'))
                # Jump to the return section
                self.add_instr(CC.JUMP, label=self.get_cur_fun().ret_label)
                break
            if case(LP.IF):
                # children: cond, (then-block)?, (else-block)?
                self.add_child_by_idx(0, on_true=self.label_then, on_false=self.label_else)
                if len(self.children) > 1:  # there is a then-block
                    self.add_instr(CC.LABEL, label=self.label_then)
                    self.add_child_by_idx(1)
                if len(self.children) > 2:  # there is an else-block
                    self.add_instr(CC.JUMP, label=self.label_after)  # jump out of then-block
                    self.add_instr(CC.LABEL, label=self.label_else)
                    self.add_child_by_idx(2)
                self.add_instr(CC.LABEL, label=self.label_after)
                break
            if case(LP.WHILE):
                # children: cond, (block)?
                if len(self.children) > 1:  # there is a loop block
                    self.add_instr(CC.JUMP, label=self.label_cond)
                    self.add_instr(CC.LABEL, label=self.label_block)
                    self.add_child_by_idx(1)
                self.add_instr(CC.LABEL, label=self.label_cond)
                self.add_child_by_idx(0, on_true=self.label_block, on_false=self.label_after)
                self.add_instr(CC.LABEL, label=self.label_after)
                break
            if case(LP.ASSIGN):
                # compute assigned value on stack
                self.add_child_by_idx(1)
                self.add_instr(CC.POP, dest=Loc.reg('a'))
                # put the value into destination address
                dest_sym = self.tree.symbol(self.children[0].value)
                self.add_instr(CC.MOV, src=Loc.reg('a'), dest=Loc.sym(dest_sym))
                break
            if case(LP.INCR, LP.DECR):
                op = CC.ADD if self.type.type == LP.INCR else CC.SUB
                sym = self.tree.symbol(self.children[0].value)
                self.add_instr(op, lhs=Loc.const(1), rhs=Loc.sym(sym))
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


# code block ####################################################################################
class BlockCode(StmtCode):
    def __init__(self, tree, **kwargs):
        super(BlockCode, self).__init__(tree, no_children=True, **kwargs)
        for stmttree in tree.children:
            self.add_child(StmtFactory(stmttree))

    def get_cur_block(self):
        return self

    def gen_code(self, **kwargs):
        self.add_instr(CC.SCOPE, tree=self)
        for child in self.children:
            self.add_child_code(child)
        fun = self.get_cur_fun()
        fun.used_vars -= self.var_count  # free local variables as the context is closed
        self.add_instr(CC.ENDSCOPE, tree=self)


# declaration ###################################################################################
class DeclCode(StmtCode):
    def __init__(self, tree, **kwargs):
        super(DeclCode, self).__init__(tree, no_children=True, **kwargs)
        self.decl_type = tree.decl_type
        self.items = []
        for item in tree.items:
            self.add_item(item)

    def add_item(self, item):
        self.items.append(item)
        self.items[-1].expr_child_idx = len(self.children)
        if item.expr:
            self.add_child(ExprFactory(item.expr))

    def count_local_vars(self):
        return len(self.items)

    def gen_code(self, **kwargs):
        fun = self.get_cur_fun()
        block = self.get_cur_block()
        # For each declared item, compute its address on stack (and assign the value if needed).
        for item in self.items:
            addr = Loc.var_addr(fun.next_var_num())
            sym = Symbol(item.name, self.decl_type.type, addr)
            if item.expr and not item.expr.is_null():
                self.add_child_by_idx(item.expr_child_idx)
                self.add_instr(CC.POP, dest=Loc.reg('a'))
                self.add_instr(CC.MOV, src=Loc.reg('a'), dest=Loc.sym(sym))
            # Important: we add symbol containing the new var's address *after* assigned expression
            # is evaluated (because of e.g. int i = i+7); but still inside the loop -- so next
            # declarations use the new symbol.
            block.tree.add_symbol(sym)


# expression ####################################################################################
class ExprCode(StmtCode):
    # niezmiennik: po wyliczeniu wyrażenia jego wynik (i tylko on) zostaje na stosie
    def __init__(self, tree, **kwargs):
        super(ExprCode, self).__init__(tree, no_children=True, **kwargs)
        self.value_type = tree.value_type

    def is_constant(self):
        # TODO use this in optimization
        return False

    def check_unused_result(self):
        """ Drop result pushed on stack which wouldn't be used. Both this add and the push itself
        will be optimized out later. """
        if self.tree.unused_result:
            debug('POP UNUSED RESULT', self.tree.pos)
            self.add_instr(CC.ADD, lhs=Loc.const(CC.var_size), rhs=Loc.reg('top'),
                           comment=CC.S_UNUSED_RESULT)

    @staticmethod
    def has_jump_codes(d):
        """ Check if a dictionary contains jump codes for lazy boolean evaluation """
        return 'on_true' in d and 'on_false' in d


# literal #######################################################################################
class LiteralCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(LiteralCode, self).__init__(tree, **kwargs)
        self.value = tree.value
        if self.type.type == LP.ATTR:
            self.obj = tree.obj
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
                    self.add_instr(CC.JUMP, label=label)
                    break
                if case(LP.IDENT) and self.tree.symbol(self.value).type == LP.BOOLEAN:
                    self.add_instr(CC.MOV, src=Loc.sym(self.tree.symbol(self.value)),
                                   dest=Loc.reg('a'))
                    # note: comparing with 0, so on equality jump to false!
                    self.add_instr(CC.IF_JUMP, lhs=Loc.const(0), rhs=Loc.reg('a'),
                                   op='je', label=kwargs['on_false'])
                    self.add_instr(CC.JUMP, label=kwargs['on_true'])
                    break
                if case():
                    raise InternalError('jump-expr codes for non-bool %s literal at %s!' % (
                        str(self.type), self.tree.pos))
            return
        for case in switch(self.type.type):
            if case(LP.BOOLEAN, LP.INT):
                self.add_instr(CC.PUSH, src=Loc.const(self.value))
                break
            if case(LP.IDENT):
                self.add_instr(CC.MOV, src=Loc.sym(self.tree.symbol(self.value)),
                               dest=Loc.reg('a'))
                self.add_instr(CC.PUSH, src=Loc.reg('a'))
                break
            if case(LP.STRING):
                self.add_instr(CC.PUSH, src=Loc.stringlit(self))
                break
            if case(LP.ATTR):
                sym = self.tree.symbol(self.obj)
                if sym.type == LP.ARRAY and self.value == Builtins.LENGTH:
                    # Array length is stored in first element of its memory block.
                    self.add_instr(CC.MOV, src=Loc.sym(sym), dest=Loc.reg('a'))
                    self.add_instr(CC.MOV, src=Loc.mem(Loc.reg_a), dest=Loc.reg('a'))
                    self.add_instr(CC.PUSH, src=Loc.reg('a'))
                else:
                    raise InternalError('invalid attr %s for type %s' % (self.value, str(obj.type)))
                break
        self.check_unused_result()

    def is_constant(self):
        return self.type.type != LP.IDENT


# unary operator ################################################################################
class UnopCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(UnopCode, self).__init__(tree, **kwargs)
        self.add_child(ExprFactory(tree.children[0]))

    def gen_code(self, **kwargs):
        for case in switch(self.type.type):
            if case(LP.NEG):  # integer negation
                self.add_child_by_idx(0)
                self.add_instr(CC.POP, dest=Loc.reg('a'))
                self.add_instr(CC.NEG, rhs=Loc.reg('a'))
                self.add_instr(CC.PUSH, src=Loc.reg('a'))
                break
            if case(LP.NOT):  # logical not
                if self.has_jump_codes(kwargs):
                    # called as part of condition evaluation -- just forward the jump labels
                    self.add_child_by_idx(0, on_true=kwargs['on_false'],
                                          on_false=kwargs['on_true'])
                    return
                # otherwise -- evaluate the boolean value: arg == false (0)
                self.add_child_by_idx(0)
                self.add_instr(CC.POP, dest=Loc.reg('a'))
                self.add_instr(CC.BOOL_OP, lhs=Loc.const(0), rhs=Loc.reg('a'),
                               op='sete', dest=Loc.reg('a'))
                self.add_instr(CC.PUSH, src=Loc.reg('a'))
                break
            if case():
                raise InternalError('wrong unop value type')
        self.check_unused_result()


# binary operator ###############################################################################
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
                    self._gen_code_relop(**kwargs)  # comparision
                else:
                    self._gen_code_boolop(**kwargs)  # logical operation
                break
            if case(LP.STRING):
                self._gen_code_stringop()
                break
            if case():
                raise InternalError('wrong binop value type %s')
        self.check_unused_result()

    def _gen_code_intop(self):
        self.add_child_by_idx(0)
        self.add_child_by_idx(1)
        for case in switch(self.type.type):
            if case(LP.PLUS, LP.MINUS, LP.MULT):
                op = {LP.PLUS: CC.ADD, LP.MINUS: CC.SUB, LP.MULT: CC.MUL}[self.type.type.id]
                self.add_instr(CC.POP, dest=Loc.reg('d'))
                self.add_instr(CC.POP, dest=Loc.reg('a'))
                self.add_instr(op, lhs=Loc.reg('d'), rhs=Loc.reg('a'))
                self.add_instr(CC.PUSH, src=Loc.reg('a'))
                break
            if case(LP.DIV, LP.MOD):
                self.add_instr(CC.POP, dest=Loc.reg('c'))
                self.add_instr(CC.POP, dest=Loc.reg('a'))
                # quotient in eax, remainder in edx
                result = {LP.DIV: Loc.reg('a'), LP.MOD: Loc.reg('d')}[self.type.type.id]
                code = {LP.DIV: CC.DIV, LP.MOD: CC.MOD}[self.type.type.id]
                self.add_instr(code, lhs=Loc.reg('c'), rhs=Loc.reg('a'), dest=result)
                self.add_instr(CC.PUSH, src=result)
                break
            if case():
                raise InternalError('wrong int op type %s' % str(self.type))

    def _gen_code_relop(self, **kwargs):
        self.add_child_by_idx(0)
        self.add_child_by_idx(1)
        self.add_instr(CC.POP, dest=Loc.reg('d'))
        self.add_instr(CC.POP, dest=Loc.reg('a'))
        try:
            if self.has_jump_codes(kwargs):
                # part of condition evaluation -- select the conditional jump instruction
                self.label_true = kwargs['on_true']
                self.label_false = kwargs['on_false']
                jmp_code = {LP.EQ: 'je', LP.NEQ: 'jne',
                            LP.GT: 'jg', LP.GEQ: 'jge',
                            LP.LT: 'jl', LP.LEQ: 'jle', }[self.type.type.id]
                self.add_instr(CC.IF_JUMP, lhs=Loc.reg('d'), rhs=Loc.reg('a'),
                               op=jmp_code, label=self.label_true)
                self.add_instr(CC.JUMP, label=self.label_false)
            else:
                # expression returning bool -- select the comparision set instruction
                set_code = {LP.EQ: 'sete', LP.NEQ: 'setne',
                            LP.GT: 'setg', LP.GEQ: 'setge',
                            LP.LT: 'setl', LP.LEQ: 'setle', }[self.type.type.id]
                self.add_instr(CC.BOOL_OP, lhs=Loc.reg('d'), rhs=Loc.reg('a'),
                               op=set_code, dest=Loc.reg('a'))
                self.add_instr(CC.PUSH, src=Loc.reg('a'))
        except KeyError:
            raise InternalError('wrong rel op type %s' % str(self.type))

    def _gen_code_boolop(self, **kwargs):
        self.label_true = kwargs.get('on_true', CC.new_label())
        self.label_false = kwargs.get('on_false', CC.new_label())
        self.label_right = CC.new_label()  # additional label to jump to the right operand
        for case in switch(self.type.type):
            if case(LP.AND):
                self.add_child_by_idx(0, on_true=self.label_right, on_false=self.label_false)
                self.add_instr(CC.LABEL, label=self.label_right)
                self.add_child_by_idx(1, on_true=self.label_true, on_false=self.label_false)
                break
            if case(LP.OR):
                self.add_child_by_idx(0, on_true=self.label_true, on_false=self.label_right)
                self.add_instr(CC.LABEL, label=self.label_right)
                self.add_child_by_idx(1, on_true=self.label_true, on_false=self.label_false)
                break
            if case():
                raise InternalError('wrong bool op type %s' % str(self.type))
        # if no jump keywords were given, the result will be used as a value -- push it
        if not self.has_jump_codes(kwargs):
            self.label_after = CC.new_label()
            self.add_instr(CC.LABEL, label=self.label_true)
            self.add_instr(CC.PUSH, src=Loc.const(1))
            self.add_instr(CC.JUMP, label=self.label_after)
            self.add_instr(CC.LABEL, label=self.label_false)
            self.add_instr(CC.PUSH, src=Loc.const(0))
            self.add_instr(CC.LABEL, label=self.label_after)

    def _gen_code_stringop(self):
        if self.type.type != LP.PLUS:
            raise InternalError('wrong string op type %s' % str(self.type))
        # only + (concatenation) for now
        # If at least one operand is a constant or a variable, we can safely just push them in
        # reversed order (for call to 'concatString' library function). Otherwise we need to
        # evaluate them in the right order and then reverse them on stack.
        if isinstance(self.children[0], LiteralCode) or isinstance(self.children[1], LiteralCode):
            self.add_child_by_idx(1)
            self.add_child_by_idx(0)
        else:
            self.add_child_by_idx(0)
            self.add_child_by_idx(1)
            self.add_instr(CC.POP, dest=Loc.reg('a'))
            self.add_instr(CC.POP, dest=Loc.reg('d'))
            self.add_instr(CC.PUSH, src=Loc.reg('a'))
            self.add_instr(CC.PUSH, src=Loc.reg('d'))
        self.add_instr(CC.CALL, label=CC.STRCAT_FUNCTION)
        self.add_instr(CC.ADD, lhs=Loc.const(2 * CC.var_size), rhs=Loc.reg('top'))
        self.add_instr(CC.PUSH, src=Loc.reg('a'))
        # TODO free memory later


# function call #################################################################################
class FuncallCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(FuncallCode, self).__init__(tree, **kwargs)
        self.fname = tree.fname
        self.fsym = tree.symbol(tree.fname)
        for exprtree in tree.children:
            self.add_child(ExprFactory(exprtree))

    def gen_code(self, **kwargs):
        # [1] Compute memory usage for arguments.
        argmem = CC.var_size * len(self.children)
        # [2] Push arguments.
        # Arguments need to be pushed in reverse order, but evaluated in normal order -- hence
        # we first make enough stack space for all of them and move them in the right place after
        # evaluation.
        if len(self.children) > 1:
            self.add_instr(CC.SUB, lhs=Loc.const(argmem), rhs=Loc.reg('top'))
            for i in xrange(len(self.children)):
                self.add_child_by_idx(i)  # Leaves the value on stack.
                self.add_instr(CC.POP, dest=Loc.reg('a'))
                # i-th argument should be placed in 4*i(%esp)
                self.add_instr(CC.MOV, src=Loc.reg('a'),
                               dest=Loc.mem(Loc.top, i * CC.var_size))
        elif len(self.children) > 0:
            self.add_child_by_idx(0)  # With only one argument we can just push it on stack.
        # [3] Call and pop arguments.
        self.add_instr(CC.CALL, label=self.fname)
        if argmem > 0:
            self.add_instr(CC.ADD, lhs=Loc.const(argmem), rhs=Loc.reg('top'))
        # [4] finish depending on how we were called:
        if self.has_jump_codes(kwargs):
            if self.fsym.ret_type.type != LP.BOOLEAN:
                raise InternalError('jump-expr codes for non-bool function %s %s at %s!' % (
                    self.fname, str(self.fsym), self.tree.pos))
            # [4a] bool function as part of condition evaluation -- jump basing on the result
            # note: comparing with 0, so on equality jump to false!
            self.add_instr(CC.IF_JUMP, lhs=Loc.const(0), rhs=Loc.reg('a'),
                           op='je', label=kwargs['on_false'])
            self.add_instr(CC.JUMP, label=kwargs['on_true'])
        else:
            # [4b] normal expression -- push the return value on stack if needed
            if self.fsym.ret_type.type != LP.VOID:
                self.add_instr(CC.PUSH, src=Loc.reg('a'))
            self.check_unused_result()


# new object construction #######################################################################
class NewCode(ExprCode):
    def __init__(self, tree, **kwargs):
        super(NewCode, self).__init__(tree, **kwargs)
        self.value_type = tree.value_type
        if self.value_type.type == LP.ARRAY:
            self.size = tree.size

    def gen_code(self, **kwargs):
        for case in switch(self.value_type.type):
            if case(LP.ARRAY):
                # Convention: array of size N is a block of memory for (N+1) variables, and the
                # first variable will contain array's size ( = N)
                mem_size = (self.size + 1) * CC.var_size
                self.add_instr(CC.PUSH, src=Loc.const(mem_size))
                self.add_instr(CC.CALL, label=CC.MALLOC_FUNCTION)
                # Write the array size into the first index.
                self.add_instr(CC.MOV, src=Loc.const(self.size), dest=Loc.mem(Loc.reg_a))
                # Push the memory pointer as expression result.
                self.add_instr(CC.PUSH, src=Loc.reg('a'))
                break
            if case():
                raise InternalError('invalid type for new operator: ' + str(self.value_type))


# factories #####################################################################################
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
        if case(LP.INT, LP.STRING, LP.BOOLEAN, LP.IDENT, LP.ARRAY, LP.ATTR):
            return LiteralCode
        if case(LP.NOT, LP.NEG):
            return UnopCode
        if case(LP.MULT, LP.DIV, LP.MOD, LP.PLUS, LP.MINUS, LP.LT, LP.LEQ, LP.GT, LP.GEQ,
                LP.EQ, LP.NEQ, LP.AND, LP.OR):
            return BinopCode
        if case(LP.FUNCALL):
            return FuncallCode
        if case(LP.NEW):
            return NewCode
    raise InternalError('wrong expr code for construction: ' + str(tree.type))


def ExprFactory(tree, **kwargs):
    cstr = _expr_constructor(tree, **kwargs)
    return cstr(tree, **kwargs)
