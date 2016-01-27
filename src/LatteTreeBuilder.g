tree grammar LatteTreeBuilder;
/*------------------------------------------------------------------
 * Artur Kozak [320770], MRJP 2014 zadanie 2
 * ANTLR3 tree grammar transforming AST generated by LatteParser
 * into a tree of LatteTree nodes for further processing.
 *------------------------------------------------------------------*/

options {
    language = Python;
    tokenVocab = Latte;
    ASTLabelType = CommonTree;
}

// Header pasted on the top of parser file.
@header {
from FuturePrint import debug
from LatteLexer import LatteLexer
from LatteParser import LatteParser
import LatteParser as LP
from LatteErrors import Status, ParserError
from LatteNodes import *
from LatteUtils import FunArg, DeclArg, DataType
}

@members {
def displayRecognitionError(self, tokenNames, e):
    """ Saves the error into the error set. """
    msg = self.getErrorMessage(e, tokenNames)
    Status.add_error(ParserError(msg, e.line, e.charPositionInLine))
}

@main {
def main(argv):
    """ Main function in case the parser is executed directly.
    
    Only for testing purposes. Reads code from stdin, writes diagnostic messages
    and textual representation of the tree, function and symbol names. """
    lexer = LatteLexer(ANTLRInputStream(sys.stdin))
    tokens = CommonTokenStream(lexer)
    Status.set_token_stream(tokens)
    parser = LatteParser(tokens)
    prog = parser.prog()
    debug("-----------------------------------------------");
    debug("Tree: ", prog.tree.toStringTree())
    debug("-----------------------------------------------");
    nodes = CommonTreeNodeStream(prog.tree)
    nodes.setTokenStream(tokens)
    Status.set_node_stream(nodes)
    walker = LatteTreeBuilder(nodes)
    root = walker.prog()
    debug("functions: \%d" \% len(root.children))
    debug("symbols: \%d" \% len(root.symbols))
}

/*------------------------------------------------------------------
 * From each node of the AST generated by LatteParser, a node subclassing
 * LatteNode is created, and its necessary attributes are set.
 * In particular, in some cases the `pos` argument needs to be adjusted
 * to correctly indicate the related token's position in the source code
 * -- in order to properly show possible error messages later.
 *------------------------------------------------------------------*/

// program -------------------------------------------------
prog returns [lt=ProgTree()]
    : ^(PROG 
            ( fundef { $lt.add_fun_tree($fundef.lt); } 
            | classdef { $lt.add_class_tree($classdef.lt); }
            )*
       )
    ;

fundef returns [lt=FunTree()]
    : ^(FUNDEF
            declType { $lt.set_ret_type($declType.dt); }
            IDENT { $lt.set_name(str($IDENT.text)); }
            (arg { $lt.add_arg($arg.fa); })*
            block { $lt.set_block($block.lt); }
       )
    ;

arg returns [fa]
    : ^(ARG declType IDENT) { $fa = FunArg($declType.dt, str($IDENT.text)); }
    ;

declType returns [dt]
    : ^(ARRAY t=declType) { $dt = DataType.mkarray($t.dt); }
    | plainType { $dt = $plainType.dt; }
    ;
plainType returns [dt]
    : t=(INT | STRING | BOOLEAN | VOID) { $dt = DataType($t.type); }
    | ^(OBJECT IDENT) { $dt = DataType.mkobject(str($IDENT.text)); }
    ;

block returns [lt=BlockTree()]
    : ^(BLOCK (stmt { $lt.add_stmt($stmt.lt); })* )
    ;

classdef returns [lt]
    : ^(CLASS
            IDENT { $lt = ClassTree(str($IDENT.text)); }
            ( decl { $lt.add_member_decl($decl.lt); }
            | fundef { $lt.add_method($fundef.lt); }
            )*
       )
    ;
        

// statements ----------------------------------------------
stmt returns [lt]
    : block
        { $lt = $block.lt; }
    | decl
        { $lt = $decl.lt; }
    | ^(ASSIGN { $lt = StmtTree(ASSIGN); } 
            var { $lt.add_child($var.lt); }
            expr { $lt.add_child($expr.lt); }
       )
    | ^(op=(INCR|DECR) var)
        { $lt = StmtTree($op.type, children=[$var.lt], pos_off=-2); }
    | ^(RETURN { $lt = StmtTree(RETURN); }
            (expr { $lt.add_child($expr.lt); })?
       )
    | ^(IF { if_pos = Status.get_cur_pos() } expr
        { $lt = StmtTree(IF, children=[$expr.lt], pos=if_pos); }
            (s=stmt { $lt.add_child($s.lt); })?
            (ifelse { $lt.add_child($ifelse.lt); })?
       )
    | ^(WHILE expr
            { $lt = StmtTree(WHILE, children=[$expr.lt]); }
            (s=stmt { $lt.add_child($s.lt); })?
       )
    | ^(FOR { $lt = ForTree(); }
            declType { $lt.add_stmt(DeclTree($declType.dt)); }
            ditem { $lt.children[0].add_item($ditem.item); }
            expr { $lt.add_stmt($expr.lt); }
            (s=stmt { $lt.add_stmt($s.lt); })?
            { $lt.morph_into_block(); }
       )
    | expr
        { $lt = $expr.lt; }
    ;

decl returns [lt]
    : ^(DECL declType { $lt = DeclTree($declType.dt); }
            (ditem { $lt.add_item($ditem.item); })+
       )
    ;

ditem returns [item]
    : ^(DITEM IDENT { id_pos = Status.get_cur_pos() } )
        { $item = DeclArg(str($IDENT.text), id_pos); }
    | ^(ASSIGN IDENT { id_pos = Status.get_cur_pos() } expr)
        { $item = DeclArg(str($IDENT.text), id_pos, $expr.lt); }
    ;

ifelse returns [lt=StmtTree()]
    : ^(ELSE { e_pos = Status.get_cur_pos() } stmt { $lt = $stmt.lt; })
        { if $lt.pos == '0:0': $lt.pos = e_pos }
    ;

// expressions ---------------------------------------------
var returns [lt]
    : ^(ATTR attr=IDENT
        { $lt = VarTree(ATTR, str($attr.text)); }
            obj=expr { $lt.add_child($obj.lt); }
       )
    | ^(ELEM num=expr
        { $lt = VarTree(ELEM, None, children=[$num.lt]); }
            obj=expr { $lt.add_child_front($obj.lt); }
       )
    | IDENT
        { $lt = VarTree(IDENT, str($IDENT.text)); }
    ;

expr returns [lt]
    : var
        { $lt = $var.lt; }
    | lit=(NUMBER|STRINGLIT|TRUE|FALSE)
        { $lt = LiteralTree($lit.type, str($lit.text)); }
    | ^(NULL declType)
        { $lt = LiteralTree($declType.dt, NULL); }
    | ^(op=(NOT|NEG) e=expr)
        { $lt = UnopTree($op.type, $e.lt); }
    | ^(op=(MULT|DIV|MOD|PLUS|MINUS | LT|LEQ|GT|GEQ|EQ|NEQ | AND|OR) { op_pos = Status.get_cur_pos() }
           a=expr b=expr
       )
        { $lt = BinopTree($op.type, $a.lt, $b.lt, pos=op_pos); }
    | ^(FUNCALL { $lt = FuncallTree(); }
            (e=expr { $lt.add_child($e.lt); } )+
            { $lt.end_of_arguments(); }
       )
    | ^(NEW declType
        { $lt = NewTree($declType.dt, pos_off=-2); }
            (size=expr { $lt.set_array_size($size.lt); })?
       )
    ;
