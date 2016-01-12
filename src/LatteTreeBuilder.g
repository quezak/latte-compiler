tree grammar LatteTreeBuilder;
/*------------------------------------------------------------------
 * Artur Kozak [320770], MRJP 2014 zadanie 2
 * Gramatyka ANTLR3 zamieniajaca AST wygenerowane przez parser z Latte.g
 * na drzewo obiektow LatteTree do dalszego przetwarzania.
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
from LatteErrors import Status
from LatteNodes import *
from LatteUtils import FunArg, DeclArg
}

@members {
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
 * reguly parsera AST
 *------------------------------------------------------------------
 * From each node of the AST generated by LatteParser, a node subclassing
 * LatteNode is created, and its necessary attributes are set.
 * In particular, in some cases the `pos` argument needs to be adjusted
 * to correctly indicate the related token's position in the source code
 * -- in order to properly show possible error messages later.
 *------------------------------------------------------------------*/

// program -------------------------------------------------
prog returns [lt=ProgTree()]
    : ^(PROG (fundef { $lt.add_fun_tree($fundef.lt); } )* )
    ;

fundef returns [lt=FunTree()]
    : ^(FUNDEF
            type { $lt.set_ret_type($type.id); }
            IDENT { $lt.set_name($IDENT.text); }
            (arg { $lt.add_arg($arg.fa); })*
            block { $lt.set_block($block.lt); }
       )
    ;

arg returns [fa]
    : ^(ARG type IDENT) { $fa = FunArg($type.id, $IDENT.text); }
    ;

type returns [id]
    : t=(INT | STRING | BOOLEAN | VOID) { $id = $t.type; }
    ;

block returns [lt=BlockTree()]
    : ^(BLOCK (stmt { $lt.add_stmt($stmt.lt); })* )
    ;

// statements ----------------------------------------------
stmt returns [lt=StmtTree()]
    : block
        { $lt = $block.lt; }
    | ^(DECL type { $lt = DeclTree($type.id); }
            (ditem { $lt.add_item($ditem.item); })+
       )
    | ^(ASSIGN { $lt = StmtTree(ASSIGN); } 
            IDENT { $lt.add_child(LiteralTree(IDENT, $IDENT.text)); }
            expr { $lt.add_child($expr.lt); }
       )
    | ^(op=(INCR|DECR) IDENT)
        { $lt = StmtTree($op.type, children=[LiteralTree(IDENT, $IDENT.text, pos_off=-2)],
        pos_off=-2); }
    | ^(RETURN { $lt = StmtTree(RETURN); }
            (expr { $lt.add_child($expr.lt); })?
       )
    | ^(IF { if_pos = Status.get_cur_pos() } expr
        { $lt = StmtTree(IF, children=[$expr.lt], pos=if_pos); }
            (s=stmt { $lt.add_child($s.lt); })?
            (ifelse { $lt.add_child($ifelse.lt); })?
       )
    | ^(WHILE expr
            { $lt = StmtTree(LP.WHILE, children=[$expr.lt]); }
            (s=stmt { $lt.add_child($s.lt); })?
       )
    | expr
        { $lt = $expr.lt; }
    ;

ditem returns [item]
    : ^(DITEM IDENT { id_pos = Status.get_cur_pos() } )
        { $item = DeclArg($IDENT.text, id_pos); }
    | ^(ASSIGN IDENT { id_pos = Status.get_cur_pos() } expr)
        { $item = DeclArg($IDENT.text, id_pos, $expr.lt); }
    ;

ifelse returns [lt=StmtTree()]
    : ^(ELSE { e_pos = Status.get_cur_pos() } stmt { $lt = $stmt.lt; })
        { if $lt.pos == '0:0': $lt.pos = e_pos }
    ;

// expressions ---------------------------------------------
expr returns [lt=ExprTree]
    : lit=(IDENT|NUMBER|STRINGLIT|TRUE|FALSE)
        { $lt = LiteralTree($lit.type, $lit.text); }
    | ^(op=(NOT|NEG) e=expr)
        { $lt = UnopTree($op.type, $e.lt); }
    | ^(op=(MULT|DIV|MOD|PLUS|MINUS | LT|LEQ|GT|GEQ|EQ|NEQ | AND|OR) { op_pos = Status.get_cur_pos() }
           a=expr b=expr
       )
        { $lt = BinopTree($op.type, $a.lt, $b.lt, pos=op_pos); }
    | ^(FUNCALL IDENT { $lt = FuncallTree($IDENT.text); }
            (e=expr { $lt.add_child($e.lt); } )*
       )
    ;
