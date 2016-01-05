tree grammar LatteTreeBuilder;
/*------------------------------------------------------------------
 * Artur Kozak [320770], MRJP 2014 zadanie 2
 * Gramatyka ANTLR3 zamieniajaca AST na drzewo do dalszego przetwarzania
 *------------------------------------------------------------------*/

options {
    language = Python;
    tokenVocab = Latte;
    ASTLabelType = CommonTree;
}

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
    lexer = LatteLexer(ANTLRInputStream(sys.stdin))
    tokens = CommonTokenStream(lexer)
    Status.setTokenStream(tokens)
    parser = LatteParser(tokens)
    #tree_adaptor = LatteASTAdaptor()
    #parser.setTreeAdaptor(tree_adaptor)
    #parser.printDotTree()
    prog = parser.prog()
    debug("-----------------------------------------------");
    debug("Tree: ", prog.tree.toStringTree())
    debug("-----------------------------------------------");
    nodes = CommonTreeNodeStream(prog.tree)
    nodes.setTokenStream(tokens)
    Status.setNodeStream(nodes)
    walker = LatteTreeBuilder(nodes)
    root = walker.prog()
    debug("functions: \%d" \% len(root.children))
    debug("symbols: \%d" \% len(root.symbols))
}

/*------------------------------------------------------------------
 * reguly parsera AST
 *------------------------------------------------------------------*/

// program -------------------------------------------------
prog returns [lt=ProgTree()]
    : ^(PROG (fundef { $lt.addFunTree($fundef.lt); } )* )
    ;

fundef returns [lt=FunTree()]
    : ^(FUNDEF
            type { $lt.setRetType($type.id); }
            IDENT { $lt.setName($IDENT.text); }
            (arg { $lt.addArg($arg.fa); })*
            block { $lt.setBlock($block.lt); }
       )
    ;

arg returns [fa]
    : ^(ARG type IDENT) { $fa = FunArg($type.id, $IDENT.text); }
    ;

type returns [id]
    : t=(INT | STRING | BOOLEAN | VOID) { $id = $t.type; }
    ;

block returns [lt=BlockTree()]
    : ^(BLOCK (stmt { $lt.addStmt($stmt.lt); })* )
    ;

// statements ----------------------------------------------
stmt returns [lt=StmtTree()]
    : block
        { $lt = $block.lt; }
    | ^(DECL type { $lt = DeclTree($type.id); }
            (ditem { $lt.addItem($ditem.item); })+
       )
    | ^(ASSIGN { as_pos = Status.getCurPos() } IDENT expr)
        { $lt = StmtTree(ASSIGN, children=[LiteralTree(IDENT, $IDENT.text), $expr.lt], pos=as_pos); }
    | ^(op=(INCR|DECR) IDENT)
        { $lt = StmtTree($op.type, children=[LiteralTree(IDENT, $IDENT.text, pos_off=-2)]); }
    | ^(RETURN { $lt = StmtTree(RETURN); }
            (expr { $lt.addChild($expr.lt); })?
       )
    | ^(IF { if_pos = Status.getCurPos() } expr s=stmt 
        { $lt = StmtTree(IF, children=[$expr.lt, $s.lt], pos=if_pos); }
            (ifelse { $lt.addChild($ifelse.lt); })?
       )
    | ^(WHILE expr s=stmt)
        { $lt = StmtTree(LP.WHILE, children=[$expr.lt, $s.lt]); }
    | expr
        { $lt = $expr.lt; }
    ;

ditem returns [item]
    : ^(DITEM IDENT { id_pos = Status.getCurPos() } )
        { $item = DeclArg($IDENT.text, id_pos); }
    | ^(ASSIGN IDENT { id_pos = Status.getCurPos() } expr)
        { $item = DeclArg($IDENT.text, id_pos, $expr.lt); }
    ;

ifelse returns [lt=StmtTree()]
    : ^(ELSE { e_pos = Status.getCurPos() } stmt { $lt = $stmt.lt; })
        { if $lt.pos == '0:0': $lt.pos = e_pos }
    ;

// expressions ---------------------------------------------
expr returns [lt=ExprTree]
    : lit=(IDENT|NUMBER|STRINGLIT|TRUE|FALSE)
        { $lt = LiteralTree($lit.type, $lit.text); }
    | ^(op=(NOT|NEG) e=expr)
        { $lt = UnopTree($op.type, $e.lt); }
    | ^(op=(MULT|DIV|MOD|PLUS|MINUS | LT|LEQ|GT|GEQ|EQ|NEQ | AND|OR) { op_pos = Status.getCurPos() }
           a=expr b=expr
       )
        { $lt = BinopTree($op.type, $a.lt, $b.lt, pos=op_pos); }
    | ^(FUNCALL IDENT { $lt = FuncallTree($IDENT.text); }
            (e=expr { $lt.addChild($e.lt); } )*
       )
    ;
