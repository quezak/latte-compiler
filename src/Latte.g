grammar Latte;
/*------------------------------------------------------------------
 * Artur Kozak [320770], MRJP 2015 zadanie 2
 * ANTLR3 gramamr generating abstract syntax tree
 * from Latte source code
 *------------------------------------------------------------------*/

options {
    language = Python;
    output = AST;
    ASTLabelType = CommonTree;
}

// Virtual tokens needed to mark some AST nodes.
tokens {
    PROG;
    FUNDEF;
    ARG;
    BLOCK;
    DECL;
    DITEM;
    DINIT;
    FUNCALL;
    NEG;
    TYPE_ERROR;
}

// Header pasted on the top of parser file.
@header {
from antlr3.dottreegen import *
from FuturePrint import debug, message, warning, error
from LatteLexer import LatteLexer
from LatteErrors import Status, ParserError

# Predefined functions names.
class Builtins:
    PRINT_INT = 'printInt'
    PRINT_STRING = 'printString'
    READ_INT = 'readInt'
    READ_STRING = 'readString'
    ERROR = 'error'
    MAIN = 'main'
}

// Header pasted on the top of lexer file.
@lexer::header {
from LatteErrors import Status, ParserError
}

// Additional methods for parser's class.
@members {
def print_dot_tree(self):
    """ Prints the AST in DOT format, which can be visualized using e.g. graphviz. """
    tree = self.prog().tree
    gen = DOTTreeGenerator()
    st = gen.toDOT(tree)
    token = tree.token
    if Status.errors() > 0:
        sys.exit(Status.errors())
    debug("TOKEN: ", tokenNames[token.type], " | ", token.line, ":", token.charPositionInLine)
    print(st)

def displayRecognitionError(self, tokenNames, e):
    """ Saves the error into the error set. """
    msg = self.getErrorMessage(e, tokenNames)
    Status.add_error(ParserError(msg, e.line, e.charPositionInLine))
}

// Additional methods for lexer's class.
@lexer::members {
def displayRecognitionError(self, tokenNames, e):
    """ Saves the error into the error set. """
    msg = self.getErrorMessage(e, tokenNames)
    Status.add_error(ParserError(msg, e.line, e.charPositionInLine))
}

@main {
def main(argv):
    """ Main function in case the parser is executed directly.
    
    Only for testing purposes. Reads code from stdin, writes diagnostic messages
    and the tree description for graphviz. """
    lexer = LatteLexer(ANTLRInputStream(sys.stdin))
    tokens = CommonTokenStream(lexer)
    parser = LatteParser(tokens)
    parser.print_dot_tree()
}
 
/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/
// Latte grammar, converted to LL* format.

// programs ------------------------------------------------
prog        : (fundef)* EOF -> ^(PROG fundef*);
fundef      : type IDENT arglist block -> ^(FUNDEF type IDENT arglist? block);
arglist     : LPAREN! (arg (LISTSEP! arg)* )? RPAREN!;
arg         : type IDENT -> ^(ARG type IDENT);
type        : INT | STRING | BOOLEAN | VOID;

// statements ----------------------------------------------
block       : LBRACE stmt* RBRACE -> ^(BLOCK stmt*);
stmt        : STMTSEP!
            | block
            | type ditemlist STMTSEP -> ^(DECL type ditemlist)
            | IDENT ASSIGN^ expr STMTSEP!
            | IDENT INCR^ STMTSEP!
            | IDENT DECR^ STMTSEP!
            | RETURN STMTSEP -> ^(RETURN)
            | RETURN^ expr STMTSEP!
            | IF^ condition stmt ((ELSE)=>ifelse)?
            | WHILE^ condition stmt
            | expr STMTSEP!
            ;

ditemlist   : ditem (LISTSEP! ditem)*;
ditem       : IDENT -> ^(DITEM IDENT)
            | IDENT ASSIGN expr -> ^(ASSIGN IDENT expr)
            ;

condition   : LPAREN! expr^ RPAREN!;
ifelse      : ELSE^ stmt;

// expressions ---------------------------------------------
boolean     : TRUE | FALSE;
eAtom       : IDENT^
            | NUMBER^
            | STRINGLIT^
            | boolean^
            | IDENT exprlist -> ^(FUNCALL IDENT exprlist?)
            | LPAREN! expr^ RPAREN!
            ;
eUnary      : NOT^ eUnary
            | MINUS eUnary -> ^(NEG eUnary)
            | eAtom
            ;
eMul        : eUnary ((MULT^ | DIV^ | MOD^) eUnary)*;
eAdd        : eMul ((PLUS^ | MINUS^) eMul)*;
eRel        : eAdd ((LT^ | LEQ^ | GT^ | GEQ^ | EQ^ | NEQ^) eAdd)*;
eAnd        : eRel (AND^ eAnd)?;
eOr         : eAnd (OR^ eOr)?;
expr        : eOr;

exprlist    : LPAREN! (expr (LISTSEP! expr)* )? RPAREN!;

            
/*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*/

STMTSEP     : ';';
LISTSEP     : ',';
LPAREN      : '(';
RPAREN      : ')';
LBRACE      : '{';
RBRACE      : '}';
GEQ         : '>=';
EQ          : '==';
NEQ         : '!=';
INCR        : '++';
DECR        : '--';
PLUS        : '+';
MINUS       : '-';
MULT        : '*';
DIV         : '/';
ASSIGN      : '=';
MOD         : '%';
LT          : '<';
NOT         : '!';
AND         : '&&';
OR          : '||';
LEQ         : '<=';
GT          : '>';
RETURN      : 'return';
IF          : 'if';
ELSE        : 'else';
WHILE       : 'while';
INT         : 'int';
STRING      : 'string';
BOOLEAN     : 'boolean';
VOID        : 'void';
TRUE        : 'true';
FALSE       : 'false';
NUMBER      : ('0'..'9')+;
IDENT       : IDFCHAR (IDCHAR)*;
fragment IDFCHAR : ('a'..'z' | 'A'..'Z' | '_');
fragment IDCHAR : (IDFCHAR | '0'..'9');
STRINGLIT   : '"' ('\\"' | ~'"')* '"';

LINECOMMENT : '//' ~('\r' | '\n')* { $channel = HIDDEN };
COMMENT     : '/*' (options {greedy=false;} : .)* '*/' { $channel = HIDDEN };
WHITESPACE  : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { $channel = HIDDEN };
