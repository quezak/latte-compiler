#!/usr/bin/python2
# -*- coding: utf8 -*-

import sys
import LatteParser as LP
from antlr3 import ANTLRInputStream, CommonTokenStream
from antlr3.tree import CommonTreeNodeStream
from FuturePrint import debug
from LatteLexer import LatteLexer
from LatteParser import LatteParser
from LatteTreeBuilder import LatteTreeBuilder
from LatteErrors import Status
from LatteNodes import *
from LatteProgram import *


def main(argv):
    # [1] parsowanie
    lexer = LatteLexer(ANTLRInputStream(sys.stdin))
    tokens = CommonTokenStream(lexer)
    Status.setTokenStream(tokens)
    parser = LatteParser(tokens)
    #tree_adaptor = LatteASTAdaptor()
    #parser.setTreeAdaptor(tree_adaptor)
    #parser.printDotTree()
    parsed_prog = parser.prog()
    if Status.errors() > 0:
        sys.exit(Status.errors())
    debug("-----------------------------------------------");
    debug("Tree: ", parsed_prog.tree.toStringTree())
    debug("-----------------------------------------------");
    # [2] budowa drzewa
    nodes = CommonTreeNodeStream(parsed_prog.tree)
    nodes.setTokenStream(tokens)
    Status.setNodeStream(nodes)
    walker = LatteTreeBuilder(nodes)
    prog_tree = walker.prog()
    # [3] sprawdzenie typów
    debug("-----------------------------------------------");
    prog_tree.printTree()
    debug("-----------------------------------------------");
    prog_tree.checkTypes()
    if Status.errors() > 0:
        sys.exit(Status.errors())
    # [4] budowa drzewa kodu
    debug("-----------------------------------------------");
    prog_code = ProgCode(prog_tree)
    prog_code.genCode()
    debug("-----------------------------------------------");
    instructions = [i for i in prog_code.instructions()]
    if Status.errors() > 0:
        sys.exit(Status.errors())
    # [6] wypisanie kodu wynikowego
    debug("-----------------------------------------------");
    for instr in instructions:
        print instr
    sys.exit(Status.errors())


if __name__ == '__main__':
    main(sys.argv)
