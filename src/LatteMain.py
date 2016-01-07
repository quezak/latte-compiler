#!/usr/bin/python2
# -*- coding: utf8 -*-

import sys

import LatteParser as LP
from antlr3 import ANTLRInputStream, ANTLRFileStream, CommonTokenStream
from antlr3.tree import CommonTreeNodeStream
from FuturePrint import debug
from LatteLexer import LatteLexer
from LatteParser import LatteParser
from LatteTreeBuilder import LatteTreeBuilder
from LatteErrors import Status
from LatteNodes import *
from LatteProgram import *
from Utils import Flags


def main(argv):
    # [1] read arguments
    Flags.parse_args(argv) # Exits on error.
    if Flags.input_from_stdin():
        filestream = ANTLRInputStream(sys.stdin)
        debug("INPUT: stdin")
    else:
        filestream = ANTLRFileStream(Flags.input_file)
        debug("INPUT: ", Flags.input_file)
    # [2] parse the code
    lexer = LatteLexer(filestream)
    tokens = CommonTokenStream(lexer)
    Status.setTokenStream(tokens)
    parser = LatteParser(tokens)
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
