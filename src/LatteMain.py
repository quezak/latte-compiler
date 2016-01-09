#!/usr/bin/python2
# -*- coding: utf8 -*-

from __future__ import print_function
from subprocess import check_output, CalledProcessError, STDOUT
import sys

import LatteParser as LP
from antlr3 import ANTLRInputStream, ANTLRFileStream, CommonTokenStream
from antlr3.tree import CommonTreeNodeStream
from FuturePrint import debug, message
from LatteLexer import LatteLexer
from LatteParser import LatteParser
from LatteTreeBuilder import LatteTreeBuilder
from LatteErrors import Status, LatteError
from LatteNodes import *
from LatteProgram import *
from Utils import Flags


def main(argv):
    # [1] read arguments and open input
    Flags.parse_args(argv) # Exits on error.
    if Flags.input_from_stdin():
        filestream = ANTLRInputStream(sys.stdin)
        debug("INPUT: stdin")
    else:
        try:
            filestream = ANTLRFileStream(Flags.input_file)
        except IOError, err:
            Status.addError(err, fatal=True)
        debug("INPUT: ", Flags.input_file)
    debug("ASM OUTPUT: ", Flags.asm_file)
    debug("BIN OUTPUT: ", Flags.bin_file)
    # [2] parse the code
    # TODO catch parser errors
    lexer = LatteLexer(filestream)
    tokens = CommonTokenStream(lexer)
    Status.setTokenStream(tokens)
    parser = LatteParser(tokens)
    parsed_prog = parser.prog()
    if Status.errors() > 0:
        sys.exit(Status.errors())
    debug("-----------------------------------------------")
    debug("Tree: ", parsed_prog.tree.toStringTree())
    debug("-----------------------------------------------")
    # [2] build the tree
    nodes = CommonTreeNodeStream(parsed_prog.tree)
    nodes.setTokenStream(tokens)
    Status.setNodeStream(nodes)
    builder = LatteTreeBuilder(nodes)
    prog_tree = builder.prog()
    debug("-----------------------------------------------")
    prog_tree.printTree()
    debug("-----------------------------------------------")
    # [3] typechecks
    prog_tree.checkTypes()
    if Status.errors() > 0:
        sys.exit(Status.errors())
    # [4] build code nodes
    debug("-----------------------------------------------")
    prog_code = ProgCode(prog_tree)
    prog_code.genCode()
    debug("-----------------------------------------------")
    instructions = [i for i in prog_code.instructions()]
    if Status.errors() > 0:
        sys.exit(Status.errors())
    # [6] assembly output
    debug("-----------------------------------------------")
    try:
        asm_file = sys.stdout if Flags.output_to_stdout() else open(Flags.asm_file, "w")
        for instr in instructions:
            print(instr, file=asm_file)
        if not Flags.output_to_stdout():
            asm_file.close()
    except IOError, err:
        Status.addError(err, fatal=True)
    # [7] link runtime library
    if not Flags.output_to_stdout():
        # capture the output, so the possible 'ERROR' message is still in first line
        try:
            gcc_out = check_output(
                    ["gcc", "-m32", Flags.runtime_file, Flags.asm_file, "-o", Flags.bin_file],
                    stderr=STDOUT)
            if gcc_out:
                Status.addWarning(LatteError("linking messages:\n" + gcc_out))
        except CalledProcessError, err:
            Status.addError(LatteError("linking failed:\n" + err.output), fatal=True)
    if Status.errors() == 0:
        message("OK") # task requirements
    sys.exit(Status.errors())


if __name__ == '__main__':
    main(sys.argv)
