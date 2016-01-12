#!/usr/bin/python2
# -*- coding: utf8 -*-

from __future__ import print_function
from subprocess import check_output, CalledProcessError, STDOUT
import sys

from antlr3 import ANTLRInputStream, ANTLRFileStream, CommonTokenStream
from antlr3.tree import CommonTreeNodeStream
from FuturePrint import debug
from LatteLexer import LatteLexer
from LatteParser import LatteParser
from LatteTreeBuilder import LatteTreeBuilder
from LatteErrors import Status, LatteError
from LatteNodes import *
from LatteProgram import *
from Utils import Flags


class Latc(object):
    """ All methods in this class in order create the compiler's main function.

    They are split just to improve readability."""

    @classmethod
    def setup_args(cls, argv):
        """ Read arguments and open input stream. """
        Flags.parse_args(argv)  # Exits on error.
        if Flags.input_from_stdin():
            cls.filestream = ANTLRInputStream(sys.stdin)
            debug('INPUT: stdin')
        else:
            try:
                cls.filestream = ANTLRFileStream(Flags.input_file)
            except IOError as err:
                Status.add_error(err, fatal=True)
            debug('INPUT: ', Flags.input_file)
        debug('ASM OUTPUT: ', Flags.asm_file)
        debug('BIN OUTPUT: ', Flags.bin_file)

    @classmethod
    def parse_code(cls):
        """ Parse the source code. """
        cls.lexer = LatteLexer(cls.filestream)
        cls.tokens = CommonTokenStream(cls.lexer)
        Status.set_token_stream(cls.tokens)
        cls.parser = LatteParser(cls.tokens)
        cls.parsed_prog = cls.parser.prog()
        if Status.errors() > 0:
            Status.add_error(LatteError('parsing failed'), fatal=True)
        debug('-----------------------------------------------')
        debug('Tree: ', cls.parsed_prog.tree.toStringTree())
        debug('-----------------------------------------------')

    @classmethod
    def build_code_tree(cls):
        """ Build the tree from code AST. """
        cls.nodes = CommonTreeNodeStream(cls.parsed_prog.tree)
        cls.nodes.setTokenStream(cls.tokens)
        Status.set_node_stream(cls.nodes)
        cls.builder = LatteTreeBuilder(cls.nodes)
        cls.prog_tree = cls.builder.prog()
        debug('-----------------------------------------------')
        cls.prog_tree.print_tree()
        debug('-----------------------------------------------')

    @classmethod
    def run_typechecks(cls):
        """ Run type and return checks. """
        cls.prog_tree.check_types()
        if Status.errors() > 0:
            Status.add_error(LatteError('compilation failed'), fatal=True)

    @classmethod
    def build_code(cls):
        """ Generate instructions from the tree. """
        debug('-----------------------------------------------')
        cls.prog_code = ProgCode(cls.prog_tree)
        cls.prog_code.gen_code()
        debug('-----------------------------------------------')
        cls.instructions = [i for i in cls.prog_code.instructions()]
        if Status.errors() > 0:
            Status.add_error(LatteError('compilation failed'), fatal=True)

    @classmethod
    def output_assembly(cls):
        """ Output the assembly code. """
        debug('-----------------------------------------------')
        try:
            asm_file = sys.stdout if Flags.output_to_stdout() else open(Flags.asm_file, 'w')
            for instr in cls.instructions:
                print(instr, file=asm_file)
            if not Flags.output_to_stdout():
                asm_file.close()
        except IOError as err:
            Status.add_error(err, fatal=True)

    @classmethod
    def link_executable(cls):
        """ Make the executable file from assembly and Latte runtime library. """
        if not Flags.output_to_stdout():
            # capture the output, so the possible 'ERROR' message is still in first line
            try:
                gcc_out = check_output(
                    ['gcc', '-m32', Flags.runtime_file, Flags.asm_file, '-o', Flags.bin_file],
                    stderr=STDOUT)
                if gcc_out:
                    Status.add_warning(LatteError('linking messages:\n' + gcc_out))
            except CalledProcessError as err:
                Status.add_error(LatteError('linking failed:\n' + err.output), fatal=True)


def main(argv):
    Latc.setup_args(argv)
    Latc.parse_code()
    Latc.build_code_tree()
    Latc.run_typechecks()
    Latc.build_code()
    Latc.output_assembly()
    Latc.link_executable()
    Status.flush()
    sys.exit(Status.errors())


if __name__ == '__main__':
    main(sys.argv)
