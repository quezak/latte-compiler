#!/usr/bin/python2
# -*- coding: utf8 -*-

import sys

import FuturePrint as FP

class Status(object):
    """ A class for collecting status messages (errors, warnings and notes). """
    _errors = 0
    _warnings = 0
    _tokens = None
    _nodes = None

    @classmethod
    def errors(cls):
        return cls._errors

    @classmethod
    def addError(cls, exc, fatal=False):
        if cls._errors  == 0: FP.message("ERROR") # task requirements
        FP.error(str(exc))
        cls._errors += 1
        if fatal:
            FP.message("aborting after a fatal error")
            sys.exit(1)

    @classmethod
    def addWarning(cls, exc):
        FP.warning(str(exc))
        cls._warnings += 1

    @classmethod
    def addNote(cls, exc):
        FP.note(str(exc))

    @classmethod
    def errors(cls):
        return cls._errors
    
    @classmethod
    def warnings(cls):
        return cls._warnings

    @classmethod
    def setTokenStream(cls, stream):
        cls._tokens = stream

    @classmethod
    def getCurPos(cls, offset=-1):
        """ Try to get the current token's position in source code. """
        if not cls._nodes.LT(offset): return None
        token = cls._nodes.LT(offset).token
        if not token: return None
        return '%d:%d' % (token.line, token.charPositionInLine+1)

    @classmethod
    def getPos(cls, pos):
        """ Get the source code position of a given token. """
        token = cls._tokens.get(pos)
        return '%d:%d' % (token.line, token.charPositionInLine+1)

    @classmethod
    def setNodeStream(cls, stream):
        cls._nodes = stream


class LatteError(Exception):
    """ Main class for all compiler's errors. """
    def __init__(self, msg, pos=None):
        self.msg = msg
        self.pos = ('at ' + pos + ': ') if pos else ''
    def __str__(self):
        return self.pos + self.msg


class InternalError(LatteError):
    def __str__(self):
        return 'internal error: ' + super(InternalError, self).__str__()


class ParserError(LatteError):
    def __init__(self, msg, pos_l, pos_c):
        pos = '%d:%d' % (pos_l, pos_c+1)
        super(ParserError, self).__init__(msg, pos)

    def __str__(self):
        return 'syntax error: ' + super(ParserError, self).__str__()


class TypecheckError(LatteError):
    _type = 'type error'

