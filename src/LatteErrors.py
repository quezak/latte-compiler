#!/usr/bin/python2
# -*- coding: utf8 -*-

from FuturePrint import debug, message, warning, error, note

class Status(object):
    _errors = 0
    _warnings = 0
    _tokens = None
    _nodes = None

    @classmethod
    def errors(cls):
        return cls._errors

    @classmethod
    def addError(cls, exc, fatal=False):
        error(str(exc))
        cls._errors += 1
        if fatal:
            raise exc

    @classmethod
    def addWarning(cls, exc):
        warning(str(exc))
        cls._warnings += 1

    @classmethod
    def addNote(cls, exc):
        note(str(exc))

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
        if not cls._nodes.LT(offset): return None
        token = cls._nodes.LT(offset).token
        if not token: return None
        return '%d:%d' % (token.line, token.charPositionInLine+1)

    @classmethod
    def getPos(cls, pos):
        token = cls._tokens.get(pos)
        return '%d:%d' % (token.line, token.charPositionInLine+1)

    @classmethod
    def setNodeStream(cls, stream):
        cls._nodes = stream


class LatteError(Exception):
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

