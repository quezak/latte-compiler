#!/usr/bin/python2
# -*- coding: utf8 -*-
""" Exception classes and status/error manager. """

import sys
import re

import FuturePrint as FP
import Utils


class Status(object):
    """ A class for collecting status messages (errors, warnings and notes). """
    _errors = 0
    _warnings = 0
    _tokens = None
    _nodes = None
    _all_messages = []
    _header_output = False

    @classmethod
    def _add_message(cls, handler, exc):
        """ Store message to be output with handler() after the initial 'OK'/'ERROR'

        (but print immediately if debug mode is on) """
        if not Utils.Flags.debug:
            cls._all_messages.append((handler, exc))
        else:
            handler(str(exc))

    @classmethod
    def flush(cls):
        """ Flush the output buffer, adding the required 'OK'/'ERROR' on first line. """
        if not cls._header_output:
            FP.message('OK' if cls._errors == 0 else 'ERROR')
            cls._header_output = True
        for handler, exc in cls._all_messages:
            handler(exc)
        cls._all_messages = []

    @classmethod
    def add_error(cls, exc, fatal=False):
        cls._add_message(FP.error, exc)
        cls._errors += 1
        if fatal:
            cls.flush()
            FP.message('aborting after a fatal error')
            sys.exit(cls._errors if cls._errors > 0 else 1)

    @classmethod
    def add_warning(cls, exc):
        cls._add_message(FP.warning, exc)
        cls._warnings += 1

    @classmethod
    def add_note(cls, exc):
        cls._add_message(FP.note, exc)

    @classmethod
    def errors(cls):
        return cls._errors

    @classmethod
    def warnings(cls):
        return cls._warnings

    @classmethod
    def set_token_stream(cls, stream):
        cls._tokens = stream

    @classmethod
    def get_cur_pos(cls, offset=-1):
        """ Try to get the current token's position in source code. """
        if not cls._nodes.LT(offset):
            return None
        token = cls._nodes.LT(offset).token
        if not token:
            return None
        return '%d:%d' % (token.line, token.charPositionInLine+1)

    @classmethod
    def get_pos(cls, pos):
        """ Get the source code position of a given token. """
        token = cls._tokens.get(pos)
        return '%d:%d' % (token.line, token.charPositionInLine+1)

    @classmethod
    def set_node_stream(cls, stream):
        cls._nodes = stream


class LatteError(Exception):
    """ Main class for all compiler's errors. """
    def __init__(self, msg, pos=None):
        # apply formatting to `quoted fragments`
        self.msg = re.sub(r'`([^`]*)`', self._replace_quote, msg)
        self.pos = ('at ' + FP.Colors.pos(pos) + ': ') if pos else ''

    @staticmethod
    def _replace_quote(matchobj):
        return FP.Colors.quote(matchobj.group(1))

    def __str__(self):
        return self.pos + self.msg


class InternalError(LatteError):
    def __str__(self):
        return 'internal error: ' + super(InternalError, self).__str__()


class ParserError(LatteError):
    def __init__(self, msg, pos_l, pos_c):
        pos = '%d:%d' % (pos_l, pos_c+1)
        # adjust the quotes generated by the parser to match our formatting functions later
        msg = re.sub(r'u\'([^\']*)\'', r'`\1`', msg)
        msg = re.sub(r'\'([^\']*)\'', r'`\1`', msg)
        super(ParserError, self).__init__(msg, pos)

    def __str__(self):
        return 'syntax error: ' + super(ParserError, self).__str__()


class TypecheckError(LatteError):
    _type = 'type error'
