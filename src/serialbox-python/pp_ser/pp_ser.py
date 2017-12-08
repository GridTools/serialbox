#!/usr/bin/env python
# -*- coding: utf-8 -*-
##===-----------------------------------------------------------------------------*- Python -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

from __future__ import print_function

import filecmp
import linecache
import os
import re
import shutil
import sys
import tempfile

"""
pp_ser.py

Parser to expand $!SER serialization directives in Fortran code in order to generate
serialization code using the m_serialize.f90 interface for the STELLA serialization
framework.

The grammar is defined by a set of !$SER directives. All directives are case-
insensitive. The main keywords are INIT for initialization, VERBATIM for echoeing
some Fortran statements, OPTION for setting specific options for the serialization
module, REGISTER for registering a data field meta-information,
ZERO for setting some field to zero, SAVEPOINT for registering a savepoint with some
optional information, DATA for serializing a data field, and CLEANUP for finishing
serialization.

The implementation uses two passes. The first pass collects all necessary calls
which is then used for a corresponding USE statement importing the necessary
methods from the Fortran serialization module. The second pass expands the directives.
"""

# information
__author__ = 'Oliver Fuhrer'
__copyright__ = 'Copyright 2014, MeteoSwiss'
__license__ = 'GPL'
__version__ = '0.1'
__date__ = 'Sun Mar 23 22:06:44 2014'
__email__ = 'oliver.fuhrer@meteoswiss.ch'


def to_ascii(text):
    if sys.version_info[0] == 3:
        return bytes(text, 'ascii')
    else:
        return str(text)

def filter_fortran(f):
    return (f.split('.')[-1].lower() in ['f90','inc','incf'])

def build_tree(src, dest, filtered_list, file_filter):
    if os.path.isdir(src):
        if not os.path.isdir(dest):
            os.makedirs(dest)
        files = os.listdir(src)
        filtered_list.extend([os.path.join(src,f) for f in files if os.path.isfile(os.path.join(src,f)) and file_filter(f)])
        dirs = [f for f in files if os.path.isdir(os.path.join(src,f))]
        for d in dirs:
            build_tree(os.path.join(src, d), os.path.join(dest, d), filtered_list, file_filter)
    else:
        if os.path.isfile(src) and file_filter(src):
            filtered_list.append(src)

class PpSer:

    def __init__(self, infile, outfile='', ifdef='SERIALIZE', real='ireals',
                 module='m_serialize', identical=True, verbose=False,
                 acc_prefix=True, acc_if='', modules=''):

        # public variables
        self.verbose = verbose
        self.infile = infile          # input file
        self.outfile = outfile        # output file
        self.ifdef = ifdef            # write #ifdef/#endif blocks
        self.real = real              # name of real type (Fortran)
        self.module = module          # name of Fortran module which contains serialization methods
        self.identical = identical    # write identical files (no preprocessing done)?
        self.acc_prefix = acc_prefix  # generate preprocessing marco for ACC_PREFIX
        self.acc_if = acc_if          # generate IF clause after OpenACC update

        # setup (also public)
        self.methods = {
            'mode':             'ppser_set_mode',
            'getmode':          'ppser_get_mode',
            'init':             'ppser_initialize',
            'cleanup':          'ppser_finalize',
            'data':             'fs_write_field',
            'datawrite':        'fs_write_field',
            'dataread':         'fs_read_field',
            'datareadperturb':  'fs_read_and_perturb_field',
            'option':           'fs_Option',
            'serinfo':          'fs_add_serializer_metainfo',
            'register':         'fs_register_field',
            'registertracers':  'fs_RegisterAllTracers',
            'fieldmetainfo':    'fs_AddFieldMetaInfo',
            'savepoint':        'fs_create_savepoint',
            'spinfo':           'fs_add_savepoint_metainfo',
            'fieldinfo':        'fs_add_field_metainfo',
            'on':               'fs_enable_serialization',
            'off':              'fs_disable_serialization'
        }

        # language definition (also public)
        self.language = {
            'cleanup':         ['CLEANUP', 'CLE'],
            'data':            ['DATA', 'DAT'],
            'accdata':         ['ACCDATA', 'ACC'],
            'mode':            ['MODE', 'MOD'],
            'init':            ['INIT', 'INI'],
            'option':          ['OPTION', 'OPT'],
            'metainfo':        ['METAINFO'],
            'verbatim':        ['VERBATIM', 'VER'],
            'register':        ['REGISTER', 'REG'],
            'registertracers': ['REGISTERTRACERS'],
            'zero':            ['ZERO', 'ZER'],
            'savepoint':       ['SAVEPOINT', 'SAV'],
            'tracer':          ['TRACER', 'TRA'],
            'on':              ['ON'],
            'off':             ['OFF']
        }

        self.modes = {
            'write':        0,
            'read':         1,
            'read-perturb': 2,
            'CPU':          0,
            'GPU':          1
        }

        self.intentin_to_remove = []
        self.intentin_removed = []

        # private variables
        self.__ser = False           # currently processing !$SER directives
        self.__line = ''             # current line
        self.__linenum = 0           # current line number
        self.__module = ''           # current module
        self.__calls = set()         # calls to serialization module
        self.__outputBuffer = ''     # preprocessed file
        self.__use_stmt_in_module = False  # USE statement was inserted in module
        self.__extra_module = []     # extra module to add to use statement
        self.__skip_next_n_lines = 0 # Number of line to skip (use for lookahead)

        if modules: 
            self.__extra_module = modules.split(',')

        # define compute sign used in field definition. If one is matched,
        # the read call is not added
        self.__computed_fields_sign = ['*', '+', '-', '/', 'merge']

    # shortcuts for field registering
    def __reg_shortcuts(self, shortcut):
        shortcut = shortcut.upper()
        l = []
        if re.match('(^$|[IJK][IJK1-9]*)', shortcut):
            if shortcut == '':
                l = '1 0 0 0 0 0 0 0 0 0 0 0'.split()
            elif shortcut == 'I':
                l = 'ie 0 0 0 nboundlines nboundlines 0 0 0 0 0 0'.split()
            elif shortcut == 'J':
                l = 'je 0 0 0 nboundlines nboundlines 0 0 0 0 0 0'.split()
            elif shortcut == 'J2':
                l = 'je 2 0 0 nboundlines nboundlines 0 0 0 0 0 0'.split()
            elif shortcut == 'K':
                l = 'ke 0 0 0 0 0 0 0 0 0 0 0'.split()
            elif shortcut == 'K1':
                l = 'ke1 0 0 0 0 1 0 0 0 0 0 0'.split()
            elif shortcut == 'IJ':
                l = 'ie je 0 0 nboundlines nboundlines nboundlines nboundlines 0 0 0 0'.split()
            elif shortcut == 'IJ3':
                l = 'ie je 3 0 nboundlines nboundlines nboundlines nboundlines 0 0 0 0'.split()
            elif shortcut == 'IK':
                l = 'ie ke 0 0 nboundlines nboundlines 0 0 0 0 0 0'.split()
            elif shortcut == 'IK1':
                l = 'ie ke1 0 0 nboundlines nboundlines 0 0 0 1 0 0'.split()
            elif shortcut == 'JK':
                l = 'je ke 0 0 nboundlines nboundlines 0 0 0 0 0 0'.split()
            elif shortcut == 'JK1':
                l = 'je ke1 0 0 nboundlines nboundlines 0 1 0 0 0 0'.split()
            elif shortcut == 'IJK':
                l = 'ie je ke 0 nboundlines nboundlines nboundlines nboundlines 0 0 0 0'.split()
            elif shortcut == 'IJK1':
                l = 'ie je ke1 0 nboundlines nboundlines nboundlines nboundlines 0 1 0 0'.split()
        return l

    # error handling
    def __exit_error(self, directive='', msg=''):
        print('File: "' + self.infile + '", line ' + str(self.__linenum))
        if directive:
            print('SyntaxError: Invalid !$SER ' + directive + ' directive')
        if msg:
            print('Message: '+msg)
        if self.__line:
            print('Line '+str(self.__linenum)+': '+self.__line)
        sys.exit(1)

    # general SER directive arguments parser
    def __ser_arg_parse(self, args):
        # returns list of directives, lists of key=value pairs and (optional) IF statmenet
        dirs = []    # directives
        keys = []    # keys
        values = []  # values
        if_encountered = False
        if_statement = ''
        for arg in args[1:]:
            if arg.upper() == 'IF':
                if_encountered = True
                continue
            if if_encountered:
                if if_statement:
                    self.__exit_error(directive=args[0],
                                      msg='IF statement must be last argument')
                if_statement = arg
            else:
                val = arg.split('=')
                if len(val) == 1:
                    dirs.append(val[0])
                elif len(val) == 2:
                    keys.append(val[0])
                    values.append(val[1])
                else:
                    self.__exit_error(directive=args[0],
                                      msg='Problem extracting arguments and key=value pairs')
        return dirs, keys, values, if_statement

    # parser for tracer directive
    def __ser_tracer_parse(self, args):
        tracersspec = []
        if_encountered = False
        if_statement = ''

        pattern = '^([a-zA-Z_0-9]+|\$[a-zA-Z_0-9\(\)]+(?:-[a-zA-Z_0-9\(\)]+)?|%all)'  # Tracer name, id(s) or %all
        pattern += '(?:#(tens|bd|surf|sedimvel))?'  # Type (stype)
        pattern += '(?:@([a-zA-Z_0-9]+))?'  # Time level (timelevel)
        r = re.compile(pattern)

        for arg in args[1:]:
            if arg.upper() == 'IF':
                if_encountered = True
                continue
            if if_encountered:
                if if_statement:
                    self.__exit_error(directive=args[0],
                                      msg='IF statement must be last argument')
                if_statement = arg
            else:
                m = r.search(arg)
                if m is None:
                    self.__exit_error(directive=args[0],
                                      msg='Tracer specification ' + arg + ' is invalid')
                tracersspec.append(m.groups())
        return tracersspec, if_statement

    # INIT directive
    def __ser_init(self, args):

        (dirs, keys, values, if_statement) = self.__ser_arg_parse(args)

        l = ''
        tab = ''
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
            tab = '  '

        l += tab + 'PRINT *, \'>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<\'\n'
        l += tab + 'PRINT *, \'>>> WARNING: SERIALIZATION IS ON <<<\'\n'
        l += tab + 'PRINT *, \'>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<\'\n'
        l += tab + '\n'
        l += tab + '! setup serialization environment\n'

        args_lower = [item.lower() for item in args]
        if 'if' in args_lower:
            if_pos = args_lower.index('if'.lower())
        else:
            if_pos = len(args)

        l += tab + 'call ' + self.methods['init'] + '(' + ','.join(args[1:if_pos]) + ')\n'
        if if_statement:
            l += 'ENDIF\n'
        self.__calls.add(self.methods['init'])
        self.__line = l

    # OPTION directive
    def __ser_option(self, args):
        (dirs, keys, values, if_statement) = self.__ser_arg_parse(args)
        if len(dirs) != 0:
            self.__exit_error(directive=args[0],
                              msg='Must specify a name and a list of key=value pairs')
        l = ''
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
        l += 'call ' + self.methods['option'] + '('
        for i in range(len(keys)):
            if keys[i].lower() == 'verbosity':
                if values[i].lower() == 'off':
                    values[i] = '0'
                if values[i].lower() == 'on':
                    values[i] = '1'
            if i == 0:
                l += keys[i] + '=' + values[i]
            else:
                l += ', ' + keys[i] + '=' + values[i]
        l += ')\n'
        if if_statement:
            l += 'ENDIF\n'
        self.__calls.add(self.methods['option'])
        self.__line = l

    # METAINFO directive
    def __ser_metainfo(self, args):
        (dirs, keys, values, if_statement) = self.__ser_arg_parse(args)
        l, tab = '', ''
        self.__calls.add(self.methods['serinfo'])
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
            tab = '  '
        for k, v in zip(keys, values):
            l += tab + 'CALL ' + self.methods['serinfo'] + '(ppser_serializer, "' + k + '", ' + v + ')\n'
        for d in dirs:
            l += tab + 'CALL ' + self.methods['serinfo'] + '(ppser_serializer, "' + d + '", ' + d + ')\n'
        if if_statement:
            l += 'ENDIF\n'
        self.__line = l

    # VERBATIM directive
    def __ser_verbatim(self, args):
        # simply remove $!SER directive for verbatim statements
        self.__line = ' '.join(args[1:]) + '\n'

    # REGISTER directive
    def __ser_register(self, args):

        # parse arguments
        (dirs, keys, values, if_statement) = self.__ser_arg_parse(args)
        if len(dirs) < 2:
            self.__exit_error(directive=args[0],
                              msg='Must specify a name, a type and the field sizes')

        if len(dirs) == 2:
            dirs.append('')

        # quote name
        dirs[0] = "'" + dirs[0] + "'"

        # data type
        datatypes = dict(integer=["'int'", 'ppser_intlength'], real=['ppser_realtype', 'ppser_reallength'])

        if dirs[1] not in datatypes:
            self.__exit_error(directive=args[0], msg='Data type "{0}" not understood. Valid types are: {1}'.
                              format(dirs[1], ', '.join('"' + d + '"' for d in datatypes.keys())))

        dirs[1:2] = datatypes[dirs[1]]

        # implement some shortcuts for often recurring patterns
        if len(dirs) == 4:
            l = self.__reg_shortcuts(dirs[3])
            if l:
                dirs[3:4] = l

        # REGISTER [arg ...]
        l = ''
        tab = ''
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
            tab = '  '

        # registration
        self.__calls.add(self.methods['register'])
        l += tab + 'call ' + self.methods['register'] + '(ppser_serializer, ' + ', '.join(dirs) + ')\n'

        # metainfo
        if len(keys) > 0:
            self.__exit_error(directive=args[0],
                              msg='Metainformation for fields are not yet implemented')
        # for k,v in zip(keys, values):
        #    l += tab + 'call ' + self.methods['fieldmetainfo'] + '
        #       (ppser_serializer, ' + dirs[0] + ', "' + k + '", ' + v + ')\n'

        if if_statement:
            l += 'ENDIF\n'

        self.__line = l

    # REGISTERTRACERS directive
    def __ser_registertracers(self, args):
        l = 'call fs_RegisterAllTracers()\n'
        self.__calls.add(self.methods['registertracers'])
        self.__line = l

    # ZERO directive
    def __ser_zero(self, args):
        (dirs, keys, values, if_statement) = self.__ser_arg_parse(args)
        if len(keys) > 0:
            self.__exit_error(directive=args[0],
                              msg='Must specify a list of fields')
        l = ''
        tab = ''
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
            tab = '  '
        for arg in dirs:
            l += tab + arg + ' = 0.0_' + self.real + '\n'
        if if_statement:
            l += 'ENDIF\n'
        self.__line = l

    # SAVEPOINT directive
    def __ser_savepoint(self, args):
        (dirs, keys, values, if_statement) = self.__ser_arg_parse(args)
        # extract save point name
        if len(dirs) != 1:
            self.__exit_error(directive=args[0],
                              msg='Must specify a name and a list of key=value pairs')
        name = dirs[0]
        # generate serialization code
        l = ''
        tab = ''
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
            tab = '  '
        self.__calls.add(self.methods['savepoint'])
        self.__calls.add(self.methods['spinfo'])
        l += tab + 'call ' + self.methods['savepoint'] + '(\'' + name + '\', ppser_savepoint)\n'
        for k, v in zip(keys, values):
            l += tab + 'call ' + self.methods['spinfo'] + '(ppser_savepoint, \'' + k + '\', ' + v + ')\n'
        if if_statement:
            l += 'ENDIF\n'
        self.__line = l

    # MODE directive
    def __ser_mode(self, args):
        self.__calls.add(self.methods['mode'])
        (dirs, keys, values, if_statement) = self.__ser_arg_parse(args)
        l = ''
        tab = ''
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
            tab = '  '
        if args[1] in self.modes:
            l += tab + 'call ' + self.methods['mode'] + '(' + str(self.modes[args[1]]) + ')\n'
        else:
            l += tab + 'call ' + self.methods['mode'] + '(' + args[1] + ')\n'
        if if_statement:
            l += 'ENDIF\n'
        self.__line = l

    # DATA directive
    def __ser_data(self, args, isacc=False):

        (dirs, keys, values, if_statement) = self.__ser_arg_parse(args)

        # generate serialization code
        self.__calls.add(self.methods['datawrite'])
        self.__calls.add(self.methods['dataread'])
        self.__calls.add(self.methods['datareadperturb'])
        self.__calls.add(self.methods['getmode'])
        l = '! file: ' + self.infile + ' lineno: #' + str(self.__linenum) + '\n'
        tab = ''
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
            tab = '  '

        for v in values:
            v = re.sub(r'\(.+\)', '', v)
            if v not in self.intentin_to_remove:
                self.intentin_to_remove.append(v)

        l += tab + 'SELECT CASE ( ' + self.methods['getmode'] + '() )\n'
        l += tab + '  ' + 'CASE(' + str(self.modes['write']) + ')\n'
        for k, v in zip(keys, values):
            if isacc:  # Generate acc update directives only for accdata clause
                l += tab + '    ' + 'ACC_PREFIX UPDATE HOST ( ' + v + ' )'
                # Generate IF clause if needed
                if len(self.acc_if) > 0:
                    l += ', IF (' + self.acc_if + ') \n'
                else:
                    l += '\n'
            l += tab + '    ' + 'call ' + self.methods['datawrite'] + \
                '(ppser_serializer, ppser_savepoint, \'' + k + '\', ' + v + ')\n'
        l += tab + '  ' + 'CASE(' + str(self.modes['read']) + ')\n'
        for k, v in zip(keys, values):
            # If the field does not contains any compute sign, the read call is
            # generated
            if not any(ext in v for ext in self.__computed_fields_sign):
                l += tab + '    ' + 'call ' + self.methods['dataread'] + '(ppser_serializer_ref, ppser_savepoint, \'' \
                     + k + '\', ' + v + ')\n'
                if isacc:  # Generate acc upadte directives only for accdata clause
                    l += tab + '    ' + 'ACC_PREFIX UPDATE DEVICE ( ' + v + ' )'
                    # Generate IF clause if needed
                    if len(self.acc_if) > 0:
                        l += ', IF (' + self.acc_if + ') \n'
                    else:
                        l += '\n'
        l += tab + '  ' + 'CASE(' + str(self.modes['read-perturb']) + ')\n'
        for k, v in zip(keys, values):
            # If the field does not contains any compute sign, the read call is
            # generated
            if not any(ext in v for ext in self.__computed_fields_sign):
                l += tab + '    ' + 'call ' + self.methods['datareadperturb'] + \
                     '(ppser_serializer_ref, ppser_savepoint, \'' + k + '\', ' + v + ', ppser_zrperturb)\n'
                if isacc:  # Generate acc upadte directives only for accdata clause
                    l += tab + '    ' + 'ACC_PREFIX UPDATE DEVICE ( ' + v + ' )'
                    # Generate IF clause if needed
                    if len(self.acc_if) > 0:
                        l += ', IF (' + self.acc_if + ') \n'
                    else:
                        l += '\n'

        l += tab + 'END SELECT\n'

        if if_statement:
            l += 'ENDIF\n'
        self.__line = l

    # TRACER directive
    def __ser_tracer(self, args):

        (tracerspec, if_statement) = self.__ser_tracer_parse(args)

        l = ''
        tab = ''
        if if_statement:
            l += 'IF (' + if_statement + ') THEN\n'
            tab = '  '

        for t in tracerspec:
            function = 'ppser_write_tracer_'
            fargs = []

            # Function name and first arguments
            if t[0] == '%all':
                # %all specifier
                function += 'all'
            elif t[0][0] == '$':
                # Index-based access
                function += 'bx_idx'
                idxs = t[0][1:]
                if '-' in idxs:
                    fargs += idxs.split('-')
                else:
                    fargs += [idxs]
            else:
                # Name-based access
                function += 'by_name'
                fargs.append("'" + t[0] + "'")

            # Required stype argument
            fargs.append("stype='" + (t[1] or '') + "'")

            # Other arguments
            for i, argname in enumerate(('timelevel',), 2):
                if t[i]:
                    fargs.append(argname + '=' + t[i])

            # Put together function call
            l += tab + 'call ' + function + '(' + ', '.join(fargs) + ')\n'

        if if_statement:
            l += 'ENDIF\n'
        self.__line = l

    # CLEANUP directive
    def __ser_cleanup(self, args):
        l = ''
        l += '! cleanup serialization environment\n'
        l += 'call ' + self.methods['cleanup'] + '(' + ','.join(args[1:]) + ')\n'
        self.__calls.add(self.methods['cleanup'])
        self.__line = l

    # ON directive
    def __ser_on(self, args):
        l = 'call ' + self.methods['on'] + '()\n'
        self.__calls.add(self.methods['on'])
        self.__line = l

    # OFF directive
    def __ser_off(self, args):
        l = 'call ' + self.methods['off'] + '()\n'
        self.__calls.add(self.methods['off'])
        self.__line = l

    # LINE: module/program
    def __re_module(self):
        r = re.compile('^ *(?P<statement>module|program) +(?P<identifier>[a-z][a-z0-9_]*)', re.IGNORECASE)
        m = r.search(self.__line)

        if m:
            if m.group('identifier').upper() == 'PROCEDURE':
                return False
            if self.__module:
                self.__exit_error(msg='Unexpected ' + m.group(1) + ' statement')
            self.__produce_use_stmt()
            if m.group('statement').upper() == 'MODULE':
                self.__use_stmt_in_module = True
            self.__module = m.group('identifier')
        return m

    # LINE: subroutine or function
    def __re_subroutine_function(self):
        if self.__use_stmt_in_module:  # Statement produced at module level
            return
        r = re.compile('^ *(subroutine|function).*', re.IGNORECASE)
        r_cont = re.compile('^ *(subroutine|function)([^!]*)&', re.IGNORECASE)
        m = r.search(self.__line)
        m_cont = r_cont.search(self.__line)
        if m and not m_cont:
            self.__produce_use_stmt()
        elif m and m_cont: 
            # look ahead to find the correct line to insert the use statement
            lookahead_index = self.__linenum
            # set to line after the subroutine/function declaration
            lookahead_index += 1
            # look ahead
            nextline = linecache.getline(os.path.join(self.infile), lookahead_index)
            r_continued_line = re.compile('^([^!]*)&', re.IGNORECASE)
            false_skip = 0
            if nextline == self.__line:
                lookahead_index += 1
                nextline = linecache.getline(os.path.join(self.infile), lookahead_index)
                false_skip = 1
            while r_continued_line.search(nextline):
                self.__line += nextline 
                lookahead_index += 1
                nextline = linecache.getline(os.path.join(self.infile), lookahead_index)
            self.__line += nextline
            self.__skip_next_n_lines = lookahead_index - self.__linenum - false_skip
            self.__produce_use_stmt()
        return m

    # LINE: !$SER directive
    def __re_ser(self):
        r1 = re.compile('^ *!\$ser *(.*)$', re.IGNORECASE)
        r2 = re.compile(r'''((?:[^ "']|"[^"]*"|'[^']*')+)''', re.IGNORECASE)
        m = r1.search(self.__line)
        if m:
            if m.group(1):
                args = r2.split(m.group(1))[1::2]
                if args[0].upper() in self.language['init']:
                    self.__ser_init(args)
                elif args[0].upper() in self.language['option']:
                    self.__ser_option(args)
                elif args[0].upper() in self.language['metainfo']:
                    self.__ser_metainfo(args)
                elif args[0].upper() in self.language['verbatim']:
                    self.__ser_verbatim(args)
                elif args[0].upper() in self.language['register']:
                    self.__ser_register(args)
                elif args[0].upper() in self.language['savepoint']:
                    self.__ser_savepoint(args)
                elif args[0].upper() in self.language['zero']:
                    self.__ser_zero(args)
                elif args[0].upper() in self.language['accdata']:
                    self.__ser_data(args, True)
                elif args[0].upper() in self.language['data']:
                    self.__ser_data(args)
                elif args[0].upper() in self.language['tracer']:
                    self.__ser_tracer(args)
                elif args[0].upper() in self.language['registertracers']:
                    self.__ser_registertracers(args)
                elif args[0].upper() in self.language['cleanup']:
                    self.__ser_cleanup(args)
                elif args[0].upper() in self.language['on']:
                    self.__ser_on(args)
                elif args[0].upper() in self.language['off']:
                    self.__ser_off(args)
                elif args[0].upper() in self.language['mode']:
                    self.__ser_mode(args)
                else:
                    self.__exit_error(directive=args[0],
                                      msg='Unknown directive encountered')
        return m

    # LINE: end module/end program
    def __re_endmodule(self):
        r = re.compile('^ *end *(module|program) *([a-z][a-z0-9_]*|)', re.IGNORECASE)
        m = r.search(self.__line)
        if m:
            if not self.__module:
                self.__exit_error(msg='Unexpected "end '+m.group(1)+'" statement')
            if m.group(2) and self.__module.lower() != m.group(2).lower():
                self.__exit_error(msg='Was expecting "end '+m.group(1)+' '+self.__module+'"')
            self.__module = ''
            self.__use_stmt_in_module = False
        return m

    def __check_intent_in(self, line):
        lhs = re.sub(r'!.*', '', line)  # Remove comments at end of the line
        var_with_dim = [x.strip().replace(' ', '') for x in re.split(r',(?![^(]*\))', lhs)]
        var = [re.sub(r'\(.*?\)', '', x) for x in var_with_dim]
        fields_in_this_line = [x for x in self.intentin_to_remove if x in var]
        self.intentin_removed.extend([x for x in fields_in_this_line if x not in self.intentin_removed])

        if fields_in_this_line:
            l = '#ifdef ' + self.ifdef + '\n'
            r = re.compile(r', *intent *\(in\)', re.IGNORECASE)
            l += r.sub('', self.__line)
            l += '#else\n' + self.__line + '#endif\n'
            self.__line = l
        return fields_in_this_line

    def __re_def(self):
        r = re.compile(r'.*intent *\(in\)[^:]*::\s*([^!]*)\s*.*', re.IGNORECASE)
        r_cont = re.compile(r'.*intent *\(in\)[^:]*::\s*([^!]*)\s*.*&', re.IGNORECASE)

        # Line contains intent with continuation
        m_cont = r_cont.search(self.__line)
        m = r.search(self.__line)
        if m_cont:
            splitted = self.__line.split('::')
            splitted[1] = re.sub(r'!.*', '', splitted[1])  # Remove comments at end of the line
            if not self.__check_intent_in(splitted[1]):
                # look ahead to find the variable
                lookahead_index = self.__linenum
                # set to line after the intent declaration
                lookahead_index += 1
                # look ahead
                nextline = linecache.getline(os.path.join(self.infile), lookahead_index)
                while nextline:
                    self.__check_intent_in(nextline)
                    if nextline.find('&') != -1:
                        lookahead_index += 1
                        nextline = linecache.getline(os.path.join(self.infile), lookahead_index)
                    else:
                        nextline = None

        # Match a standard declaration with variable and intent on the same line
        elif m:
            splitted = self.__line.split('::')
            splitted[1] = re.sub(r'!.*', '', splitted[1])  # Remove comments at end of the line
            self.__check_intent_in(splitted[1])
        return m

    # Produce the USE statement and append it to l
    def __produce_use_stmt(self):
        if self.__use_stmt_in_module is True:
            return
        calls_pp = [c for c in self.__calls if c.startswith('ppser')]
        calls_fs = [c for c in self.__calls if not c.startswith('ppser')]
        ncalls = len(calls_pp) + len(calls_fs)
        if ncalls > 0:
            calls_pp += ['ppser_savepoint', 'ppser_serializer', 'ppser_serializer_ref',
                         'ppser_intlength', 'ppser_reallength', 'ppser_realtype', 'ppser_zrperturb']
            self.__line += '\n'
            if self.ifdef:
                self.__line += '#ifdef ' + self.ifdef + '\n'
            if len(calls_fs) > 0:
                self.__line += 'USE ' + self.module + ', ONLY: &\n'
                for s in calls_fs[:-1]:
                    self.__line += '  ' + s + ', &\n'
                self.__line += '  ' + calls_fs[-1] + '\n'
            if len(calls_pp) > 0:
                self.__line += 'USE utils_ppser, ONLY:  &\n'
                for s in calls_pp[:-1]:
                    self.__line += '  ' + s + ', &\n'
                self.__line += '  ' + calls_pp[-1] + '\n'

            if len(self.__extra_module) > 0:
                for mod in self.__extra_module:
                    self.__line += 'USE ' + mod + '\n'

            if self.ifdef:
                self.__line += '#endif\n'
            self.__line += '\n'

    # evaluate one line
    def lexer(self, final=False):

        # parse lines related to scope
        self.__re_module()
        self.__re_subroutine_function()
        self.__re_endmodule()
        self.__re_def()

        # parse !$SER lines
        if self.__re_ser():
            # if this is the first line with !$SER statements, add #ifdef
            if self.ifdef and not self.__ser:
                self.__line = '#ifdef ' + self.ifdef + '\n' + self.__line
                self.__ser = True
        else:
            # if this is the first line without !$SER statements, add #endif
            if self.ifdef and self.__ser:
                self.__line = '#endif\n' + self.__line
                self.__ser = False

        if final:
            # final call, check consistency
            if self.__ser:
                self.__exit_error(msg='Unterminated #ifdef ' + self.ifdef + ' encountered')
            if self.__module:
                self.__exit_error(msg='Unterminated module or program unit encountered')

    # execute one parsing pass over file
    def parse(self, generate=False):
        # if generate == False we only analyse the file

        # reset flags (which define state of parser)
        self.__ser = False        # currently processing !$SER directives
        self.__line = ''          # current line
        self.__linenum = 0        # current line number
        self.__module = ''        # current module
        self.__outputBuffer = ''  # preprocessed file

        # generate preprocessing macro for ACC_PREFIX
        if self.acc_prefix:
            self.__outputBuffer += '#define ACC_PREFIX !$acc\n'

        # open and parse file
        input_file = open(os.path.join(self.infile), 'r')
        try:
            self.line = ''
            for line in input_file:
                if(self.__skip_next_n_lines > 0):
                    self.__skip_next_n_lines -= 1
                    continue
                # handle line continuation (next line coming in)
                if self.__line:
                    if re.match('^ *!\$ser& ', line, re.IGNORECASE):
                        line = re.sub('^ *!\$ser& *', ' ', line, re.IGNORECASE)
                    else:
                        self.__exit_error(msg='Incorrect line continuation encountered')
                self.__line += line
                self.__linenum += 1
                # handle line continuation (continued line going out)
                if re.match('^ *!\$ser *(.*) & *$', self.__line, re.IGNORECASE):
                    # chop trailing &
                    self.__line = re.sub(' +& *$', '', self.__line, re.IGNORECASE)
                    self.__line = self.__line.rstrip()
                    continue
                # parse line
                self.lexer()
                if generate:
                    self.__outputBuffer += self.__line
                # cleanup current line (used for line continuation and final lexer call)
                self.__line = ''
            self.lexer(final=True)

        finally:
            input_file.close()

    # main processing method
    def preprocess(self):
        # parse file
        self.parse()                # first pass, analyse only
        self.parse(generate=True)   # second pass, preprocess
        # write output
        if self.outfile != '':
            output_file = tempfile.NamedTemporaryFile(delete=False)
            output_file.write(to_ascii(self.__outputBuffer))
            output_file.close()
            useit = True
            if os.path.isfile(self.outfile) and not self.identical:
                if filecmp.cmp(self.outfile, output_file.name):
                    useit = False
            if useit:
                try:
                    os.rename(output_file.name, self.outfile)
                except:
                    shutil.move(output_file.name, self.outfile)
            else:
                os.remove(output_file.name)
        else:
            print(self.__outputBuffer)


def simple_test():
    try:
        test = """
module test
implicit none

!$SER VERBATIM CHARACTER (LEN=6) :: fs_realtype

!$SER INIT singlefile=.true.
!$SER ZERO a b c d
!$SER SAVEPOINT gugus
!$SER SAVEPOINT DycoreUnittest.DoStep-in LargeTimeStep=ntstep Test=Blabla IF ntstep>0
! this is a comment
!$SER DATA u=u(:,:,:,nnow)
!$SER DATA v=v_in(:,:,:)+v_ref(:,:,:,nnow) IF allocated(v_in)
!$SER DATA test='  gugjs is here ' IF a==' this is a test '
!$SER DATA nsmsteps=REAL(nsmsteps,ireals)
!$SER DATA u=u(:,:,:,nnew) u_nnow=u(:,:,:,nnow) v=v(:,:,:,nnew) v_nnow=v(:,:,:,nnow) IF ntstep>0

!$SER VERBATIM ! REAL field type
!$SER VERBATIM SELECT CASE (ireals)
!$SER VERBATIM   CASE (ireals4) ; fs_realtype = 'float'
!$SER VERBATIM   CASE (ireals8) ; fs_realtype = 'double'
!$SER VERBATIM END SELECT

!$SER REG u fs_realtype IJK
!$SER REGISTER u          fs_realtype ie je ke  1 nboundlines nboundlines nboundlines nboundlines 0 0 0 0
!$SER REG w fs_realtype IJK1
!$SER REGISTER w          fs_realtype ie je ke1 1 nboundlines nboundlines nboundlines nboundlines 0 1 0 0
!$SER REG cpollen2_s fs_realtype IJ
!$SER REGISTER cpollen2_s fs_realtype ie je 1   1 nboundlines nboundlines nboundlines nboundlines 0 0 0 0
!$SER REGISTER dts fs_realtype
!$SER REGISTER nlastbound 'integer' 1
!$SER REG wgtfacq fs_realtype IJ3
!$SER REGISTER wgtfacq fs_realtype ie je 3 1 nboundlines nboundlines nboundlines nboundlines 0 0 0 0
!$SER REG vcoord fs_realtype K1
!$SER REGISTER vcoord fs_realtype KSize=ke1 KPlusHalo=1
!$SER REG crlat fs_realtype J2
!$SER REGISTER crlat fs_realtype JSize=je JMinusHalo=nboundlines JPlusHalo=nboundlines KSize=2
!$SER REG tgrlat J
!$SER REGISTER tgrlat fs_realtype JSize=je JMinusHalo=nboundlines JPlusHalo=nboundlines
!$SER REG a1t fs_realtype K1
!$SER REGISTER a1t fs_realtype KSize=ke1 KPlusHalo=1
!$SER REG lwest_lbdz 'integer' IJ
!$SER REGISTER lwest_lbdz 'integer' ie je 1 1 nboundlines nboundlines nboundlines nboundlines 0 0 0 0
!$SER DATA lwest_lbdz=merge(1,0,lwest_lbdz)

!$SER VERBATIM ! recalculate bottom boundary condition for serialization
!$SER VERBATIM CALL w_bbc_var(zuhl(:,:,ke1), zvhl(:,:,ke1), zphl(:,:,:), zfx, zfyd)
!$SER DATA wbbc=zphl(:,:,ke1)

! check line continuation
!$ser data gugu=dada &
!$ser&     test=igi dede=a+3+f &
!$ser&     check=in

!$SER VERBATIM #ifdef POLLEN
!$SER VERBATIM IF (ALLOCATED(cpollen))
!$SER DATA cpollen1=cpollen(:,:,:,1,nnew) IF isp_pollen>0
!$SER DATA cpollen2=cpollen(:,:,:,2,nnew) IF isp_pollen>1
!$SER VERBATIM ENDIF
!$SER VERBATIM #endif

!$SER CLEANUP
!$SER

end module test
"""
        f = tempfile.NamedTemporaryFile(delete=False)
        f.write(test)
        f.close()
        ser = PpSer(f.name)
        PpSer.real = 'wp'
        ser.preprocess()
    finally:
        os.remove(f.name)


def parse_args():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option('-i', '--ignore-identical', help='Ignore files which are not modified by pre-processor',
                      default=False, action='store_true', dest='ignore_identical')
    parser.add_option('-d', '--output-dir', help='The target directory for writing pre-processed files',
                      default='', type=str, dest='output_dir')
    parser.add_option('-o', '--output', help='Output file name to preprocess single file',
                      default='', type=str, dest='output_file')
    parser.add_option('-r', '--recursive', help='Recursively process target directory and mirror tree',
                      default=False, action='store_true', dest='recursive')
    parser.add_option('-v', '--verbose', help='Enable verbose execution',
                      default=False, action='store_true', dest='verbose')
    parser.add_option('-p', '--no-prefix', help='Don\'t generate preprocessing macro definition for ACC_PREFIX',
                      default=True, action='store_false', dest='acc_prefix')
    parser.add_option('-a', '--acc-if', help='Add IF clause to OpenACC update statement',
                      default='', type=str, dest='acc_if')
    parser.add_option('-m', '--module', help='Extra MODULE to be add to the use statement',
                      default='', type=str, dest='modules')
    (options, args) = parser.parse_args()
    if len(args) < 1:
        parser.error('Need at least one source file to process')
    if options.output_file and len(args) > 1:
        parser.error('Single source file required if output file is given')
    if options.recursive:
        if not options.output_dir:
            parser.error('Output directory is required with recursive option')
        for indir in args:
            if not os.path.isdir(indir):
                parser.error('Arguments need to be directories with recursive option')
    return options, args

if __name__ == "__main__":
    (options, args) = parse_args()
    if options.recursive:
        file_list = []
        for indir in args:
            build_tree(indir, options.output_dir, file_list, filter_fortran)
        args = file_list

    for infile in args:
        if options.output_dir:
            if options.recursive:
                outfile = os.path.join(options.output_dir,
                                       os.path.sep.join([p for p in os.path.dirname(infile).rsplit(os.path.sep) if p][1:]),
                                       os.path.basename(infile))
            else:
                outfile = os.path.join(options.output_dir, os.path.basename(infile))
        elif options.output_file:
            outfile = options.output_file
        else:
            outfile = ''

        # If output is to a file and the file is more updated than the input, skip
        if os.path.exists(outfile) and os.path.getctime(outfile) > os.path.getctime(infile):
            print('Skipping', infile)
        else:
            print('Processing file', infile)
            ser = PpSer(infile, real='wp', outfile=outfile, identical=(not options.ignore_identical),
                        verbose=options.verbose, acc_prefix=options.acc_prefix, acc_if=options.acc_if,
                        modules=options.modules)
            ser.preprocess()
