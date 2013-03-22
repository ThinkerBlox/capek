#!/usr/bin/env python
# Capek Main

#   Copyright 2013 David B. Curtis

#   This file is part of Capek.
#   
#   Capek is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#   
#   Capek is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with Capek.  If not, see <http://www.gnu.org/licenses/>.


import argparse

def process_arguments():
    argp = argparse.ArgumentParser(description='Capek compiler')

    argp.add_argument('sourcefile',type=str, help='Name of a .ck file.')
    argp.add_argument('--pretty', action='store_true', help='Pretty print on stdout.')
    argp.add_argument('-c', metavar='output.c', help='Create output ANSI .c file.')
    argp.add_argument('-nxc', metavar='output.c', type=str, help='Create output .nxc file.')
    argp.add_argument('-py',metavar='output.py', type=str, help='Create output Python file.')
    argp.add_argument('--debug', metavar='alpst', type=str, help='Debug switches.')
    argp.add_argument('-v', action='count', help='Debug verbosity knob.')

    args = vars(argp.parse_args())
    # Make sure output files don't clobber the input or each other.
    files = set([])
    files.add(args['sourcefile'])
    check = ['c','py','nxc']
    for arg in check:
        if args[arg]:
            if args[arg] in files:
                argp.error('File naming conflict.')
            else:
                files.add(args[arg])
    
    # Fake up an output file argument:
    args['compiledoutput'] = args['sourcefile'] + '.i'
    
    # All OK.
    return args

#########################################################################
args = process_arguments()

if args['debug'] and 'a' in args['debug']:
    print args

import capekParser as parser
import capekCodeGen as cg
import sys
import cPickle as pkl

p_log = None # Assign a logger to turn on parser logging. 

inputFile = open(args['sourcefile']).read()

if args['debug']:
    import logging
    if 'p' in args['debug']:
        parseLogName = 'parse.log'
        # erase the old log
        f = open(parseLogName,'w')
        f.close() 
        logging.basicConfig(filename=parseLogName,level=logging.DEBUG)
        p_log = logging.getLogger()

if p_log:
    print 'logging the parse'
    cu_list = parser.capekParser.parse(inputFile, debug = p_log)
else: 
    cu_list = parser.capekParser.parse(inputFile)

if parser.errorCount > 0:
    print parser.errorCount, ' Syntax error(s).  Compilation stopped.'
    sys.exit(1)

outputFile = open(args['compiledoutput'],'wb')
pkl.dump(cu_list, outputFile, 2)
outputFile.close()

if args['debug'] and 't' in args['debug']:
    print cu_list

if args['pretty']:
    sink = cg.CapekCodeSink(sys.stdout)
    for cu in cu_list:
        cu.pretty(sink)

