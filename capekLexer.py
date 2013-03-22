# lexer for Capek

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

#
# To Do:
# ____ Complete the set of arithmetic operators. (include compare and bool)
# ____ Add unittest function
# ____ add way to comment out code #- <buildswitch> #% #+
# ____ Add newline count as a value to NEWLINE

import ply.lex as lex
import capekASTNodes as st

# Lexer states:
#  inline: Collecting inline text. No lexing/parsing done at all. 
#  inlineparam: Collect parameters to the inline funciton.
#  pragmaparam: Collect parameters to the pragma function.
#  foreignparam: Collect parameters that go with the 'foreign' keyword. 
#  multidedent: Issue DEDENT tokens while unwinding a multi-level dedent.
states= ( 
            ('inline','exclusive'),
            ('inlineparam','exclusive'),
            ('foreignparam','exclusive'),
            ('pragmaparam', 'exclusive'),
            ('multidedent', 'exclusive'),
)

tokens = (
    'ACTUATOR',
    'AS',
    'ATOMIC',
    'BEHAVIOR',
    'CLASS',
    'COMMENT',
    'COMMENT_DOC',
    'CONST',
    'DEDENT',
    'DEDENT_ERROR',
    'DEVICE',
    'ELIF',
    'ELSE',
    'EVERY',
    'EXCEPT',
    'FINALLY',
    'FOREIGN',
    'FUNCTION',
    'HASH',
    'ID',
    'IF',
    'IMPORT',
    'INDENT',
    'INLINE',
    'INLINE_END',
    'INIT',
    'INTERRUPT',
    'ITER',
    'LIBRARY',
    'LINE_IMAGE',
    'LIST',
    'LOAD',
    'MIXING',
    'MODE',
    'NEWLINE',
    'NUMBER',
    'POLL',
    'POOL',
    'PRAGMA',
    'PRIVATE',
    'RESUME',
    'RETURN',
    'REVERSE',
    'SELECT',
    'SENSOR',
    'SHARE',
    'START',
    'STRLIT', 
    'SUPPRESS',
    'STRUCT',
    'TRY',
    'TYPE',
    'UNION',
    'UNIT_TERMINATOR',
    'VAR',
    'WHILE',
    'LPAR', 'RPAR',
    'LBRKT', 'RBRKT',
    'PLUS_EQ', 'MINUS_EQ',
    'LE', 'GE', 'EQ_EQ', 'NEQ',
)


reserved = {
    'actuator': 'ACTUATOR',
    'as' : 'AS',
    'atomic' : 'ATOMIC',
    'behavior' : 'BEHAVIOR',
    'class' : 'CLASS',
    'const' : 'CONST',
    'device' : 'DEVICE',
    'elif' : 'ELIF',
    'else' : 'ELSE',
    'every' : 'EVERY',
    'except' : 'EXCEPT',
    'finally' : 'FINALLY',
    'function' : 'FUNCTION',
    'global' : 'GLOBAL',
    'if' : 'IF',
    'init' : 'INIT',
    'import' : 'IMPORT',
    'interrupt' : 'INTERRUPT',
    'iter' : 'ITER',
    'library' : 'LIBRARY',
    'load' : 'LOAD',
    'mixing' : 'MIXING',
    'mode' : 'MODE',
    'poll' : 'POLL',
    'pool' : 'POOL',
    'private' : 'PRIVATE',
    'resume' : 'RESUME',
    'return' : 'RETURN',
    'reverse' : 'REVERSE',
    'select' : 'SELECT',
    'sensor' : 'SENSOR',
    'share' : 'SHARE',
    'start' : 'START',
    'suppress' : 'SUPPRESS',
    'struct' : 'STRUCT',
    'try' : 'TRY',
    'type' : 'TYPE',
    'union' : 'UNION',
    'var' : 'VAR',
    'while' : 'WHILE',
}

brackets = {
    '(' : 'LPAR',
    ')' : 'RPAR',
    '[' : 'LBRKT',
    ']' : 'RBRKT',
}

def strconv(s):
    "Process string escapes."
    # FIXME
    # \n \t \xDD \\ \"
    return s

# Identifiers and Keywords
def t_ID(t):    
    r'([a-zA-Z_][a-zA-Z_0-9]*)|(\$)(\$)?'
    # This pattern recognizes normal identifiers, keywords,
    # and the special $ and $$ (self and super) identifiers.
    # The keywords 'inline', 'foreign', and 'pragma' kick off
    # alternative lexer states.  The balance of the keywords
    # are filtered out using a dictionary, and anything left
    # is presumed to be an identifier.
    if t.value == 'inline':
        t.lexer.begin('inlineparam')
    elif t.value == 'foreign':
        t.lexer.begin('foreignparam')
    elif t.value == 'pragma':
        t.lexer.begin('pragmaparam')
    else:
        t.type = reserved.get(t.value, 'ID')
        if t.type == 'ID':
            t.value = st.ND_id(t.value)
        return t    

# Numeric Literals
# Floats.  They look like this:
# 1.0 1.0e5 1.0e+5 1.0e-5 1e-5 1e5 1e+5 .5 .5e2 5. 
def t_NUMBER_float(t):
    r'(([0-9]*\.[0-9]+)|([0-9]+\.[0-9]*))(e[+-]?[0-9]+)?'
    t.value = st.ND_literal('float', float(t.value), t.value)
    t.type = 'NUMBER'
    return t

# Hex integers.
# 0x12aF
def t_NUMBER_hex(t):
    r'0x[0-9a-fA-F]+'
    t.value = st.ND_literal('int32', int(t.value,16), t.value)
    t.type = 'NUMBER'
    return t

# Decimal integers
def t_NUMBER_dec(t):
    r'[0-9]+'
    t.value = st.ND_literal('int32', int(t.value), t.value)
    t.type = 'NUMBER'
    return t

# Sring Literals
def t_STRLIT(t):
    r'(\")([^"]|(\\\"))*(\")'
    t.value = st.ND_literal('str', strconv(t.value), t.value) 
    return t


# Groups bracketed by () [] or {} do not emit INDENT or
# DEDENT tokens.  Instead, multiple newlines, tabs, and spaces
# are compressed into a single NEWLINE token. 
# INDENT/DEDENT suppression is controlled by groupNest, which 
# is adjusted here.
def t_group_open(t):
    r'[\(\[\{]'
    t.lexer.groupNest += 1
    t.type = brackets[t.value]
    return t

def t_group_close(t):
    r'[\)\]\}]'
    t.lexer.groupNest = max(t.lexer.groupNest - 1, 0)
    t.type = brackets[t.value]
    return t

# Absorb a white line.  This has the effect of suppressing
# redundant NEWLINE tokens, simplifying the grammar.
# It also keeps extraneous tabs and spaces on all-white-
# space lines from being considered signficant indentation.
def t_white_line(t):
    r'(\n)+((\t)|(\ ))*(?=(\n))'
    # Absorb white, non-comment lines.  Leave the trailing
    # newline to trigger other newline rules.
    # Update tne lineno.
    t.lexer.lineno += t.value.count('\n')

# Comments have two flavors: ordinary comments start with
# a single # character.  Comments intended to be automatically
# extracted as documentation start with the #| digraph.
# Leading white space in a comment line is preserved, with
# the exception that if there is not a leading space character
# one will be forced.
def t_COMMENT_DOC(t):
    r'\#\|.*'
    t.value = t.value[2:].rstrip()
    if t.value and t.value[0] != ' ':
        t.value = ' ' + t.value 
    return t

def t_COMMENT(t):
    r'\#.*'
    t.value = t.value[1:].rstrip()
    if t.value and t.value[0] != ' ':
        t.value = ' ' + t.value 
    return t

# White space within a line of code is insignificant and
# is absorbed here. 
def t_white_within_line(t):
    r'((\t)|(\ ))+'
    pass

# Escaped newlines are absorbed like white space, and then
# the white-space-within-line rule absorbs white space that
# would otherwise be recognized as indentation.
def t_escaped_newline(t):
    r'(\\)(\n)'
    t.lexer.lineno += 1
    
# Indent - Dedent - Newline
#
# Count spaces after newline and compute indentation,
# return INDENT or DEDENT or NEWLINE as appropriate.
# When groupNest != 0, INDENT and DEDENT are suppressed, but
# NEWLINE is passed through.
# 
# This pattern depends on the pattern in t_white_line
# to suppress redundant NEWLINES and eliminate insignficant
# all-white-space lines. The only white space that is 
# significant is between a newline and the first 
# non-white-space charcater on a line.
#
# Erroneous dedents are a problem case.  After recongizing
# a reduction in indentation, if the new effective indentation
# level does not line up with a previous indentation level then
# a DEDENT_ERROR token is emitted.
#
# It is also possible for the reduction in indentation to indicate 
# multiple levels of dedent.  In this case the lexer needs to emit 
# multiple DEDENT tokens. The'multidedent' start state unwinds
# the indentation counter. In order to have something to match
# against, the lexer is backed up by one character every iteration
# of the multiple-dedent unwind. Yes, after 30 years of lexing I 
# was finally forced to 'unput'. Drat.
def t_newline_indent_dedent(t):
    r'(\n)+((\ )|(\t))*'
    # Expand tabs into spaces
    t.value.expandtabs(t.lexer.spacesPerTab)
    # Strip off leading newlines. 
    # Set numSpc to the number of spaces, which is the indent
    # of this line.  numNL is the number of newlines chars matched.
    # Update lineno with the number of newlines stripped off.
    numSpc = len(t.value.lstrip('\n'))
    numNL = len(t.value) - numSpc
    t.lexer.lineno += numNL
    if t.lexer.groupNest == 0:
        # No nested paren, brackets, or braces.
        if numSpc == t.lexer.indentStack[0]:
            # No change in indentation.
            t.type = 'NEWLINE'
            t.value = numNL
        else:
            # Indentation changed.
            if numSpc > t.lexer.indentStack[0]:
                # Indent. Push new indent level onto the stack.
                t.lexer.indentStack.insert(0,numSpc)
                t.type = 'INDENT'
            else:
                # Dedent might back out multiple indents
                # or not align with a valid indentation level.
                t.lexer.indentStack.pop(0)
                if numSpc == t.lexer.indentStack[0]:
                    # Single, correct dedent.
                    t.type = 'DEDENT'
                else:
                    if numSpc in t.lexer.indentStack:
                        # Valid multiple dedent.
                        t.lexer.begin('multidedent') 
                        t.lexer.dedentCountdown = \
                            t.lexer.indentStack.index(numSpc)
                        # Back up by one character so the dedent
                        # unwinder has a character to match against.
                        # the character is guaranteed to be a space,
                        # tab, or newline.
                        t.lexer.lexpos -= 1
                        t.type = 'DEDENT'
                    else:
                        # Bogus dedent. Peel that stack back to 
                        # somewhere close to the right level, and
                        # punt a DEDENT_ERROR token into the parser. 
                        # Parser syntax error handling will generate
                        # an error message.
                        while numSpc < indentStack[0]:
                            t.lexer.indentStack.pop(0)
                        t.type = 'DEDENT_ERROR'
    else:
        # Under group nesting indentation is ignored,
        # but newlines are passed through.
        t.type = 'NEWLINE'
        t.value = numNL
    return t   

# Multiple DEDENT unwinder.
# The lexpos is guaranteed to be pointing at a space, tab, or
# newline.  Match one and emit a DEDENT. If not fully unwound,
# back up one character and stay in multidedent state.
def t_multidedent_DEDENT(t):
    r'((\ )|(\t)|(\n))'
    t.lexer.indentStack.pop(0)
    t.lexer.dedentCountdown -= 1
    if t.lexer.dedentCountdown == 0:
        # Fully unwound.
        t.lexer.begin('INITIAL')
    else:
        # Back up for another step.
        t.lexer.lexpos -= 1
    t.type = 'DEDENT'    
    return t
    
# Digraphs
def t_digraph_plus_eq(t):
    r'\+\='
    t.type = 'PLUS_EQ'
    return t

def t_digraph_minus_eq(t):
    r'\-\='
    t.type = 'MINUS_EQ'
    return t

def t_digraph_le(t):
    r'\<\='
    t.type = 'LE'
    return t

def t_digraph_ge(t):
    r'\>\='
    t.type = 'GE'
    return t

def t_digraph_eq_eq(t):
    r'\=\='
    t.type = 'EQ_EQ'
    return t

def t_digraph_neq(t):
    r'\!\='
    t.type = 'NEQ'
    return t

def t_unit_terminator(t):
    r'\@\@'
    t.type = 'UNIT_TERMINATOR'
    return t

# Literals
literals = ':=+-*/.,<>@'

# Error handling rule
def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

####################################################################
#
# Inline, foreign, and pragma states
#    
# Collect the rest of the line right after the INLINE keyword.
# Split the line and return the results a parameters to the inline.
# Enter the 'inline' state where the actual inline text is collected.
def t_inlineparam_INLINE(t):
    r'.*\n'
    t.lexer.lineno += 1
    params = t.value.strip(': \t\n').split()
    global inlineSentinel 
    inlineSentinel = params[-1]
    t.lexer.begin('inline')
    t.value = params
    t.type = 'INLINE'
    return t

# Collect all text in 'inline' mode.
# The last word on the line containing the inline statement is
# the sentinel that exits the 'inline' lexical state.
def t_inline_passedline(t):
    r'.*\n'
    t.lexer.lineno += 1
    ln = t.value.rstrip('\n')
    if ln == inlineSentinel:
        t.lexer.begin('INITIAL')
        t.type = 'INLINE_END'
        t.value = 1
        t.lexer.lexpos -= 1 # Push '\n' back so INDENT?DEDENT can catch it.
    else:
        t.type = 'LINE_IMAGE'
        t.value = ln
    return t

# Collect the rest of the line after the FOREIGN token.
# Split the line and return the results as parameters.
def t_foreignparam_FOREIGN(t):
    r'(\ ).*'
    t.lexer.lineno += 1
    params = t.value.rstrip(': \t').split()
    t.value = params
    t.type = 'FOREIGN'
    t.lexer.begin('INITIAL')
    return t

# Collect the rest of the line after the PRAGMA token.
# Split the line and return the results as pragma parameters.
def t_pragmaparam_PRAGMA(t):
    r'(\ ).*'
    t.lexer.lineno += 1
    params = t.value.rstrip(': \t').split()
    t.value = params
    t.type = 'PRAGMA'
    t.lexer.begin('INITIAL')
    return t

def t_inline_foreignparam_pragmaparam_inlineparam_error(t):
    pass

def t_multidedent_error(t):
    pass

#########################################################
#
# Create lexer 
capekLexer = lex.lex()

# Added state
# 1. State to keep track of current indentation level.
#    indentStack contains the the number of leading
#    spaces for the current indentation level history.
#    One stack entry per indentation level.
#    dedentCountdown is used by the dedent unwinder
#    to keep track of the number of DEDENT tokens yet
#    to be issued.
capekLexer.indentStack = [0]
capekLexer.dedentCountdown = 0

# 2. State to keep track of () [] {} nesting level.
capekLexer.groupNest = 0

# 3. State to set the default tab-to-space conversion factor
#    for indent.
capekLexer.spacesPerTab = 8


#########################################################

if __name__ == '__main__':
    print 'tokens=', tokens
    print 'literals=', literals
    print 'reserved=', reserved
    print '---'
    
    testdata = open('lexertest.txt').read()
    
    # Give the lexer some input
    capekLexer.input(testdata)

    # Tokenize
    while True:
        tok = capekLexer.token()
        if not tok: break      # No more input
        print tok


