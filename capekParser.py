# Capek Parser

#   Copyright 2013 ThinkerBlox, LLC

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


# To do:
# done Add MIXING actuators and EXCLUSIVE attachment
#        of mixing actuators
# done What are 'acquire' and 'release' for?  Isn't 'with'
#        all that is necessary? 
# done top level grammar for compilation units
# done type, consontant, and import section
# done detailed type decl grammar
# done detailed var decl grammar, with initializers
# done some kind of 'inline' code pass-through
# done Add optional type declaration to constant.
#        Allow:
#      const int16 foo = exp
#      const bar = exp
#      const int16 bar16 = bar
#      const int8 bar8 = bar
# ____ boolean expression
# ____ shift expressions
# ____ bitwise expressions
# ____ object instantiation
# ____ class definition
#        This may lead to need for CLASS VAR and CLASS FUNCTION
#        definitions, with semantic checks to clobber where they
#        do not make sense.
# done Add exception try..except..finally blocks
# done Add asynrchronous/interrupt functions
# done Complete the ASTNode for interrupt code
# ____ Document the semantics of interrupt handlers, esp w.r.t.
#       spin locks, which can NOT be aquired in ATOMIC code sections.
# done There needs to be a way to do atomic code under the global
#       interrupt disable. (Simply omit semaphore, this implies that
#       global interrupts are disabled.)
# done Make foreign look more like pragma
# done Handle the DEDENT_ERROR token. It should invoke syntax
#       error handling of some kind.  
# ____ Are precedences needed any more on COMMENT and NEWLINE since
#      the comment_block cruft has been removed?
# done Look for places where ':' does lead to INDENT -- that is
#       very inconsistent.
# ____ think about how to handle error counting for multipel
#       compilation units.  Perhaps put in error counters in the
#       top level cu object, and clear the running syntax count
#       in the compiliation_unit_list reduction.
# ____ consider a more restrictive comment grammar that fits with a
#       Scratch-like inteface.



import ply.yacc as yacc
from capekLexer import tokens, literals, capekLexer
import capekASTNodes as st

#################################################################
def errorMessage(s):
    print s
    
#################################################################
#
# Parser Definitions
#
precedence = (
    ('left', 'COMMENT'),
    ('left', 'NEWLINE'),
    ('left', '<', '>', 'LE', 'GE', 'EQ_EQ', 'NEQ'),
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('right', 'UMINUS'),
)

# ################################################################
#
# Start symbol 'compilation' contains the list of compilation unit
# results at the completion of the parse.
#
def p_compilation(p):
    "compilation : startup compilation_unit_list"
    p[0] = p[2]

def p_compilation_unit_list(p):
    '''compilation_unit_list :  compilation_unit
        | compilation_unit_list compilation_unit '''
    # Simple list of ND_compilation
    if len(p) > 2:
        p[0] = p[1]
        p[1].append(p[2])
    else:
        p[0] = [p[1]]

def p_compilation_unit(p):
    'compilation_unit : opt_imports comp_unit_body opt_newline opt_unit_terminator '
    p[0] = p[2]    
    
def p_startup(p):
    'startup : opt_newline'
    # Do required initialization here.
    pass

def p_opt_unit_terminator(p):
    '''opt_unit_terminator : UNIT_TERMINATOR
        | empty '''
    p[0] = p[1]

# ##########################################################
#
# Imports
#
def p_opt_imports(p):
    '''opt_imports : import_list
        | empty '''
    p[0] = p[1]

def p_import_list_recurse(p):
    '''import_list : import_list import_block
        | import_list comment NEWLINE '''
    p[0] = st.ndlappend(p[1], p[2])

def p_import_list_induce(p):
    '''import_list : import_block 
        | comment NEWLINE '''
    p[0] = [p[1]]

# #############################################################
#
# Compiliation unit bodies
#
def p_comp_unit_body_mode(p):
    '''comp_unit_body : MODE ID formal_params NEWLINE mode_body '''
    p[0] = st.ND_mode(p[3], p[5][0], p[5][1], p[5][2]) 

def p_comp_unit_body_lib(p):
    '''comp_unit_body : LIBRARY ID formal_params NEWLINE definitions init_block '''
    p[0] = st.ND_library(st.LN(p.lineno(1)), p[2], p[3], p[5], p[6])
    
def p_comp_unit_body_sen(p):
    '''comp_unit_body : SENSOR ID formal_params NEWLINE definitions sens_act_entries '''
    p[0] = st.ND_sensor(st.LN(p.lineno(1)), p[2], p[3], p[5], p[6])
    
def p_comp_unit_body_beh(p):
    '''comp_unit_body : BEHAVIOR ID formal_params NEWLINE definitions beh_entries '''
    p[0] = st.ND_behavior(st.LN(p.lineno(1)), p[2], p[3], p[5], p[6])

def p_comp_unit_body_act(p):
    '''comp_unit_body : opt_mixing ACTUATOR ID formal_params NEWLINE definitions sens_act_entries '''
    p[0] = st.ND_actuator(st.LN(p.lineno(2)), p[3] ,p[4], p[6], p[7], p[1] != None )
    
def p_formal_params(p):
    '''formal_params : LPAR opt_formal_param_list RPAR '''
    p[0] = p[2]

def p_definitions(p):
    '''definitions : opt_types_consts opt_globals PRIVATE NEWLINE opt_types_consts opt_globals '''
    # ND_comp_unit_defs (exportedTypesConsts exportedGlobals privateTypesConsts privateGlobals)
    p[0] = st.ND_comp_unit_defs(p[1], p[2], p[5], p[6])

# ################################################################
#    
# Import statements
#
def p_import_block_single(p):
    'import_block : IMPORT import_spec NEWLINE'
    p[0] = st.ND_def_block_import(p[2])

def p_import_block_list(p):
    "import_block : IMPORT ':' INDENT import_spec_list dedent"
    p[0] = st.ND_def_block_import(p[4])

def p_import_spec_list(p):
    '''import_spec_list : import_spec_list NEWLINE import_spec
        | import_spec'''
    if len(p) > 2:
        p[0] = st.ndlappend(p[1], p[3], p[2])
    else:
        p[0] = [p[1]]
    
def p_import_spec(p):
    '''import_spec : ID
        | ID AS ID '''
    if len(p) > 2:
        p[0] = st.ND_def_import(st.LN(p.lineno(1)), p[1], p[3])
    else:
        p[0] = st.ND_def_import(st.LN(p.lineno(1)), p[1])

def p_import_spec_comment(p):
    '''import_spec : comment '''
    p[0] = p[1]

# ##############################################################
#
# Type and constant declarations
#
# Types and consts occur lexically prior to any storage
# allocation and function definitions.
#

def p_opt_types_consts(p):
    '''opt_types_consts : type_const_list
        | empty %prec COMMENT '''
    p[0] = p[1]

def p_type_const_list_recurse(p):
    '''type_const_list : type_const_list type_const_block'''
    p[0] = st.ndlappend(p[1], p[2])

def p_type_const_list_induce(p):
    '''type_const_list : type_const_block'''
    p[0] = [p[1]]

def p_type_const_block(p):
    '''type_const_block : typedef_block
        | constdef_block
        | comment NEWLINE %prec COMMENT'''
    p[0] = p[1]



# ################################################################
#
# Type definitions
#


def p_typedef_block_single(p):
    'typedef_block : TYPE type_spec'
    p[0] = st.ND_def_block_type(p[2])
    
def p_typedef_block_list(p):
    "typedef_block : TYPE ':' INDENT type_spec_list dedent"
    p[0] = st.ND_def_block_type(p[3])

def p_type_spec_list_recurse(p):
    'type_spec_list : type_spec_list NEWLINE type_spec'
    p[0] = st.ndlappend(p[1], p[3], p[2])    
    
def p_type_spec_list_induce(p):
    'type_spec_list : type_spec'
    p[0] = [p[1]]

def p_type_spec(p):
    '''type_spec : type_constructor
        | type_id type_derivative ID '''
    p[0] = p[1] if len(p) < 3 else st.ND_def_type_derivation(p[3], p[1], p[2])

def p_type_spec_funcsig(p):
    "type_spec : function_signature "
    p[0] = p[1]
    
def p_type_spec_comment(p):
    '''type_spec : comment '''
    p[0] = p[1]
    
def p_type_construction(p):
    '''type_constructor : STRUCT ID ':' state_alloc_block %prec NEWLINE 
        | UNION ID ':' state_alloc_block %prec NEWLINE
        | CLASS ID '(' ID ')' ':' INDENT state_and_fcn_decls dedent '''
    if p[1] == 'struct':
        p[0] = st.ND_def_type_struct(st.LN(p.lineno(1)), p[2], p[4])
    elif p[1] == 'union':
        p[0] = st.ND_def_type_union(st.LN(p.lineno(1)), p[2], p[4])
    else:
        p[0] = None # FIXME: class creation

def p_type_derivitive_list(p):
    "type_derivative : LIST"
    p[0] = st.ND_def_type_derive_list()
    
def p_type_derivitive_hash(p):
    "type_derivative : LBRKT exp HASH ID RBRKT"
    p[0] = st.ND_def_type_derive_hash(p[2],p[4])
    
def p_type_derivitive_array(p):
    "type_derivative : LBRKT exp_list RBRKT"
    p[0] = st.ND_def_type_derive_array(p[2])

def p_type_derivitive_iter(p):
    '''type_derivative : LIST ITER
        | LIST REVERSE ITER
        | HASH ITER '''
    if p[1] == 'HASH':
        p[0] = st.ND_def_type_derive_iter_hash()
    else:
        p[0] = st.ND_def_type_derive_iter_list(p[2] == 'REVERSE')

# #########################
#
# Constant expressions
#
def p_constdef_block_single(p):
    'constdef_block : CONST const_spec'
    p[0] = st.ND_def_block_const(p[2])

def p_constdef_block_list(p):
    "constdef_block : CONST ':' INDENT const_spec_list dedent"
    p[0] = st.ND_def_block_const(p[3])

def p_const_spec_list_recurse(p):
    'const_spec_list : const_spec_list NEWLINE const_spec'
    # simple list
    p[0] = st.ndlappend(p[1], p[3], p[2])

def p_const_spec_list_induce(p):
    'const_spec_list : const_spec'
    # simple list
    p[0] = [p[1]]
    
def p_const_spec(p):
    "const_spec : ID '=' exp"
    p[0] = st.ND_def_const(st.LN(p.lineno(2)), p[1],p[3])

def p_const_spec_cast(p):
    "const_spec : type_id ID '=' exp"
    p[0] = st.ND_def_const(st.LN(p.lineno(3)), p[2], p[4], p[1])
    
def p_cost_spec_comment(p):
    'const_spec : comment'
#    'const_spec : comment_block'
    p[0] = p[1]

# ###############################################################
#
# Variable Declarations
#

def p_state_and_fcn_decls_recurse(p):
    '''state_and_fcn_decls : state_and_fcn_decls var_block_lockable
        | state_and_fcn_decls function_def '''
    # Returns a simple list. 
    p[0] = st.ndlappend(p[1], p[2])

def p_state_and_fcn_decls_recurse_cmt(p):
    '''state_and_fcn_decls : state_and_fcn_decls comment NEWLINE '''
    # Add the merge test code here.
    p[0] = st.ndlappend(p[1], p[2])  
      
def p_state_and_fcn_decls_induce(p): 
    '''state_and_fcn_decls : var_block_lockable
        | function_def '''
        # 'comment' here introduces s/r conflicts.
    p[0] = [p[1]]

def p_var_block_lockable(p):
    "var_block_lockable : VAR ':' INDENT state_decl_lockable_list dedent"
    p[0] = st.ND_var_section(p[4])

def p_state_decl_lockable_list(p):
    '''state_decl_lockable_list : state_decl_lockable_list state_decl_lockable
        | state_decl_lockable '''
    # Returns simple list of ND_state_block & ND_state_block_locked
    if len(p) > 2:
        p[0] = st.ndlappend(p[1], p[2])
    else:
        p[0] = [p[1]]
    
def p_state_decl_atomic(p):
    '''state_decl_lockable : ATOMIC ID ':' INDENT state_block dedent '''
    # The _semaphore type is implied for the lock variable,
    # so construct a storage allocator.
    semtype = st.ND_fqid([st.ND_id('_semaphore','type')])
    seminst = st.ND_def_instance_list(semtype, [p[2]], False)
    p[0] =st.ND_state_block_locked(p[5], seminst)

def p_state_decl_not_atomic(p):
    '''state_decl_lockable  : state_block %prec NEWLINE'''
    p[0] = p[1]

def p_state_block(p):
    '''state_block : state_block NEWLINE state_alloc
        | state_block NEWLINE comment
        | state_alloc 
        | comment'''
    # reduces to ND_state_block
    if len(p) > 2:
        p[0] = p[1].append(p[3])
    else:
        p[0] = st.ND_state_block([p[1]])

# ND_def_instance_list understands both single line and block 
# style decls, and has a 'blockStyle' property to distinguish for 
# lexical reconstruction.
# The instance_list passed in is a list of lists.
def p_state_alloc_list(p):
    'state_alloc : type_id instance_alloc_list'
    p[0] = st.ND_def_instance_list(p[1], [p[2]], False)

def p_state_alloc_indented_block(p):
    "state_alloc : type_id ':' INDENT state_alloc_block dedent"
    p[0] = st.ND_def_instance_list(p[1], p[4], True)

def p_state_alloc_block(p):
    '''state_alloc_block : state_alloc_block NEWLINE instance_alloc_list
        | instance_alloc_list'''
    # Creates a list of lists. Each sublist is lexically one line.
    if len(p) > 2:
        p[0] = p[1]
        p[0].append(p[3])
    else:
        p[0] = [p[1]]

def p_type_id_recurse(p):
    '''type_id : type_id '.' ID '''
    p[0] = p[1]
    p[0].append(p[3])

def p_type_id_induce(p):
    '''type_id : ID '''
    p[0] = st.ND_fqid([p[1]])

def p_instance_alloc_list(p):
    '''instance_alloc_list : instance_alloc_list ',' instance_alloc
        | instance_alloc'''
    # Returns a simple list of instantiations.
    # Lexically, the list represents one line of source. 
    if len(p) > 2:
        p[0] = st.ndlappend(p[1], p[3])
    else:
        p[0] = [p[1]]

##############################################################
# FIXME: complete here and the ND_def_instance() object
# Decide if will have class hierarcy or a combined class.
# Design how to work in derivations and initializations.
def p_instance_alloc(p):
    '''instance_alloc : comment
        | ID '=' exp
        | ID '=' initializer
        | ID LPAR exp_list RPAR
        | LBRKT RBRKT ID '=' initializer '''
#        | comment_block
    # 2. simple variable with initial value. 
    # 3. complex struct and array initializer
    # 4. object instance
    # 7. Initialized array where size set by initialization.
    pass

def p_instance_alloc_simple(p):
    '''instance_alloc : ID '''
    # Simple variable.
    p[0] = st.ND_alloc_var(st.LN(p.lineno(1)), p[1])

def p_instance_alloc_pool(p):
    '''instance_alloc : POOL exp '''
    # Allocation for a list pool.
    p[0] = st.ND_alloc_pool(st.LN(p.lineno(1)), st.makename('_P'), p[2])

def p_instance_alloc_pool_share(p):
    '''instance_alloc : POOL SHARE ID '''
    # Shared pool allocation. Allocate storage for one pool, 
    # size is computed to store N of those items.
    # The free-list is constructed using the largest
    # type. So... sizeof(largest) * N explicitly size
    # pool is the total storage required, plus overhead.
    # Essentially, all are competing for a shared free list. 
    p[0] = st.ND_alloc_pool(st.LN(p.lineno(1)), st.makename('_P', None, p[3]))

def p_instance_alloc_derivative(p):
    '''instance_alloc : type_derivative ID'''
    # Instantiation of a variable with local derivation from base type. 
    p[0] = st.ND_alloc_derivative(st.LN(p.lineno(2)), p[2],p[1])
    
####
##############################
####

def p_initializer(p):
    '''initializer : LPAR exp_list RPAR 
        | INDENT initializer_list dedent 
        | LPAR INDENT initializer_list dedent RPAR'''
    pass

def p_initializer_list(p):
    '''initializer_list : initializer_list NEWLINE initializer
        | initializer_list NEWLINE comment
        | initializer 
        | exp_list
        | comment'''
    pass
    
# ###############################################################
#
# Compilation unit code bodies.
#
def p_beh_entries(p):
    '''beh_entries : beh_entries beh_entry
        | beh_entries comment
        | beh_entry'''
    # simple list
    if len(p) > 2:
        p[0] = st.ndlappend(p[1],p[2])
    else:
        p[0] = [p[1]]
     
def p_beh_entry(p):
    '''beh_entry : init_block
        | beh_start
        | beh_resume
        | beh_suppress
        | beh_select'''
    p[0] = p[1]

def p_init_block(p):
    "init_block : INIT ':' local_context"
    p[0] = st.ND_entry_point_init(p[3])

def p_beh_start(p):
    '''beh_start : START ':' local_context'''
    p[0] = st.ND_entry_point_start(p[3])
    
def p_beh_resume(p):
    '''beh_resume : RESUME ':' local_context'''
    p[0] = st.ND_entry_point_resume(p[3])
    
def p_beh_suppress(p):
    '''beh_suppress : SUPPRESS ':' local_context'''
    p[0] = st.ND_entry_point_suppress(p[3])
    
def p_beh_select(p):
    '''beh_select : SELECT ':' local_context'''
    p[0] = st.ND_entry_point_select(p[3])

def p_poll_entry(p):
    ''' poll_entry : POLL opt_exp ':' local_context '''
    p[0] = st.ND_entry_point_poll(p[4], p[2])

def p_opt_mixing(p):
    '''opt_mixing : MIXING
        | empty '''
    p[0] = p[1]

def p_sens_act_entries(p):
    '''sens_act_entries : sens_act_entries sens_act_entry
        | sens_act_entries comment
        | sens_act_entry '''
    # simple list
    if len(p) > 2:
        p[0] = st.ndlappend(p[1], p[2])
    else:
        p[0] = [p[1]]

def p_sens_act_entry(p):
    '''sens_act_entry : init_block
        | poll_entry
        | interrupt_handler '''
    p[0] = p[1]

def p_interrupt_handler(p):
    "interrupt_handler : INTERRUPT STRLIT interrupt_body "
    p[0] = st.ND_interrupt_handler(p[2], p[3])

def p_interrupt_body(p):
    '''interrupt_body : FOREIGN NEWLINE
        | ':' INDENT ATOMIC ':' local_context FINALLY ':' local_context dedent '''
    # The code block under ATOMIC runs with interrupts disabled.
    # Interrupts are re-enabled at the FINALLY keyword. In some
    # implementations the FINALLY section may be scheduled through a 
    # realtime dispatcher, not simply fallen into, although either is
    # allowed.  The variables in each section are local to that section.
    if len(p) > 3:
        p[0] = st.ND_interrupt_body(p[5], p[8])
    else:
        # The FOREIGN token's value is a list of parameters accumualted
        # from the souce line containing the 'foreign' keyword. 
        p[0] =st.ND_foreign_extern(p[1])

############################################################
#
# Compilation unit: Mode.
#

def p_mode_body(p):
    '''mode_body : opt_mode_device_section mode_load_section mode_beh_section '''
    p[0] = p[1:]
    
def p_opt_mode_device_section(p):
    '''opt_mode_device_section : mode_device_section
        | empty '''
    p[0] = p[1]

def p_mode_device_section(p):
    '''mode_device_section : DEVICE ':' INDENT device_list dedent '''
    p[0] = st.ND_ldblock_device(p[4])
    
def p_mode_load_section(p):
    '''mode_load_section : LOAD ':' INDENT loadable_list dedent '''
    p[0] = st.ND_ldblock_code(p[1], p[4])
    
def p_mode_beh_section(p):
    '''mode_beh_section : BEHAVIOR ':' INDENT loadable_list dedent '''
    p[0] = st.ND_ldblock_code(p[1], p[4])
    
def p_device_list_recurse(p):
    '''device_list : device_list NEWLINE device
        | device_list NEWLINE comment '''
    # simple list
    p[0] = st.ndlappend(p[1], p[3])

def p_device_list_induce(p):
    '''device_list : device 
        | comment'''
    # simple list
    p[0] = [p[1]]

def p_device(p):
    '''device : ID '=' exp opt_node_name '''
    # new gram: drop the opt_node_name
    # exp must evaluate to a string that is a device name.
    # ND_device
    p[0] = st.ND_device(st.LN(p.lineno(2)), p[1], p[3], p[4])
    
def p_loadable_list_recurse(p):
    '''loadable_list : loadable_list NEWLINE loadable
        | loadable_list NEWLINE comment '''
    # simple list
    p[0] = st.ndlappend(p[1], p[3]) 

def p_loadable_list_induce(p):
    '''loadable_list : loadable
        | comment '''
    # simple list
    p[0] = [p[1]]

def p_loadable(p):
    '''loadable : ID '=' opt_mixing fqid LPAR opt_exp_list RPAR opt_node_name '''
    # ND_loadable -- the type property (ACTUATOR, BEHAVIOR, SENSOR) can
    # be set when the load_block is reduced. Right here, the type is
    # set to '<loadable>'.
    # FIXME: in new grammar, must check/set type property during
    # semantic analysis.
    p[0] = st.ND_loadable(st.LN(p.lineno(2)), p[1], '<loadable>',\
        p[4], p[6], p[8], p[3] == 'mixing')

def p_opt_node_name(p):
    '''opt_node_name : '@' exp 
        | empty '''
    # Exp must evaluate to a string that names a processing node.
    p[0] = p[2] if len(p) > 2 else None
    
# ############################################################
#
# Global variable and function declarations.    
#
def p_opt_globals(p):
    '''opt_globals : state_and_fcn_decls
        | empty'''
    p[0] = p[1]

# Functions
def p_function_def(p):
    "function_def : function_signature function_body"
    p[0] = st.ND_def_function(p[1], p[2])
    
def p_function_signature(p):
    "function_signature : opt_type_id FUNCTION ID LPAR opt_formal_param_list opt_iter RPAR "
    # ND_def_func_sig
    p[0] = st.ND_def_func_sig(st.LN(p.lineno(2)), p[3], p[1], p[5], p[6])
    
def p_opt_iter(p):
    '''opt_iter : ITER ID
        | empty '''
    p[0] = p[2] if len(p) > 2 else None
    
def p_opt_type_id(p):
    '''opt_type_id : type_id
        | empty'''
    p[0] = p[1]

def p_function_body(p):
    '''function_body : ':' local_context'''
    p[0] = p[2]


def p_function_body_foreign(p):
    '''function_body : FOREIGN NEWLINE '''
    # The FOREIGN token's value is a list of parameters accumualted
    # from the souce line containing the 'foreign' keyword. 
    p[0] = st.ND_foreign_extern(p[2])

# Local Context
def p_local_context(p):
    '''local_context : INDENT opt_local_vars stmt_list dedent'''
    p[0] = st.ND_local_context(p[2], p[3])

def p_opt_local_vars(p):
    '''opt_local_vars : VAR ':' INDENT state_block dedent
        | empty'''
    p[0] = p[4] if len(p) > 2 else None

# Formal param lists
def p_opt_formal_param_list(p):
    '''opt_formal_param_list : formal_param_list
        | empty'''
    p[0] = p[1]

def p_formal_param_list(p):
    '''formal_param_list  : formal_param_list ',' formal_param
        | formal_param'''
    # Simple list of ND_def_formal
    if len(p) > 2:
        p[0] = p[1]
        p[0].append(p[3])
    else:
        p[0] = [p[1]]

def p_formal_param(p):
    '''formal_param : type_id ID
        | keyword_formal_type ID'''
    # ND_def_formal(typeID, formalID)
    p[0] = st.ND_def_formal(p[1], p[2])

def p_formal_param_func(p):
    '''formal_param : function_signature '''
    p[0] = p[1]

# 'mixing actuator <name>' is only allowed for actuators
# that are declared 'mixing'.  If 'mixing' is not stated
# at the instantiation of a behavior, then the actuator
# is taken exclusively, even if it has been declared mixing.
# Two behaviors can both share a mixing actuator, but
# if one takes the actuator non-mixing, then it will block
# lower priority behaviors or be blocked by higher priority
# behaviors, even those that desire mixing access.
def p_keyword_formal_type(p):
    '''keyword_formal_type : BEHAVIOR
        | DEVICE
        | SENSOR '''
    p[0] = st.ND_kw_formal_type(p[1])
    
def p_keyword_formal_type_act(p):
    '''keyword_formal_type : opt_mixing ACTUATOR'''
    p[0] = st.ND_kw_formal_type(p[2], True)
    
#######################################################
# Statements
def p_stmt_list(p):
    '''stmt_list : stmt_list NEWLINE stmt
        | stmt'''
    # ND_stmt_list
    if len(p) > 2:
        p[0] = p[1].append(p[3])
    else:
        p[0] = st.ND_stmt_list([p[1]])

def p_block(p):
    'block : INDENT stmt_list dedent'
    p[0] = p[2]
    
# If statements
def p_stmt_if(p):
    "stmt : IF exp ':' block opt_elif_list opt_else"
    l = [(p[2],p[4])]
    l.extend(p[5])
    p[0] = st.ND_stmt_if(l, p[6])

def p_opt_elif_list(p):
    '''opt_elif_list : elif_list
        | empty '''
    p[0] = p[1]
    
def p_elif_list_recurse(p):
    "elif_list : elif_list ELIF exp ':' block"
    # returns list of tuples (exp, block)
    p[0] = p[1]
    p[0].append((p[3],p[5]))
    
def p_elif_list_induce(p):
    "elif_list : ELIF exp ':' block"
    # Returns simple list starter containing a tuple (exp, block) 
    p[0] = [(p[2], p[4])]
    
def p_opt_else(p):
    '''opt_else : ELSE ':' block
        | empty'''
    p[0] = p[3] if len(p) > 2 else None

# Looping constructs
def p_stmt_while(p):
    "stmt : WHILE exp ':' block"
    p[0] =st.ND_stmt_while(st.LN(p.lineno(1)), p[2], p[4])

def p_stmt_every(p):
    "stmt : EVERY exp ':' block"
    # exp is a time in mSec
    p[0] =st.ND_stmt_every(st.LN(p.lineno(1)), p[2], p[4])

# Critical section
def p_stmt_with(p):
    "stmt : ATOMIC opt_lval ':' block"
    # opt_lval is either a semaphore variable (spin lock), or if omitted
    # the atomic code section runs with interrupts disabled.
    p[0] = st.ND_stmt_atomic(st.LN(p.lineno(1)), p[2], p[4])

# Exception wrapper
def p_stmt_try(p):
    "stmt : TRY ':' block opt_except_list opt_finally "
    p[0] = st.ND_stmt_try(st.LN(p.lineno(1)), p[3],p[4],p[5])

def p_opt_except_list(p):
    '''opt_except_list : except_list
        | empty'''
    # Simple list of ND_except_block or None
    p[0] = p[1]

def p_except_list(p):
    '''except_list : except_list except_clause
        | except_clause '''
    # simple list of ND_except_block
    if len(p) > 2:
        p[0] = p[1]
        p[0].append(p[2])
    else:
        p[0] = [p[1]] 

def p_except_clause(p):
    '''except_clause : EXCEPT exp_list ':' block '''
    # exp_list is a list of strings that evaluate to exception names.
    p[0] = st.ND_except_block(st.LN(p.lineno(1)), p[2], p[4])

def p_opt_finally(p):
    '''opt_finally : finally_clause
        | empty '''
    p[0] = p[1]

def p_finally_clause(p):
    '''finally_clause : FINALLY ':' block '''
    # FIXME: Unless a ND_ class is created for the finally clause, there
    # isn't a way to add tracking.
    p[0] = p[3]
    
# Assignment statement
def p_stmt_assignment(p):
    '''stmt : lval '=' exp
        | lval PLUS_EQ exp
        | lval MINUS_EQ exp '''
    p[0] = st.ND_stmt_assign(st.LN(p.lineno(2)), p[2], p[1],p[3])

def p_lval(p):
    "lval : fqid"
    p[0] = p[1]
    
def p_opt_lval(p):
    '''opt_lval : lval
        | empty '''
    p[0] = p[1]

# procedure call
def p_stmt_proc_call(p):
    'stmt : fqid LPAR opt_exp_list RPAR'
    p[0] = st.ND_stmt_call(st.LN(p.lineno(2)), p[1], p[3])
    
# Return statement
def p_stmt_return(p):
    '''stmt : RETURN opt_exp '''
    p[0] = st.ND_stmt_return(st.LN(p.lineno(1)), p[2])

# Inline
def p_inline_block(p):
    '''stmt : INLINE line_image_list INLINE_END''' 
    # ND_stmt_inline
    p[0] = st.ND_stmt_inline(st.LN(p.lineno(1)), p[1], p[2])

def p_strlit_list(p):
    '''line_image_list : line_image_list LINE_IMAGE
        | LINE_IMAGE '''
    # Simple list of line images.
    if len(p) > 2:
        p[0] = p[1]
        p[0].append(p[2])
    else:
        p[0] = [p[1]]

# Comment statements
def p_stmt_comment(p):
    "stmt : comment %prec COMMENT"
    p[0] = p[1]

# ###################################################################
#
# Expressions
#
def p_opt_exp(p):
    '''opt_exp : exp
        | empty '''
    p[0] = p[1]
    
def p_exp_binop(p):
    '''exp : exp '+' exp
        | exp '-' exp
        | exp '*' exp
        | exp '/' exp
        | exp '<' exp
        | exp '>' exp 
        | exp LE exp
        | exp GE exp
        | exp EQ_EQ exp
        | exp NEQ exp '''
    p[0] = st.ND_exp(p[2], [p[1],p[3]]) 

def p_exp_unary_prefix(p):
    "exp : '-' exp %prec UMINUS"
    p[0] = st.ND_exp('UMINUS',p[2])

def p_exp_parens(p):
    "exp : '(' exp ')'"
    p[0] = p[2]

def p_exp_id(p):
    'exp : fqid'
    p[0] = p[1]

def p_exp_fcn(p):
    'exp : fqid LPAR opt_exp_list RPAR'
    # Function or method call site. 
    p[0] = st.ND_exp_callsite(st.LN(p.lineno(2)), p[1], p[3])

# FIXME: The opt_comment_block should go on exp_list, not opt_exp_list
# FIXME: Use ND_exp_list to construct the parse node.
#           Split exp and comment, and let the append of comments
#           trail.  That cleans up the parser, and is squared up
#           on the last reduce operation. That will simplify tree
#           walking.  
#           Or maybe just get rid of the comments in expression lists.
#           Comments after commas are mainly useful in initializers.
def p_opt_exp_list(p):
    '''opt_exp_list : exp_list
        | empty'''
    p[0] = p[1]
    
def p_exp_list(p):
    '''exp_list : exp_list ',' exp
        | exp'''
    # ND_exp_list
    if len(p) > 2:
        p[0] = p[1].append([p[3]])
    else:
        p[0] = st.ND_exp_list([p[1]])
    
def p_exp_lit_literal(p):
    '''exp : NUMBER
        | STRLIT '''
    p[0] = p[1]
    
# FIXME: Use ND_fqid and ND_index_exp parse nodes.
def p_fqid_induce(p):
    'fqid : ID'
    p[0] = st.ND_fqid([p[1]])

def p_fqid_recurse(p):
    '''fqid : fqid '.' ID
        | fqid LBRKT exp_list RBRKT'''
    p[0] = p[1]
    p[0].append(p[3])

# #################################################################
#
# Comments
#
def p_comment(p):
    '''comment : COMMENT '''
    p[0] = st.ND_comment_blk(st.LN(p.lineno(1)), p[1])

def p_comment_doc(p):
    '''comment : COMMENT_DOC %prec COMMENT'''
    p[0] = st.ND_comment_blk(st.LN(p.lineno(1)), p[1], True)
    
def p_comment_pragma(p):
    '''comment : PRAGMA %prec COMMENT '''
    p[0] = st.ND_pragma(st.LN(p.lineno(1)), p[1])
    
# #############################################################
#
# Infrastructure productions
#
def p_opt_newline(p):
    '''opt_newline : NEWLINE
        | empty'''
    pass

def p_dedent(p):
    'dedent : DEDENT'
    pass

def p_dedent_error(p):
    'dedent : DEDENT_ERROR'
    errorMessage('Indentation error at line ' + str(p.lineno)) 
    errorCount += 1
    
def p_empty(p):
    'empty :'
    pass

def p_error(p):
    global errorCount
    ln = 'on line ' + str(p.lineno) + ' near: ' + str(p.type) if p else \
        'at end of file.'
    s = 'Syntax error ' + ln
    errorMessage(s)
    errorMessage(repr(p))
    errorCount += 1

# Instantiate the parser
capekParser = yacc.yacc()

# Clear the error count.
errorCount = 0

if __name__ == "__main__":
    import logging
    logging.basicConfig(filename='parse.log',level=logging.DEBUG)
    log = logging.getLogger()
    
    testname = 'test/syntax001.ck'
    print 'Testing with:',testname
    f = open(testname).read()
    #print f
    rslt = capekParser.parse(f, debug=log)
    print 'result>',rslt
    
