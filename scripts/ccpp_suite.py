#!/usr/bin/env python
#

"""Classes and methods to create a Fortran suite-implementation file
to implement calls to a set of suites for a given host model."""

# Python library imports
from __future__ import print_function
import copy
import os.path
import sys
import re
import xml.etree.ElementTree as ET
# CCPP framework imports
from parse_tools   import ParseContext, ParseSource, context_string
from parse_tools   import ParseInternalError, ParseSyntaxError, CCPPError
from parse_tools   import FORTRAN_ID
from parse_tools   import read_xml_file, validate_xml_file, find_schema_version
from metavar       import Var, VarDDT, VarDictionary, ddt_modules, CCPP_STANDARD_VARS
from state_machine import StateMachine
from fortran_tools import FortranWriter

###############################################################################
# Module (global) variables
###############################################################################

__init_st__ = r"(?:(?i)init(?:ial(?:ize)?)?)"
__final_st__ = r"(?:(?i)final(?:ize)?)"
__run_st__ = r"(?:(?i)run)"
__ts_init_st__ = r"(?:(?i)timestep_init(?:ial(?:ize)?)?)"
__ts_final_st__ = r"(?:(?i)timestep_final(?:ize)?)"

dimension_re = re.compile(FORTRAN_ID+r"_((?i)dimension)$")

array_ref_re = re.compile(r"([^(]*)[(]([^)]*)[)]")

obj_loc_re = re.compile(r"(0x[0-9A-Fa-f]+)>")

# Source for internally generated variables.
__api_source__ = ParseSource("CCPP_API", "module",
                             ParseContext(filename="ccpp_suite.py"))

# Allowed CCPP transitions
CCPP_STATE_MACH = StateMachine((('initialize',       'uninitialized',
                                 'initialized',       __init_st__),
                                ('timestep_initial', 'initialized',
                                 'in_time_step',      __ts_init_st__),
                                ('run',              'in_time_step',
                                 'in_time_step',      __run_st__),
                                ('timestep_final',   'in_time_step',
                                 'initialized',       __ts_final_st__),
                                ('finalize',         'initialized',
                                 'uninitialized',     __final_st__)))

# Required variables for inclusion in auto-generated schemes
CCPP_REQUIRED_VARS = [CCPP_STANDARD_VARS['ccpp_error_flag'],
                      CCPP_STANDARD_VARS['ccpp_error_message']]

# CCPP copyright statement to be included in all generated Fortran files
COPYRIGHT = '''!
! This work (Common Community Physics Package Framework), identified by
! NOAA, NCAR, CU/CIRES, is free of known copyright restrictions and is
! placed in the public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'''

# Standardize name of generated kinds file and module
KINDS_MODULE = 'ccpp_kinds'
KINDS_FILENAME = '{}.F90'.format(KINDS_MODULE)

###############################################################################
def new_suite_object(item, context, parent, logger):
###############################################################################
    "'Factory' method to create the appropriate suite object from XML"
    new_item = None
    if item.tag == 'subcycle':
        new_item = Subcycle(item, context, parent, logger)
    elif item.tag == 'scheme':
        new_item = Scheme(item, context, parent, logger)
    elif item.tag == 'timesplit':
        new_item = TimeSplit(item, context, parent, logger)
    else:
        raise CCPPError("Unknown CCPP suite element type, '{}'".format(item.tag))
    # End if
    return new_item

###############################################################################

class CallList(VarDictionary):
    "A simple class to hold a routine's call list (dummy arguments)"

    def __init__(self, name, logger=None):
        super(CallList, self).__init__(name, logger=logger)

    def add_vars(self, call_list):
        "Add new variables from another CallList (<call_list>)"
        for var in call_list.variable_list():
            stdname = var.get_prop_value('standard_name')
            if stdname not in self:
                self.add_variable(var)
            # End if
        # End for

    def call_string(self, dict=None):
        """Return a dummy argument string for this call list.
        dict may be another VarDictionary object from which to retrieve
        local_names (default is to use self).
        """
        arg_str = ""
        for var in self.variable_list():
            if dict is not None:
                stdname = var.get_prop_value('standard_name')
                dvar = dict.find_variable(stdname)
                if dvar is None:
                    raise CCPPError("Variable, '{}', not found in {}".format(stdname, dict.name))
                else:
                    lname = dvar.get_prop_value('local_name')
                # End if
            else:
                lname = var.get_prop_value('local_name')
            # End if
            if len(arg_str) > 0:
                arg_str = arg_str + ", "
            # End if
            arg_str = arg_str + lname
        # End for
        return arg_str

###############################################################################

class SuiteObject(VarDictionary):
    "Base class for all CCPP Suite objects (e.g., Scheme, Subcycle)"

    def __init__(self, name, context, parent, logger):
        self.__name = name
        self._context = context
        self._logger = logger
        self._parent = parent
        self._call_list = CallList(name, logger)
        self._parts = list()
        # Initialize our dictionary
        super(SuiteObject, self).__init__(self.name, parent_dict=parent)

    def declarations(self):
        """Return a list of local variables to be declared in parent Group
        or Suite. By default, this list is the object's embedded VarDictionary.
        """
        return self.variable_list()

    def add_part(self, item):
        'Add an object (e.g., Suite, Subcycle) to this SuiteObject'
        self._parts.append(item)

    def schemes(self):
        "Return a flattened list of schemes for this group"
        schemes = list()
        for item in self._parts:
            schemes.extend(item.schemes())
        # End for
        return schemes

    @property
    def name(self):
        '''Return the name of the element'''
        return self.__name

    @name.setter
    def name(self, value):
        '''Set the name of the element if it has not been set'''
        if self.__name is None:
            self.__name = value
        else:
            raise ParseInternalError('Attempt to change name of {} to {}'.format(self, value))
        # End if

    @property
    def parent(self):
        '''Element parent (or none)'''
        return self._parent

    @property
    def call_list(self):
        return self._call_list

    @property
    def parts(self):
        return self._parts

    def __repr__(self):
        'Create a unique readable string for this Object'
        repr = super(SuiteObject, self).__repr__()
        olmatch = obj_loc_re.search(repr)
        if olmatch is not None:
            loc = ' at {}'.format(olmatch.group(1))
        else:
            loc = ""
        # End if
        if self._subroutine_name is None:
            substr = ''
        else:
            substr = ': {}'.format(self._subroutine_name)
        # End if
        return '<{} {}{}>'.format(self.__class__.__name__, self.name, loc)

    def __format__(self, spec):
        """Return a string representing the SuiteObject, including its children.
        <sep> is used between subitems.
        <ind_level> is the indent level for multi-line output.
        """
        if len(spec) > 0:
            sep = spec[0]
        else:
            sep = '\n'
        # End if
        try:
            ind_level = int(spec[1:])
        except (ValueError, IndexError) as ve:
            ind_level = 0
        # End try
        if sep == '\n':
            indent = "  "
        else:
            indent = ""
        # End if
        if self.name == self.__class__.__name__:
            # This object does not have separate name
            nstr = self.name
        else:
            nstr = "{}: {}".format(self.__class__.__name__, self.name)
        # End if
        output = "{}<{}>".format(indent*ind_level, nstr)
        subspec = "{}{}".format(sep, ind_level + 1)
        substr = "{o}{s}{p:" + subspec + "}"
        subout = ""
        for part in self.parts:
            subout = substr.format(o=subout, s=sep, p=part)
        # End for
        if len(subout) > 0:
            output = "{}{}{}{}</{}>".format(output, subout, sep,
                                            indent*ind_level,
                                            self.__class__.__name__)
        else:
            output = "{}</{}>".format(output, self.__class__.__name__)
        # End if
        return output

###############################################################################

class Scheme(SuiteObject):
    "A single scheme in a suite (e.g., init method)"

    def __init__(self, scheme_xml, context, parent, logger):
        name = scheme_xml.text
        self._subroutine_name = None
        self._context = context
        self._version = scheme_xml.get('version', None)
        self._lib = scheme_xml.get('lib', None)
        super(Scheme, self).__init__(name, context, parent, logger)

# XXgoldyXX: v debug only
#    def ddtspec_to_str(self, ddt_spec, host_model):
#        "Properly convert a DDT field reference to a string"
#        args = list()
#        alen = len(ddt_spec)
#        index = 0
#        for var in ddt_spec:
#            ddt = index < (alen - 1)
#            argstr = self.host_arg_str(var, host_model, ddt)
#            index = index + 1
#            args.append(argstr)
#        # End for
#        return '%'.join(args)
#
#    def find_host_model_var(self, hdim, host_model):
#        "Create the correct array dimension reference for hdim"
#        hsdims = list()
#        for hsdim in hdim.split(':'):
#            hsdim_var = host_model.find_variable(hsdim, loop_subst=True)
#            if hsdim_var is None:
#                raise CCPPError("No matching host variable for {} dimension, {}".format(self._subroutine_name, hsdim))
#            elif isinstance(hsdim_var, tuple):
#                # This is a dimension range (e.g., from a loop_subst)
#                lnames = [x.get_prop_value('local_name') for x in hsdim_var]
#                hsdims.extend(lnames)
#            elif isinstance(hsdim_var, VarDDT):
#                # This is a DDT reference
#                ##XXgoldyXX: HACK! Take this out when DDTS removed from suites
#                hsdims.append(self.ddtspec_to_str(hsdim_var._var_ref_list, host_model))
#            else:
#                hsdims.append(hsdim_var.get_prop_value('local_name'))
#            # End if
#        # End for
#        loop_var = VarDictionary.loop_var_match(hdim)
#        if (dimension_re.match(hdim) is not None) and (len(hsdims) == 1):
#            # We need to specify the whole range
#            hsdims = ['1'] + hsdims
#        elif loop_var and (len(hsdims) == 1):
#            # We may to specify the whole range
#            lv_type = hdim.split('_')[-1]
#            if lv_type == 'extent':
#                hsdims = ['1'] + hsdims # This should print as '1:<name>_extent'
#            elif lv_type == 'beg':
#                hsdims.append('') # This should print as '<name>_beg:'
#            elif lv_type == 'end':
#                hsdims = [''] + hsdims # This should print as ':<name>_end'
#            elif lv_type == 'number':
#                pass # This should be a single value (not an array section)
#            else:
#                raise ParseInternalError("Unknown loop variable type, '{}' in '{}'".format(lv_type, hdim))
#            # End if
#        # End if
#        return ':'.join(hsdims)
#
#    def host_arg_str(self, hvar, host_model, ddt):
#        '''Create the proper statement of a piece of a host-model variable.
#        If ddt is True, we can only have a single element selected
#        '''
#        hstr = hvar.get_prop_value('local_name')
#        hdims = hvar.get_dimensions()
#        dimsep = ''
#        # Does the local name have any extra indices?
#        match = array_ref_re.match(hstr.strip())
#        if match is not None:
#            tokens = [x.strip() for x in match.group(2).strip().split(',')]
#            # There should one ':' token for each entry in hdims
#            if tokens.count(':') != len(hdims):
#                raise CCPPError("Invalid DDT variable spec, {}, should have {} colons".format(hstr, len(hdims)))
#            else:
#                hstr = match.group(1)
#                hdims_temp = hdims
#                hdims = list()
#                hdim_index = 0
#                for token in tokens:
#                    if token == ':':
#                        hdims.append(hdims_temp[hdim_index])
#                        hdim_index = hdim_index + 1
#                    else:
#                        hdims.append(token)
#                    # End if
#                # End for
#            # End if
#        # End if
#        if len(hdims) > 0:
#            dimstr = '('
#        else:
#            dimstr = ''
#        # End if
#        for hdim in hdims:
#            # We can only have a single element of a DDT when selecting
#            # a field. Is this a thread block?
#            if ddt and (hdim == 'thread_block_begin:thread_block_end'):
#                hdim = 'thread_block_number'
#            # End if
#            if ddt and (':' in hdim):
#                raise CCPPError("Invalid DDT dimension spec {}{}".format(hstr, hdimval))
#            else:
#                # Find the host model variable for each dim
#                hsdims = self.find_host_model_var(hdim, host_model)
#                dimstr = dimstr + dimsep + hsdims
#                dimsep = ', '
#            # End if
#        # End for
#        if len(hdims) > 0:
#            dimstr = dimstr + ')'
#        # End if
#        return hstr + dimstr
# XXgoldyXX: ^ debug only

    def analyze(self, phase, group, scheme_library, suite_vars, level, logger):
        self._group = group
        my_header = None
        if self.name in scheme_library:
            func = scheme_library[self.name]
            if phase in func:
                my_header = func[phase]
                self._subroutine_name = my_header.title
            # End if
        # End if
        if my_header is None:
            estr = 'No {} header found for scheme, {}'
            raise ParseInternalError(estr.format(phase, self.name),
                                     context=self._context)
        # End if
        if my_header.module is None:
            estr = 'No module found for subroutine, {}'
            raise ParseInternalError(estr.format(self._subroutine_name),
                                     context=self._context)
        # End if
        scheme_mods = set()
        scheme_use = 'use {}, only: {}'.format(my_header.module,
                                               self._subroutine_name)
        scheme_mods.add(scheme_use)
        for var in my_header.variable_list():
            self._call_list.add_variable(var)
        # End for
        return scheme_mods

    def write(self, outfile, logger, indent):
        my_args = self.call_list.call_string(dict=self._group.call_list)
        outfile.write('call {}({})'.format(self._subroutine_name, my_args), indent)

    def schemes(self):
        'Return self as a list for consistency with subcycle'
        return [self]

    def __str__(self):
        'Create a readable string for this Scheme'
        return '<Scheme {}: {}>'.format(self.name, self._subroutine_name)

###############################################################################

class Subcycle(SuiteObject):
    "Class to represent a subcycled group of schemes or scheme collections"

    def __init__(self, sub_xml, context, parent, logger):
        name = sub_xml.get('name', None)
        self._loop = sub_xml.get('loop', "1")
        # See if our loop variable is an interger or a variable
        try:
            loop_int = int(self.loop)
            self._loop_var_int = True
        except ValueError as ve:
            self._loop_var_int = False
            lvar = parent.find_variable(self.loop, any_scope=True)
            if lvar is None:
                emsg = "Subcycle, {}, specifies {} iterations but {} not found"
                raise CCPPError(emsg.format(name, self.loop, self.loop))
            # End if
        # End try
        for scheme in sub_xml:
            new_item = new_suite_item(item, context)
            self.add_part(new_item)
        # End for
        super(Subcycle, self).__init__(name, context, parent, logger)

    def analyze(self, phase, group, scheme_library, suite_vars, level, logger):
        if self.name is None:
            self.name = "subcycle_index{}".format(level)
        # End if
        # Create a variable for the loop index
        self.add_variable(Var({'local_name':self.name,
                               'standard_name':'loop_variable',
                               'type':'integer', 'units':'count',
                               'dimensions':'()'}, __api_source__))
        # Handle all the suite objects inside of this subcycle
        scheme_mods = set()
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library, suite_vars, level+1, logger)
            for smod in smods:
                scheme_mods.add(smod)
            # End for
        # End for
        return scheme_mods

    def write(self, outfile, logger, indent):
        outfile.write('do {} = 1, {}'.format(self.name, self.loop), indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for item in self.parts:
            item.write(outfile, logger, indent+1)
        # End for
        outfile.write('end do', 2)

    @property
    def loop(self):
        '''Get the loop value or variable standard_name'''
        return self._loop

###############################################################################

class TimeSplit(SuiteObject):
    """Class to represent a group of processes to be computed in a time-split
    manner -- each parameterization or other construct is called with an
    state which has been updated from the previous step.
    """

    def __init__(self, sub_xml, context, parent, logger):
        for scheme in sub_xml:
            new_item = new_suite_item(item, context)
            self.add_part(new_item)
        # End for
        super(TimeSplit, self).__init__('TimeSplit', context, parent, logger)

    def analyze(self, phase, group, scheme_library, suite_vars, level, logger):
        # Handle all the suite objects inside of this group
        scheme_mods = set()
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library, suite_vars, level+1, logger)
            for smod in smods:
                scheme_mods.add(smod)
            # End for
        # End for
        return scheme_mods

    def write(self, outfile, logger, indent):
        for item in self.parts:
            item.write(outfile, logger, indent+1)
        # End for

###############################################################################

class Group(SuiteObject):
    """Class to represent a grouping of schemes in a suite
    A Group object is implemented as a subroutine callable by the API.
    The main arguments to a group are the host model variables.
    Additional output arguments are generated from schemes with intent(out)
    arguments.
    Additional input or inout arguments are generated for inputs needed by
    schemes which are produced (intent(out)) by other groups.
    """

    __subhead__ = '''
   subroutine {subname}({args})
'''

    __subend__ = '''
   end subroutine {subname}
'''

    __process_types__ = ['timesplit', 'processsplit']

    __process_xml__ = {}
    for gptype in __process_types__:
        __process_xml__[gptype] = '<{}></{}>'.format(gptype, gptype)
    # End for

    def __init__(self, group_xml, transition, parent, context, logger):
        name = parent.name + '_' + group_xml.get('name')
        if transition not in CCPP_STATE_MACH.transitions():
            raise ParseInternalError("Bad transition argument to Group, '{}'".format(transition))
        # End if
        self._transition = transition
        # Initialize the dictionary of variables internal to group
        super(Group, self).__init__(name, context, parent, logger)
        # Add the items but first make sure we know the process tpye for
        # the group (e.g., TimeSplit or ProcessSplit).
        if (len(group_xml) == 0) or (group_xml[0].tag not in Group.__process_types__):
            # Default is TimeSplit
            tsxml = ET.fromstring(Group.__process_xml__['timesplit'])
            ts = new_suite_object(tsxml, context, parent, logger)
            add_to = ts
            self.add_part(ts)
        else:
            add_to = self
        # End if
        # Add the sub objects either directly to the Group or to the TimeSplit
        for item in group_xml:
            new_item = new_suite_object(item, context, parent, logger)
            add_to.add_part(new_item)
        # End for
        self._local_schemes = set()
        self._host_vars = None
        self._host_ddts = None
        logger.debug("{}".format(self))

    def has_item(self, item_name):
        'Check to see if an item is already in this group'
        has = False
        for item in self.parts:
            if item.name == item_name:
                has = True
                break
            # End if
        # End for
        return has

    @property
    def phase(self):
        'Return the CCPP state transition for this group spec'
        return self._transition

    def phase_match(self, scheme_name):
        '''If scheme_name matches the group phase, return the group and
            function ID. Otherwise, return None
        '''
        fid, tid, mt = CCPP_STATE_MACH.transition_match(scheme_name,
                                                        transition=self.phase)
        if tid is not None:
            return self, fid
        else:
            return None, None
        # End if

    def move_to_call_list(self, standard_name):
        '''Move a variable from the group internal dictionary to the call list.
        This is done when the variable, <standard_name>, will be allocated by
        the suite.
        '''
        gvar = self.find_variable(standard_name, any_scope=False)
        if gvar is None:
            raise ParseInternalError("Group {}, cannot move {}, variable not found".format(self.name, standard_name))
        else:
            self._call_list.add_variable(gvar, exists_ok=True)
            self.remove_variable(standard_name)
        # End if

    def analyze(self, phase, suite_vars, scheme_library, logger):
        parent = self.parent
        self._phase = phase
        for item in self.parts:
            # Items can be schemes, subcycles or other objects
            # All have the same interface and return a set of module use
            # statements (lschemes) and a set of loop variables
            lschemes = item.analyze(phase, self, scheme_library, suite_vars, 1, logger)
            for lscheme in lschemes:
                self._local_schemes.add(lscheme)
            # End for
        # End for
        # Add each scheme's variables either to our call list (field
        # comes from the suite or the host model) or to our internal
        # dictionary (field is defined in the group subroutine).
        # Variable dimensions also count since they need a source.
        for scheme in self.schemes():
            for cvar in scheme.call_list.variable_list():
                slist = [cvar.get_prop_value('standard_name')]
                slist.extend(cvar.get_dim_stdnames())
                for stdname in slist:
                    if (parent is not None) and (parent.find_variable(stdname) is not None):
                        # Someone higher up knows about this
                        self._call_list.add_variable(cvar, exists_ok=True)
                    elif cvar.get_prop_value('intent') == 'out':
                        self.add_variable(cvar, exists_ok=True)
                    elif VarDictionary.loop_var_match(stdname):
                        # Someone will provide this for us (probably host)
                        # So add it to our call list
                        self._call_list.add_variable(cvar, exists_ok=True)
                    elif self.find_variable(stdname, any_scope=False) is None:
                        intent = cvar.get_prop_value('intent')
                        lname = cvar.get_prop_value('local_name')
                        raise CCPPError("{grp} / {sch} intent({int}) variable {lnam} has no source".format(grp=self.name, sch=scheme.name, int=intent, lnam=stdname))
                    # No else, this is a local group variable
                    # End if
                # End for
            # End for
        # End for
        self._phase_check_stmts = Suite.check_suite_state(phase)
        self._set_state = Suite.set_suite_state(phase)

    def write(self, outfile, logger, host_arglist, indent,
              suite_vars=None, allocate=False, deallocate=False):
        local_subs = ''
        # group type for (de)allocation
        if 'timestep' in self._phase:
            group_type = 'timestep'
        else:
            group_type = 'run'
        # End if
        # First, write out the subroutine header
        subname = self.name
        call_list = ", ".join(self._call_list.prop_list('local_name'))
        outfile.write(Group.__subhead__.format(subname=subname, args=call_list), indent)
        # Write out any use statements
        # Write out the scheme use statements
        for scheme in self._local_schemes:
            outfile.write(scheme, indent+1)
        # End for
        outfile.write('', 0)
        # Write out dummy arguments
        outfile.write('! Dummy arguments', indent+1)
        logger.debug('Variables for {}: ({})'.format(self.name, self._call_list.variable_list()))
        self._call_list.declare_variables(outfile, indent+1)
        subpart_var_set = {}
        for item in self.parts:
            for var in item.declarations():
                lname = var.get_prop_value('local_name')
                if lname in subpart_var_set:
                    if subpart_var_set[lname].compatible(var):
                        pass # We already are going to declare this variable
                    else:
                        errmsg = "Duplicate suite part variable, {}"
                        raise ParseInternalError(errmsg.format(lvar))
                    # End if
                else:
                    subpart_var_set[lname] = var
                # End if
            # End for
        # End for
        if len(subpart_var_set) > 0:
            outfile.write('\n! Local Variables', indent+1)
        # Write out local variables
        for var in subpart_var_set:
            outfile.write(var, indent+1)
        # End for
        outfile.write('', 0)
        # Check state machine
        verrflg = self.find_variable('ccpp_error_flag', any_scope=True)
        if verrflg is not None:
            errflg = verrflg.get_prop_value('local_name')
        else:
            raise CCPPError("No ccpp_error_flag variable for group, {}".format(self.name))
        # End if
        verrmsg = self.find_variable('ccpp_error_message', any_scope=True)
        if verrmsg is not None:
            errmsg = verrmsg.get_prop_value('local_name')
        else:
            raise CCPPError("No ccpp_error_message variable for group, {}".format(self.name))
        # End if
        for stmt in self._phase_check_stmts:
            text = stmt[0].format(errflg=errflg , errmsg=errmsg, funcname=self.name)
            outfile.write(text, indent + stmt[1])
        # End for
        # Allocate suite vars
        if allocate:
            for svar in suite_vars.variable_list():
                timestep_var = svar.get_prop_value('persistence')
                if group_type == timestep_var:
                    dims = svar.get_dimensions()
                    rdims = list()
                    for dim in dims:
                        rdparts = list()
                        dparts = dim.split(':')
                        for dpart in dparts:
                            dvar = self.find_variable(dpart, any_scope=True)
                            if dvar is None:
                                raise CCPPError("Dimension variable, {} not found{}".format(dim, context_string(self._context)))
                            else:
                                rdparts.append(dvar.get_prop_value('local_name'))
                            # End if
                        # End for
                        rdims.append(':'.join(rdparts))
                    # End for
                    alloc_str = ', '.join(rdims)
                    lname = svar.get_prop_value('local_name')
                    outfile.write("allocate({}({}))".format(lname, alloc_str), indent+1)
                # End if
            # End for
        # End if
        # Write the scheme and subcycle calls
        for item in self.parts:
            item.write(outfile, logger, indent+1)
        # End for
        # Deallocate suite vars
        if deallocate:
            for svar in suite_vars.variable_list():
                timestep_var = svar.get_prop_value('persistence')
                if group_type == timestep_var:
                    lname = svar.get_prop_value('local_name')
                    outfile.write('deallocate({})'.format(lname), indent+1)
                # End if
            # End for
        # End if
        outfile.write(self._set_state[0], indent + self._set_state[1])
        outfile.write(Group.__subend__.format(subname=subname), indent)

###############################################################################

class Suite(VarDictionary):

    ___state_machine_initial_state__ = 'uninitialized'
    __state_machine_var_name__     = 'ccpp_suite_state'

    __header__ ='''
!>
!! @brief Auto-generated cap module for the CCPP suite
!!
!
module {module}
'''

    __state_machine_init__ ='''
character(len=16) :: {css_var_name} = '{state}'
'''

    __footer__ = '''
end module {module}
'''

    # Note that these group names need to match CCPP_STATE_MACH
    __initial_group__ = '<group name="initialize"></group>'

    __final_group__ = '<group name="finalize"></group>'

    __timestep_initial_group__ = '<group name="timestep_initial"></group>'

    __timestep_final_group__ = '<group name="timestep_final"></group>'

    __scheme_template__ = '<scheme>{}</scheme>'

    def __init__(self, filename, api, logger):
        self._logger = logger
        self._name = None
        self._sdf_name = filename
        self._groups = list()
        self._suite_init_group = None
        self._suite_final_group = None
        self._timestep_init_group = None
        self._timestep_final_group = None
        self._context = None
        # Full phases/groups are special groups where the entire state is passed
        self._full_groups = {}
        self._full_phases = {}
        self._gvar_stdnames = {} # Standard names of group-created vars
        # Initialize our dictionary
        super(Suite, self).__init__(self.sdf_name, parent_dict=api, logger=logger)
        if not os.path.exists(self._sdf_name):
            raise CCPPError("Suite definition file {0} not found.".format(self._sdf_name))
        else:
            # Parse the SDF
            self.parse()
        # End if

    @property
    def name(self):
        '''Get the name of the suite.'''
        return self._name

    @property
    def sdf_name(self):
        '''Get the name of the suite definition file.'''
        return self._sdf_name

    @classmethod
    def check_suite_state(cls, stage):
        "Return a list of CCPP state check statements for <stage>"
        check_stmts = list()
        if stage in CCPP_STATE_MACH.transitions():
            # We need to make sure we are an allowed previous state
            prev_state = CCPP_STATE_MACH.initial_state(stage)
            css = "trim({})".format(Suite.__state_machine_var_name__)
            prev_str = "({} /= '{}')".format(css, prev_state)
            check_stmts.append(("if {} then".format(prev_str), 1))
            check_stmts.append(("{errflg} = 1", 2))
            errmsg_str = ("\"Invalid initial CCPP state, '\"//"+ css +
                          "//\"' in {funcname}\"")
            check_stmts.append(("{{errmsg}} = {}".format(errmsg_str), 2))
            check_stmts.append(("return", 2))
            check_stmts.append(("end if", 1))
        else:
            raise ParseInternalError("Unknown stage, '{}'".format(stage))
        # End if
        return check_stmts

    @classmethod
    def set_suite_state(cls, phase):
        final = CCPP_STATE_MACH.final_state(phase)
        return ("ccpp_suite_state = '{}'".format(final), 1)

    def new_group(self, group_string, transition):
        gxml = ET.fromstring(group_string)
        group = Group(gxml, transition, self, self._context, self._logger)
        self._full_groups[group.name] = group
        self._full_phases[group.phase] = group
        return group

    def parse(self):
        '''Parse the suite definition file.'''
        success = True

        tree, suite_xml = read_xml_file(self._sdf_name, self._logger)
        # We do not have line number information for the XML file
        self._context = ParseContext(filename=self._sdf_name)
        # Validate the XML file
        version = find_schema_version(suite_xml, self._logger)
        res = validate_xml_file(self._sdf_name, 'suite', version, self._logger)
        if not res:
            raise CCPPError("Invalid suite definition file, '{}'".format(self._sdf_name))
        # End if
        self._name = suite_xml.get('name')
        self._logger.info("Reading suite definition file for '{}'".format(self._name))
        self._suite_init_group = self.new_group(Suite.__initial_group__,
                                                "initialize")
        self._suite_final_group = self.new_group(Suite.__final_group__,
                                                 "finalize")
        self._timestep_init_group = self.new_group(Suite.__timestep_initial_group__,
                                                   "timestep_initial")
        self._timestep_final_group = self.new_group(Suite.__timestep_final_group__,
                                                    "timestep_final")
        # Set up some groupings for later efficiency
        self._beg_groups = [self._suite_init_group.name,
                            self._timestep_init_group.name]
        self._end_groups = [self._suite_final_group.name,
                            self._timestep_final_group.name]
        # Build hierarchical structure as in SDF
        self._groups.append(self._suite_init_group)
        self._groups.append(self._timestep_init_group)
        for suite_item in suite_xml:
            item_type = suite_item.tag.lower()
            # Suite item is a group or a suite-wide init or final method
            if item_type == 'group':
                # Parse a group
                self._groups.append(Group(suite_item, 'run', self, self._context, self._logger))
            else:
                match_trans = CCPP_STATE_MACH.function_match(item_type)
                if match_trans is None:
                    raise CCPPError("Unknown CCPP suite component tag type, '{}'".format(item_type))
                elif match_trans in self._full_phases:
                    # Parse a suite-wide initialization scheme
                    self._full_phases[match_trans].add_item(Scheme(suite_item, self._context))
                else:
                    raise ParseInternalError("Unhandled CCPP suite component tag type, '{}'".format(match_trans))
                # End if
        # End for
        self._groups.append(self._timestep_final_group)
        self._groups.append(self._suite_final_group)
        return success

    @property
    def module(self):
        '''Get the list of the module generated for this suite.'''
        return self._module

    @property
    def groups(self):
        '''Get the list of groups in this suite.'''
        return self._groups

    def find_variable(self, standard_name, any_scope=True, clone=None):
        """Attempt to return the variable matching <standard_name>.
        If <any_scope> is True, search parent scopes if not in current scope.
        If the variable is not found and <clone> is not None, add a clone of
        <clone> to this dictionary.
        If the variable is not found and <clone> is None, return None.
        """
        # First, see if the variable is already in our path
        var = super(Suite, self).find_variable(standard_name, any_scope=any_scope)
        if var is None:
            # No dice? Check for a group variable which can be promoted
            if standard_name in self._gvar_stdnames:
                group = self._gvar_stdnames[standard_name]
                # Promote variable to suite level
                var = group.find_variable(standard_name, any_scope=False)
                if var is None:
                    raise CCPPError("Group, {}, claimed it had created {} but variable was not found".format(group.name, standard_name))
                else:
                    # Remove this entry to avoid looping back here
                    del self._gvar_stdnames[standard_name]
                    self.add_variable(var)
                    # Move to group's call list and our group list
                    group.move_to_call_list(standard_name)
                # End if
            # End if
        # End if
        if (var is None) and (clone is not None):
            # Guess it is time to clone a different variable
            var = super(Suite, self).find_variable(standard_name, any_scope=any_scope, clone=clone)
        # End if
        return var

    def analyze(self, host_model, scheme_library, logger):
        '''Collect all information needed to write a suite file
        >>> CCPP_STATE_MACH.transition_match('init')
        'initialize'
        >>> CCPP_STATE_MACH.transition_match('init', transition='finalize')

        >>> CCPP_STATE_MACH.transition_match('INIT')
        'initialize'
        >>> CCPP_STATE_MACH.transition_match('initial')
        'initialize'
        >>> CCPP_STATE_MACH.transition_match('timestep_initial')
        'timestep_initial'
        >>> CCPP_STATE_MACH.transition_match('timestep_initialize')
        'timestep_initial'
        >>> CCPP_STATE_MACH.transition_match('timestep_init')
        'timestep_initial'
        >>> CCPP_STATE_MACH.transition_match('initialize')
        'initialize'
        >>> CCPP_STATE_MACH.transition_match('initialize')[0:4]
        'init'
        >>> CCPP_STATE_MACH.transition_match('initize')

        >>> CCPP_STATE_MACH.transition_match('run')
        'run'
        >>> CCPP_STATE_MACH.transition_match('finalize')
        'finalize'
        >>> CCPP_STATE_MACH.transition_match('finalize')[0:5]
        'final'
        >>> CCPP_STATE_MACH.transition_match('final')
        'finalize'
        >>> CCPP_STATE_MACH.transition_match('finalize_bar')

        >>> CCPP_STATE_MACH.function_match('foo_init')
        ('foo', 'init', 'initialize')
        >>> CCPP_STATE_MACH.function_match('foo_init', transition='finalize')
        (None, None, None)
        >>> CCPP_STATE_MACH.function_match('FOO_INIT')
        ('FOO', 'INIT', 'initialize')
        >>> CCPP_STATE_MACH.function_match('foo_initial')
        ('foo', 'initial', 'initialize')
        >>> CCPP_STATE_MACH.function_match('foo_initialize')
        ('foo', 'initialize', 'initialize')
        >>> CCPP_STATE_MACH.function_match('foo_initialize')[1][0:4]
        'init'
        >>> CCPP_STATE_MACH.function_match('foo_initize')
        (None, None, None)
        >>> CCPP_STATE_MACH.function_match('foo_timestep_initial')
        ('foo', 'timestep_initial', 'timestep_initial')
        >>> CCPP_STATE_MACH.function_match('foo_timestep_init')
        ('foo', 'timestep_init', 'timestep_initial')
        >>> CCPP_STATE_MACH.function_match('foo_timestep_initialize')
        ('foo', 'timestep_initialize', 'timestep_initial')
        >>> CCPP_STATE_MACH.function_match('foo_run')
        ('foo', 'run', 'run')
        >>> CCPP_STATE_MACH.function_match('foo_finalize')
        ('foo', 'finalize', 'finalize')
        >>> CCPP_STATE_MACH.function_match('foo_finalize')[1][0:5]
        'final'
        >>> CCPP_STATE_MACH.function_match('foo_final')
        ('foo', 'final', 'finalize')
        >>> CCPP_STATE_MACH.function_match('foo_finalize_bar')
        (None, None, None)
        >>> CCPP_STATE_MACH.function_match('foo_timestep_final')
        ('foo', 'timestep_final', 'timestep_final')
        >>> CCPP_STATE_MACH.function_match('foo_timestep_finalize')
        ('foo', 'timestep_finalize', 'timestep_final')
        '''
        # Collect all relevant schemes
        mgroups = self._full_groups.values()
        # For run groups, find associated init and final methods
        for group in self.groups:
            if group.name not in self._full_groups:
                scheme_list = group.schemes()
                for scheme in scheme_list:
                    if scheme.name in scheme_library:
                        func_entry = scheme_library[scheme.name]
                        for phase in self._full_phases:
                            if phase in func_entry:
                                # Add this scheme's init or final routine
                                pgroup = self._full_phases[phase]
                                header = func_entry[phase]
                                if not pgroup.has_item(header.title):
                                    sstr = Suite.__scheme_template__.format(scheme.name)
                                    sxml = ET.fromstring(sstr)
                                    scheme = Scheme(sxml, self._context,
                                                    pgroup, logger)
                                    pgroup.add_part(scheme)
                                # End if (no else needed)
                            # End if
                        # End for (phase)
                    # End if
                # End for (scheme_list)
            # End if
        # End for (groups)
        # Grab the host model argument list
        self._host_arg_list_full = host_model.argument_list()
        self._host_arg_list_noloop = host_model.argument_list(loop_vars=False)
        # First pass, create init, run, and finalize sequences
        for item in self.groups:
            if item.name in self._full_groups:
                phase = self._full_groups[item.name].phase
            else:
                phase = 'run'
            # End if
            logger.debug("Group {}, schemes = {}".format(item.name, [x.name for x in item.schemes()]))
            item.analyze(phase, self, scheme_library, logger)
            # Look for group variables that need to be promoted to the suite
            # We need to promote any variable used later to the suite, however,
            # we do not yet know if it will be used.
            # Add new group-created variables
            gvars = item.variable_list()
            for gvar in gvars:
                stdname = gvar.get_prop_value('standard_name')
                if not stdname in self._gvar_stdnames:
                    self._gvar_stdnames[stdname] = item
                # End if
            # End for
        # End for

    def is_run_group(self, group):
        """Method to separate out run-loop groups from special initial
        and final groups
        """
        return (group.name not in self._beg_groups) and (group.name not in self._end_groups)

    def max_part_len(self):
        "What is the longest suite subroutine name?"
        maxlen = 0
        for spart in self.groups:
            if self.is_run_group(spart):
                maxlen = max(maxlen, len(spart.name))
            # End if
        # End for
        return maxlen

    def part_list(self):
        "Return list of run phase parts (groups)"
        parts = list()
        for spart in self.groups:
            if self.is_run_group(spart):
                parts.append(spart.name[len(self.name)+1:])
            # End if
        # End for
        return parts

    def phase_group(self, phase):
        "Return the (non-run) group specified by <phase>"
        if phase in self._full_phases:
            return self._full_phases[phase]
        else:
            raise ParseInternalError("Incorrect phase, '{}'".format(phase))
        # End if

    def write(self, output_dir, logger):
        """Create caps for all groups in the suite and for the entire suite
        (calling the group caps one after another)"""
        # Set name of module and filename of cap
        self._module = 'ccpp_{}_cap'.format(self.name)
        filename = '{module_name}.F90'.format(module_name=self._module)
        logger.debug('Writing CCPP suite file, {}'.format(filename))
        # Init
        module_use = None
        output_file_name = os.path.join(output_dir, filename)
        with FortranWriter(output_file_name, 'w') as outfile:
            # Write suite header
            outfile.write(COPYRIGHT, 0)
            outfile.write(Suite.__header__.format(module=self._module), 0)
            # Write module 'use' statements here
            outfile.write('use {}'.format(KINDS_MODULE), 1)
            outfile.write('implicit none\nprivate\n\n! Suite interfaces', 1)
            outfile.write(Suite.__state_machine_init__.format(css_var_name=Suite.__state_machine_var_name__, state=Suite.___state_machine_initial_state__), 1)
            for group in self._groups:
                outfile.write('public :: {}'.format(group.name), 1)
            # End for
            outfile.write('\n! Private suite variables', 1)
            for svar in self.keys():
                self[svar].write_def(outfile, 1, self, allocatable=True)
            # End for
            outfile.write('\ncontains', 0)
            for group in self._groups:
                if group.name in self._beg_groups:
                    group.write(outfile, logger, self._host_arg_list_noloop, 1,
                                suite_vars=self, allocate=True)
                elif group.name in self._end_groups:
                    group.write(outfile, logger, self._host_arg_list_noloop, 1,
                                suite_vars=self, deallocate=True)
                else:
                    group.write(outfile, logger, self._host_arg_list_full, 1)
                # End if
            # End for
            # Finish off the module
            outfile.write(Suite.__footer__.format(module=self._module), 0)
            return output_file_name

###############################################################################

class API(VarDictionary):

    __suite_fname__ = 'ccpp_physics_suite_list'
    __part_fname__  = 'ccpp_physics_suite_part_list'

    __header__ = '''
!>
!! @brief Auto-generated API for {host_model} calls to CCPP suites
!!
!
module {module}
'''
    __preamble__ = '''
{module_use}

implicit none
private

'''

    __sub_name_template__ = 'ccpp_physics'

    __subhead__ = 'subroutine {subname}({api_call_list})'

    __subfoot__ = 'end subroutine {subname}\n'

    __footer__ = '''
end module {module}
'''

    # Note, we cannot add these vars to our dictionary as we do not want
    #    them showing up in group dummy arg lists
    __suite_name__ = Var({'local_name':'suite_name',
                          'standard_name':'suite_name',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'',
                          'dimensions':'()'}, __api_source__)

    __suite_part__ = Var({'local_name':'suite_part',
                          'standard_name':'suite_part',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'',
                          'dimensions':'()'}, __api_source__)

    def __init__(self, sdfs, host_model, scheme_headers, logger):
        self._module        = 'ccpp_physics_api'
        self._host          = host_model
        self._suites        = list()
        super(API, self).__init__(self.module, parent_dict=host_model, logger=logger)
        self._host_arg_list_full = host_model.argument_list()
        self._host_arg_list_noloop = host_model.argument_list(loop_vars=False)
        # Create a usable library out of scheme_headers
        # Structure is dictionary of dictionaries
        # Top-level dictionary is keyed by function name
        # Secondary level is by phase
        scheme_library = {}
        for header in scheme_headers:
            if header.type != 'scheme':
                continue
            # End if
            func_id, ftrans, match_trans = CCPP_STATE_MACH.function_match(header.title)
            if func_id not in scheme_library:
                scheme_library[func_id] = {}
            # End if
            func_entry = scheme_library[func_id]
            if match_trans in func_entry:
                raise CCPPError("Duplicate scheme entry, {}".format(header.title))
            else:
                func_entry[match_trans] = header
            # End if
        # End for
        # Turn the SDF files into Suites
        for sdf in sdfs:
            suite = Suite(sdf, self, logger)
            suite.analyze(host_model, scheme_library, logger)
            self._suites.append(suite)
        # End for
        # Find DDTs for use statements
        host_vars = host_model.variable_list()
        self._host_ddt_list_full = ddt_modules(host_vars)
        host_vars = host_model.variable_list(loop_vars=False)
        self._host_ddt_list_noloop = ddt_modules(host_vars)
        # We will need the correct names for errmsg and errflg
        self._errmsg_var = host_model.find_variable('ccpp_error_message')
        if self._errmsg_var is None:
            raise CCPPError('Required variable, ccpp_error_message, not found')
        # End if
        self._errflg_var = host_model.find_variable('ccpp_error_flag')
        if self._errflg_var is None:
            raise CCPPError('Required variable, ccpp_error_flag, not found')
        # End if
        # We need a call list for every phase
        self.__call_lists = {}
        for phase in CCPP_STATE_MACH.transitions():
            self.__call_lists[phase] = CallList('API_' + phase, self._logger)
            self.__call_lists[phase].add_variable(self.suite_name_var)
            if phase == 'run':
                self.__call_lists[phase].add_variable(self.suite_part_var)
            # End if
            for suite in self._suites:
                for group in suite.groups:
                    if group.phase == phase:
                        self.__call_lists[phase].add_vars(group.call_list)
                    # End if
                # End for
             # End for
        # End for

    @property
    def module(self):
        '''Get the module name of the API.'''
        return self._module

    @property
    def suite_name_var(self):
        return type(self).__suite_name__

    @property
    def suite_part_var(self):
        return type(self).__suite_part__

    @property
    def suites(self):
        return self._suites

    @classmethod
    def interface_name(cls, phase):
        'Return the name of an API interface function'
        return "{}_{}".format(cls.__sub_name_template__, phase)

    def call_list(self, phase):
        "Return the appropriate API call list variables"
        if phase in self.__call_lists:
            return self.__call_lists[phase]
        else:
            raise ParseInternalError("Illegal phase, '{}'".format(phase))
        # End if

    def write(self, output_dir, logger):
        """Write CCPP API module"""
        if len(self.suites) == 0:
            raise CCPPError("No suite specified for generating API")
        # End if
        filename = os.path.join(output_dir, self.module + '.F90')
        api_filenames = list()
        module_use = 'use {}'.format(KINDS_MODULE)
        # Write out the suite files
        for suite in self.suites:
            out_file_name = suite.write(output_dir, logger)
            api_filenames.append(out_file_name)
        # End for
        return api_filenames

    def declare_inspection_interfaces(self, ofile):
        "Declare the API interfaces for the suite inquiry functions"
        ofile.write("public :: {}".format(API.__suite_fname__), 1)
        ofile.write("public :: {}".format(API.__part_fname__), 1)

    def get_errinfo_names(self):
        "Return a tuple of error output local names"
        errmsg_name = self._errmsg_var.get_prop_value('local_name')
        errflg_name = self._errflg_var.get_prop_value('local_name')
        return (errmsg_name, errflg_name)

    def write_inspection_routines(self, ofile):
        "Write the list_suites and list_suite_parts subroutines"
        errmsg_name, errflg_name = self.get_errinfo_names()
        ofile.write("subroutine {}(suites)".format(API.__suite_fname__), 1)
        nsuites = 0
        for suite in self.suites:
            nsuites = nsuites + 1
        # End for
        ofile.write("character(len=*), allocatable, intent(out) :: suites(:)", 2)
        ofile.write("\ninteger                                    :: sindex", 2)
        ofile.write("\nallocate(suites({}))".format(nsuites), 2)
        ofile.write("do sindex = 1, {}".format(nsuites), 2)
        for suite in self.suites:
            ofile.write("suites(sindex) = '{}'".format(suite.name), 3)
        # End for
        ofile.write("end do", 2)
        ofile.write("end subroutine {}".format(API.__suite_fname__), 1)
        # Write out the suite part list subroutine
        inargs = "suite_name, part_list, {errmsg}, {errflg}".format(errmsg=errmsg_name,
                                                                    errflg=errflg_name)
        ofile.write("\nsubroutine {}({})".format(API.__part_fname__, inargs), 1)
        ofile.write("character(len=*),              intent(in)  :: suite_name", 2)
        ofile.write("character(len=*), allocatable, intent(out) :: part_list(:)", 2)
        self._errmsg_var.write_def(ofile, 2, self)
        self._errflg_var.write_def(ofile, 2, self)
        ofile.write("\ninteger                                   :: pindex\n", 2)
        else_str = ''
        for suite in self.suites:
            ofile.write("{}if(trim(suite_name) == '{}') then".format(else_str, suite.name), 2)
            parts = suite.part_list()
            nparts = len(parts)
            ofile.write("allocate(part_list({}))\n".format(nparts), 3)
            ofile.write("do pindex = 1, {}".format(nparts), 3)
            for part in parts:
                ofile.write("part_list(pindex) = '{}'".format((part)), 4)
            # End for
            ofile.write("end do", 3)
            else_str = 'else '
        # End for
        ofile.write("else", 2)
        ofile.write("{errmsg} = 'No suite named '//trim(suite_name)//' found'".format(errmsg=errmsg_name), 3)
        ofile.write("{errflg} = 1".format(errflg=errflg_name), 3)
        ofile.write("end if", 2)
        ofile.write("end subroutine {}".format(API.__part_fname__), 1)
        # Finish off the module
        ofile.write(API.__footer__.format(module=self.module), 0)

# XXgoldyXX: v debug only
    def foo(self):
        # Write out the API module
        with FortranWriter(filename, 'w') as api:
            api.write(COPYRIGHT, 0)
            api.write(API.__header__.format(host_model=self._host.name,
                                            module=self.module), 0)
            api.write(API.__preamble__.format(module_use=module_use), 1)
            # Declare the API interfaces for each stage
            for phase in CCPP_STATE_MACH.transitions():
                api.write("public :: ccpp_physics_{}".format(phase), 1)
            # End for
            # Declare the API interfaces for the suite inquiry functions
            api.write("public :: {}".format(API.__suite_fname__), 1)
            api.write("public :: {}".format(API.__part_fname__), 1)
            api.write("\ncontains\n", 0)
            # Write the module body
            max_suite_len = 0
            for suite in self.suites:
                max_suite_len = max(max_suite_len, len(suite.module))
            # End for
            for phase in CCPP_STATE_MACH.transitions():
                host_call_list = self.suite_name_var.get_prop_value('local_name')
                if phase == 'run':
                    host_call_list = host_call_list + ', ' + self.suite_part_var.get_prop_value('local_name')
                    hal = self._host_arg_list_full
                    hddt = self._host_ddt_list_full
                else:
                    hal = self._host_arg_list_noloop
                    hddt = self._host_ddt_list_noloop
                # End if
                host_call_list = host_call_list + ", " + hal
                host_call_list = host_call_list + ', '.join(self.prop_list('local_name'))
                subname = API.interface_name(phase)
                api.write(API.__subhead__.format(subname=subname, host_call_list=host_call_list), 1)
                # Write out any use statements
                mlen = max([len(x[0]) for x in hddt]) if len(hddt) > 0 else 0
                mlen = max(mlen, max_suite_len)
                for suite in self.suites:
                    mspc = (mlen - len(suite.module))*' '
                    if phase == 'run':
                        for spart in suite.groups:
                            if suite.is_run_group(spart):
                                api.write("use {}, {}only: {}".format(suite.module, mspc, spart.name), 2)
                            # End if
                        # End for
                    else:
                        api.write("use {}, {}only: {}_{}".format(suite.module, mspc, suite.name, phase), 2)
                    # End if
                # End for
                for ddt in hddt:
                    mspc = (mlen - len(ddt[0]))*' '
                    api.write("use {}, {}only: {}".format(ddt[0], mspc, ddt[1]), 2)
                # End for
                # Dclare dummy arguments
                self.suite_name_var.write_def(api, 2, self)
                if phase == 'run':
                    self.suite_part_var.write_def(api, 2, self)
                # End if
                for var in self._host.variable_list():
                    stdname = var.get_prop_value('standard_name')
                    if (phase=='run') or (not VarDictionary.loop_var_match(stdname)):
                        var.write_def(api, 2, self)
                    # End if
                # End for
                self.declare_variables(api, 2, loop_vars=(phase=='run'))
                self.call_list(phase).declare_variables(api, 2)
                # Now, add in cases for all suite parts
                else_str = '\n'
                for suite in self.suites:
                    api.write("{}if (trim(suite_name) == '{}') then".format(else_str, suite.name), 2)
                    if phase == 'run':
                        el2_str = ''
                        for spart in suite.groups:
                            if suite.is_run_group(spart):
                                pname = spart.name[len(suite.name)+1:]
                                api.write("{}if (trim(suite_part) == '{}') then".format(el2_str, pname), 3)
                                call_str = spart.call_list.call_string()
                                api.write("call {}({})".format(spart.name, call_str), 4)
                                el2_str = 'else '
                            # End if
                        # End for
                        api.write("else", 3)
                        api.write("{errmsg} = 'No suite part named '//trim(suite_part)".format(errmsg=errmsg_name), 4)
                        api.write("{errmsg} = trim({errmsg})//' found in suite {sname}'".format(errmsg=errmsg_name, sname=suite.name), 4)
                        api.write("{errflg} = 1".format(errflg=errflg_name), 4)
                        api.write("end if", 3)
                    else:
                        call_str = suite.phase_group(phase).call_list.call_string()
                        api.write("call {}_{}({})".format(suite.name, phase, call_str), 3)
                    # End if
                    else_str = 'else '
                # End for
                api.write("else", 2)
                api.write("{errmsg} = 'No suite named '//trim(suite_name)//' found'".format(errmsg=errmsg_name), 3)
                api.write("{errflg} = 1".format(errflg=errflg_name), 3)
                api.write("end if", 2)
                api.write(API.__subfoot__.format(subname=subname), 1)
            # End for
            # Write the list_suites subroutine
            api.write("subroutine {}(suites)".format(API.__suite_fname__), 1)
            nsuites = 0
            for suite in self.suites:
                nsuites = nsuites + 1
            # End for
            api.write("character(len=*), allocatable, intent(out) :: suites(:)", 2)
            api.write("\ninteger                                    :: sindex", 2)
            api.write("\nallocate(suites({}))".format(nsuites), 2)
            api.write("do sindex = 1, {}".format(nsuites), 2)
            for suite in self.suites:
                api.write("suites(sindex) = '{}'".format(suite.name), 3)
            # End for
            api.write("end do", 2)
            api.write("end subroutine {}".format(API.__suite_fname__), 1)
            # Write out the suite part list subroutine
            inargs = "suite_name, part_list, {errmsg}, {errflg}".format(errmsg=errmsg_name,
                                                                        errflg=errflg_name)
            api.write("\nsubroutine {}({})".format(API.__part_fname__, inargs), 1)
            api.write("character(len=*),              intent(in)  :: suite_name", 2)
            api.write("character(len=*), allocatable, intent(out) :: part_list(:)", 2)
            self._errmsg_var.write_def(api, 2, self)
            self._errflg_var.write_def(api, 2, self)
            api.write("\ninteger                                   :: pindex\n", 2)
            else_str = ''
            for suite in self.suites:
                api.write("{}if(trim(suite_name) == '{}') then".format(else_str, suite.name), 2)
                parts = suite.part_list()
                nparts = len(parts)
                api.write("allocate(part_list({}))\n".format(nparts), 3)
                api.write("do pindex = 1, {}".format(nparts), 3)
                for part in parts:
                    api.write("part_list(pindex) = '{}'".format((part)), 4)
                # End for
                api.write("end do", 3)
                else_str = 'else '
            # End for
            api.write("else", 2)
            api.write("{errmsg} = 'No suite named '//trim(suite_name)//' found'".format(errmsg=errmsg_name), 3)
            api.write("{errflg} = 1".format(errflg=errflg_name), 3)
            api.write("end if", 2)
            api.write("end subroutine {}".format(API.__part_fname__), 1)
            # Finish off the module
            api.write(API.__footer__.format(module=self.module), 0)
        # End with
        api_filenames.append(filename)
        return api_filenames

###############################################################################
if __name__ == "__main__":
    from parse_tools import init_log, set_log_to_null
    logger = init_log('ccpp_suite')
    set_log_to_null(logger)
    try:
        # First, run doctest
        import doctest
        doctest.testmod()
        frame_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        cpf = os.path.dirname(frame_root)
        kessler = os.path.join(cpf, 'cam_driver', 'suites',
                               'suite_cam_kessler_test_simple1.xml')
        if os.path.exists(kessler):
            foo = Suite(kessler, VarDictionary('foo'), logger)
        else:
            raise CCPPError("Cannot find test file, '{}'".format(kessler))
    except CCPPError as sa:
        print("{}".format(sa))
# No else
