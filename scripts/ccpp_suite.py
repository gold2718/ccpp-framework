#!/usr/bin/env python
#

"""Classes and methods to create a Fortran suite-implementation file
to implement calls to a set of suites for a given host model."""

# Python library imports
from __future__ import print_function
import os.path
import re
import xml.etree.ElementTree as ET
# CCPP framework imports
from parse_tools   import ParseContext, ParseSource, context_string
from parse_tools   import ParseInternalError, CCPPError
from parse_tools   import read_xml_file, validate_xml_file, find_schema_version
from metavar       import Var, VarDDT, VarDictionary, VarLoopSubst, ddt_modules
from metavar       import ccpp_standard_var, CCPP_CONSTANT_VARS
from state_machine import StateMachine
from fortran_tools import FortranWriter

# pylint: disable=too-many-lines

###############################################################################
# Module (global) variables
###############################################################################

__init_st__ = r"(?:(?i)init(?:ial(?:ize)?)?)"
__final_st__ = r"(?:(?i)final(?:ize)?)"
__run_st__ = r"(?:(?i)run)"
__ts_init_st__ = r"(?:(?i)timestep_init(?:ial(?:ize)?)?)"
__ts_final_st__ = r"(?:(?i)timestep_final(?:ize)?)"

OBJ_LOC_RE = re.compile(r"(0x[0-9A-Fa-f]+)>")

# Source for internally generated variables.
__api_context__ = ParseContext(filename="ccpp_suite.py")
__api_source__ = ParseSource("CCPP_API", "scheme", __api_context__)
__api_local__ = ParseSource("CCPP_API", "local", __api_context__)

# Allowed CCPP transitions
# pylint: disable=bad-whitespace
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
# pylint: enable=bad-whitespace

# Required variables for inclusion in auto-generated schemes
CCPP_REQUIRED_VARS = [ccpp_standard_var('ccpp_error_flag', 'scheme',
                                        context=__api_context__),
                      ccpp_standard_var('ccpp_error_message', 'scheme',
                                        context=__api_context__)]

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

    def call_string(self, cldict=None):
        """Return a dummy argument string for this call list.
        <cldict> may be another VarDictionary object from which to retrieve
        local_names (default is to use self).
        """
        arg_str = ""
        for var in self.variable_list():
            # Do not include constants
            stdname = var.get_prop_value('standard_name')
            if stdname not in CCPP_CONSTANT_VARS:
                if cldict is not None:
                    dvar = cldict.find_variable(stdname)
                    if dvar is None:
                        errmsg = "Variable, '{}', not found in {}"
                        raise CCPPError(errmsg.format(stdname, cldict.name))
                    else:
                        lname = dvar.get_prop_value('local_name')
                    # End if
                else:
                    lname = var.get_prop_value('local_name')
                # End if
                if arg_str:
                    arg_str = arg_str + ", "
                # End if
                arg_str = arg_str + lname
            # End if
        # End for
        return arg_str

###############################################################################

class SuiteObject(VarDictionary):
    """Base class for all CCPP Suite objects (e.g., Scheme, Subcycle)
    SuiteObjects have an internal dictionary for variables created for
    execution of the SuiteObject. These variables will be allocated and
    managed at the Group level (unless cross-group usage or persistence
    requires handling at the Suite level).
    SuiteObjects also have a call list which is a list of variables which
    are passed to callable SuiteObjects (e.g., Scheme).
    """

    def __init__(self, name, context, parent, logger,
                 active_call_list=False, variables=None):
        # pylint: disable=too-many-arguments
        self.__name = name
        self._context = context
        self._logger = logger
        self._parent = parent
        if active_call_list:
            self._call_list = CallList(name + '_call_list', logger)
        else:
            self._call_list = None
        # End if
        self._parts = list()
        self._needs_vertical = None
        self._needs_horizontal = None
        # Initialize our dictionary
        super(SuiteObject, self).__init__(self.name, variables=variables,
                                          parent_dict=parent)

    def declarations(self):
        """Return a list of local variables to be declared in parent Group
        or Suite. By default, this list is the object's embedded VarDictionary.
        """
        return self.variable_list()

    def add_part(self, item, replace=False):
        """Add an object (e.g., Scheme, Subcycle) to this SuiteObject.
        If <item> needs to be in a VerticalLoop, look for an appropriate
        VerticalLoop object or create one.
        if <replace> is True, replace <item> in its current position in self.
        Note that if <item> is not to be inserted in a VerticalLoop,
        <replace> has no effect.
        """
        if replace:
            if item in self._parts:
                index = self._parts.index(item)
            else:
                emsg = 'Cannot replace {} in {}, not a member'
                raise ParseInternalError(emsg.format(item.name, self.name))
            # End if
        else:
            if item in self._parts:
                emsg = 'Cannot add {} to {}, already a member'
                raise ParseInternalError(emsg.format(item.name, self.name))
            else:
                index = len(self._parts)
            # End if
        # End if
        # Does this item need to be in a VerticalLoop?
        if item.needs_vertical is not None:
            iparent = item.parent
            if isinstance(self, VerticalLoop):
                # It is being added to a VerticalLoop, call it good
                pass
            elif isinstance(iparent, VerticalLoop):
                # Why are we doing this?
                emsg = ('Trying to add {} {} to {} {} but it is already '
                        'in VerticalLoop {}')
                raise ParseInternalError(emsg.format(item.__class__.__name__,
                                                     item,name,
                                                     self.__class__.__name__,
                                                     self.name, iparent.name))
            else:
                pitem = iparent.part(-1, error=False)
                if isinstance(pitem, VerticalLoop):
                    # Can we attach item to this loop?
                    if pitem.dimension_name == item.needs_vertical:
                        pitem.add_part(item)
                        if replace:
                            self.remove_part(index)
                        # End if (no else, we already added it)
                    else:
                        # Need to add item to a new VerticalLoop
                        new_vl = VerticalLoop(item.needs_vertical,
                                              self._context, self,
                                              self._logger, items=[item])
                        if replace:
                            self.remove_part(index)
                        # End if (no else, adding the loop below)
                        # Add item to new loop
                        new_vl.add_part(item)
                        self._parts.insert(index, new_vl)
                    # End if
                # End if
            # End if
        else:
            # Just add <item>
            self._parts.insert(index, item)
            item.reset_parent(self)
        # End if

    def remove_part(self, index):
        'Remove the part at index'
        plen = len(self._parts)
        if ((index >= 0) and (index < plen)) or (abs(index) <= plen):
            del self._parts[index]
        else:
            errmsg = "Invalid index for remove_part, {}, ".format(index)
            if plen > 0:
                errmsg += "SuiteObject only has {} parts".format(plen)
            else:
                errmsg += "SuiteObject only has no parts"
            raise ParseInternalError(errmsg, context=self._context)
        # End if

    def schemes(self):
        "Return a flattened list of schemes for this group"
        schemes = list()
        for item in self._parts:
            schemes.extend(item.schemes())
        # End for
        return schemes

    def move_part(self, part, source_object, loc=-1):
        """Operator to move <part> from <source_object> to <self>.
        If <loc> is -1, <part> is appended to <self>,
        otherwise, <part> is inserted at <loc>.
        """
        if part in source_object.parts:
            # Sanitize loc
            try:
                iloc = int(loc)
            except ValueError:
                errmsg = "Invalid loc value for move_part, {}".format(loc)
                raise ParseInternalError(errmsg, context=self._context)
            # End try
            if iloc == -1:
                self._parts.append(part)
            else:
                self._parts.insert(iloc, part)
            # End if
            index = source_object.index(part)
            source_object.remove_part(index)
            # <part> now has a new parent
            part.reset_parent(self)

    def reset_parent(self, new_parent):
        'Reset the parent of this SuiteObject (which has been moved)'
        self._parent = new_parent

    def register_action(self, vaction):
        """Register (i.e., save information for processing during write stage)
        <vaction> and return True or pass <vaction> up to the parent of
        <self>. Return True if any level registers <vaction>, False otherwise.
        The base class will not register any action, it must be registered in
        an override of this method.
        """
        if self.parent is not None:
            return self.parent.register_action(vaction)
        # End if
        return False

    def add_call_list_variable(self, newvar, exists_ok=False, gen_unique=False):
        """Add <newvar> to this SuiteObject's call_list. If this SuiteObject
        does not have a call list, recursively try the SuiteObject's parent
        Do not add <newvar> if it exists as a local variable."""
        stdname = newvar.get_prop_value('standard_name')
        if self.call_list is not None:
            self.call_list.add_variable(newvar, exists_ok=exists_ok,
                                        gen_unique=gen_unique)
        elif self.parent is None:
            errmsg = 'No call_list found for {}'.format(newvar)
            raise ParseInternalError(errmsg)
        elif self.parent.find_variable(stdname, any_scope=False):
            pass # We have the variable, no need to add it to a call list
        else:
            self.parent.add_call_list_variable(newvar, exists_ok=exists_ok,
                                               gen_unique=gen_unique)
        # End if

    def add_variable_to_call_tree(self, var, vmatch=None):
        """Add <var> to <self>'s call_list (or a parent if <self> does not
        have an active call_list).
        if <vmatch> is not None, also add the loop substitution variables
        which must be present.
        """
        found_dims = False
        if var is not None:
            self.add_call_list_variable(var, exists_ok=True)
            found_dims = True
        # End if
        if vmatch is not None:
            svars = vmatch.has_subst(self, any_scope=True)
            if svars is None:
                found_dims = False
            else:
                found_dims = True
                for svar in svars:
                    self.add_call_list_variable(svar, exists_ok=True)
                # End for
                # Register the action
                self.register_action(vmatch)
            # End if
        # End if
        return found_dims

    def horiz_dim_match(self, ndim, hdim, nloop_subst):
        """Find a match between <ndim> and <hdim>, if they are both
        horizontal dimensions.
        If <ndim> == <hdim>, return <ndim>.
        If <nloop_subst> is not None and its required standard names exist
        in our extended dictionary, return them.
        Otherwise, return None.
        """
        dim_match = None
        nis_hdim = Var.is_horizontal_dimension(ndim)
        his_hdim = Var.is_horizontal_dimension(hdim)
        if nis_hdim and his_hdim:
            if ndim == hdim:
                dim_match = ndim
            elif nloop_subst is not None:
                svars = nloop_subst.has_subst(self, any_scope=True)
                match = svars is not None
                if match:
                    if isinstance(self, Scheme):
                        obj = self.parent
                    else:
                        obj = self
                    # End if
                    for svar in svars:
                        obj.add_call_list_variable(svar, exists_ok=True)
                    # End for
                    dim_match = ':'.join(nloop_subst.required_stdnames)
                # End if
            # End if (no else, there is no match)
        # End if (no else, there is no match)
        return dim_match

    def dimension_match(self, need_dims, have_dims):
        """Compare dimensions between <need_dims> and <have_dims>.
        A match is declared even if <have_dims> has a vertical dimension
        and <need_dims> does not (as long as all other dimensions match).
        Return True if all dims match.
        Return <need_dims> and <have_dims> modified, if necessary to
        reflect the available limits.
        Also return any horizontal VarLoopSubst matches that are needed
        >>> SuiteObject('foo', __api_context__, None, None).dimension_match(['horizontal_loop_extent'], ['horizontal_loop_extent'])
        (True, ['horizontal_loop_extent'], ['horizontal_loop_extent'])
        >>> SuiteObject('foo', __api_context__,None, None,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, __api_local__),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, __api_local__)],active_call_list=True).dimension_match(['ccpp_constant_one:horizontal_loop_extent'], ['ccpp_constant_one:horizontal_dimension'])
        (True, ['ccpp_constant_one:horizontal_loop_extent'], ['horizontal_loop_begin:horizontal_loop_end'])
        >>> SuiteObject('foo', __api_context__,None,None,variables=[Var({'local_name':'beg','standard_name':'horizontal_loop_begin','units':'count','dimensions':'()','type':'integer'}, __api_local__),Var({'local_name':'end','standard_name':'horizontal_loop_end','units':'count','dimensions':'()','type':'integer'}, __api_local__)],active_call_list=True).dimension_match(['ccpp_constant_one:horizontal_loop_extent'], ['horizontal_loop_begin:horizontal_loop_end'])
        (True, ['ccpp_constant_one:horizontal_loop_extent'], ['horizontal_loop_begin:horizontal_loop_end'])
        """
        new_need_dims = list(need_dims)
        new_have_dims = list(have_dims)
        match = True
        nlen = len(need_dims)
        hlen = len(have_dims)
        nhas_vdim, nvdim_index = Var.find_vertical_dimension(need_dims)
        hhas_vdim, hvdim_index = Var.find_vertical_dimension(have_dims)
        if (hvdim_index >= 0) and (nvdim_index < 0):
            nlen_check = nlen + 1
        else:
            nlen_check = nlen
        # End if
        if nlen_check != hlen:
            match = False
        else:
            for hindex in range(nlen):
                # Compare each dimension unless <need_dims> is missing
                # a vertical dimension.
                if nvdim_index >= 0:
                    nindex = hindex
                else:
                    if hindex == hvdim_index:
                        continue # Skip the missing dimension
                    elif hindex > hvdim_index:
                        nindex = hindex - 1
                    else:
                        nindex = hindex
                    # End if
                # End if
                if need_dims[nindex] != have_dims[hindex]:
                    # No match, look for a loop match
                    dim = need_dims[nindex]
                    vmatch = VarDictionary.loop_var_match(dim)
                    if vmatch is None:
                        match = False
                        break
                    else:
                        ldim = self.horiz_dim_match(need_dims[nindex],
                                                    have_dims[hindex], vmatch)
                        if ldim is not None:
                            # Fix the subroutine dummy argument
                            new_have_dims[hindex] = ldim
                            # Make sure we have the correct local variable
                            self.register_action(vmatch)
                        else:
                            match = False
                            break
                        # End if
                    # End if
                # End if
            # End for
        # End if
        return match, new_need_dims, new_have_dims

    def dimension_permute(self, need_dims, have_dims, loop_check=False):
        """Compare dimensions between <need_dims> and <have_dims>.
        Return permutation if the dimensions in <have_dims> are a
        permutation of the dimensions in <need_dims>, otherwise return None.
        The permutation is the index in <have_dims> for every dimension in
        <need_dims>
        """
        if loop_check:
            vmatch_list = [None]*len(need_dims)
        # End if
        new_dims = list(need_dims)
        if set(need_dims) == set(have_dims):
            perm = list()
            for ndim in need_dims:
                perm.append(have_dims.index(ndim))
            # End if
        elif loop_check and (len(need_dims) == len(have_dims)):
            perm = [None]*len(need_dims)
            for nindex, dim in enumerate(need_dims):
                try:
                    hindex = have_dims.index(dim)
                    perm[nindex] = hindex
                except ValueError:
                    # No match, look for a loop match
                    vmatch = VarDictionary.loop_var_match(dim)
                    if vmatch is None:
                        perm = None
                    else:
                        ldim = ':'.join(vmatch.required_stdnames)
                        try:
                            hindex = have_dims.index(ldim)
                            perm[nindex] = hindex
                            new_dims[nindex] = ldim
                            vmatch_list[nindex] = vmatch
                        except ValueError:
                            for hindex, hdim in enumerate(have_dims):
                                pdim = self.horiz_dim_match(ldim, hdim, vmatch)
                                if pdim is not None:
                                    perm[nindex] = hindex
                                    new_dims[nindex] = pdim
                                    vmatch_list[nindex] = vmatch
                                else:
                                    perm = None
                                    break
                                # End if
                            # End for
                        # End try
                    # End if (vmatch)
                # End try
                if perm is None:
                    break
                # End if
            # End for
        else:
            perm = None
        # End if
        if loop_check:
            return perm, new_dims, vmatch_list
        # End if
        return perm, new_dims

    def match_dimensions(self, need_dims, have_dims):
        """Attempt to find match for all the dimensions in need_dims.
        For a loop variable, a match can be created using a VarLoopSubst object.
        Return values are:
        match: True iff a matching variable was found
        perm: A permutation if the match is permuted
        new_need_dims: The dimension standard names to be used in a call
        new_have_dims: The dimension standard names to be used as input
        vmatches: Any VarLoopSubst objects needed to generate dimensions
        """
        # Note, should handle missing vertical dimension here
        args = self.dimension_match(need_dims, have_dims)
        match, new_need_dims, new_have_dims = args
        perm = None
        if not match:
            new_have_dims = have_dims
            args = self.dimension_permute(need_dims, have_dims, loop_check=True)
            perm, new_need_dims, vmatches = args
            match = perm is not None
        # End if
        return match, perm, new_need_dims, new_have_dims

    def find_variable(self, standard_name, any_scope=True, clone=False):
        """Find a matching variable to <var>, create a local clone (if
        <clone> is True), or return None.
        First search the SuiteObject's internal dictionary, then its
        call list, then any parent dictionary (if <any_scope> is True).
        <var> can be a Var object or a standard_name string.

        """
        if isinstance(standard_name, Var):
            stdname = standard_name.get_prop_value('standard_name')
        else:
            stdname = standard_name
        # End if
        # First, search our local dictionary
        local_var = super(SuiteObject, self).find_variable(stdname,
                                                           any_scope=False)
        if self.call_list is not None:
            call_var = self.call_list.find_variable(stdname, any_scope=False)
        else:
            call_var = None
        # End if
        if (local_var is None) and (call_var is None) and any_scope:
            # We do not have the variable, look to parents.
            call_var = super(SuiteObject, self).find_variable(stdname,
                                                              any_scope=True)
        # End if
        if local_var is not None:
            found_var = local_var
        elif call_var is not None:
            found_var = call_var
        elif clone:
            msg = "ERROR: SuiteObject variable clone is not implemented"
            raise ParseInternalError(msg)
        else:
            found_var = None
        # End if
        return found_var

    def match_variable(self, var, vstdname=None, vdims=None):
        """Try to find a source for <var> in this SuiteObject's dictionary
        tree. Several items are returned:
        found_var: True if a match was found
        vert_dim: The vertical dimension in <var>, or None
        call_dims: How this variable should be called (or None if no match)
        missing_vert: Vertical dim in parent but not in <var>
        perm: Permutation (XXgoldyXX: Not yet implemented)
        """
        if vstdname is None:
            vstdname = var.get_prop_value('standard_name')
        # End if
        if vdims is None:
            vdims = var.get_dimensions()
        # End if
        if not vdims:
            vmatch = VarDictionary.loop_var_match(vstdname)
        else:
            vmatch = None
        # End if
        found_var = False
        missing_vert = None
        new_vdims = list()
        var_vdim = None
        var_hdim = None
        # Does this variable exist in the calling tree?
        dict_var = self.find_variable(vstdname, any_scope=True)
        if dict_var is None:
            found_var = self.parent.add_variable_to_call_tree(dict_var,
                                                              vmatch=vmatch)
        else:
            # Check dimensions
            dict_dims = dict_var.get_dimensions()
            if vdims:
                args = self.parent.match_dimensions(vdims, dict_dims)
                match, perm, new_vdims, new_dict_dims = args
                if perm is not None:
                    raise CCPPError("Permuted indices are not yet supported")
                # End if
            else:
                new_vdims = list()
                new_dict_dims = dict_dims
                match = True
            # End if
            # Add the variable to the parent call tree
            subst_dict = {'dimensions':new_dict_dims}
            clone = var.clone(subst_dict)
            found_var = self.parent.add_variable_to_call_tree(clone)
            if not match:
                new_vdims = None
                found_var = False
                var_vdim = var.has_vertical_dimension(dims=vdims)
                var_hdim = var.has_horizontal_dimension(dims=vdims)
                dict_vdim = dict_var.has_vertical_dimension(dims=dict_dims)
                if (dict_vdim is not None) and (var_vdim is None):
                    missing_vert = dict_vdim
                elif dict_vdim != var_vdim:
                    emsg = "Vertical dimension mismatch, {} calling {}"
                    raise CCPPError(emsg.format(dict_vdim, var_vdim))
                # End if
            # End if
        # End if
        return found_var, var_vdim, new_vdims, missing_vert

    def in_process_split(self):
        "Find out if we are in a process-split region"
        proc_split = False
        obj = self
        while obj is not None:
            if isinstance(obj, ProcessSplit):
                proc_split = True
                break
            elif isinstance(obj, TimeSplit):
                break
            # End if (other object types do not change status)
            obj = obj.parent
        # End while
        return proc_split

    def part(self, index, error=True):
        """Return one of this SuiteObject's parts raise an exception, or,
        if <error> is False, just return None"""
        plen = len(self._parts)
        if ((index >= 0) and (index < plen)) or (abs(index) <= plen):
            return self._parts[index]
        elif error:
            errmsg = 'No part {} in {} {}'.format(index,
                                                  self.__class__.__name__,
                                                  self.name)
            raise ParseInternalError(errmsg)
        else:
            return None
        # End if

    @property
    def name(self):
        """Return the name of the element"""
        return self.__name

    @name.setter
    def name(self, value):
        """Set the name of the element if it has not been set"""
        if self.__name is None:
            self.__name = value
        else:
            errmsg = 'Attempt to change name of {} to {}'
            raise ParseInternalError(errmsg.format(self, value))
        # End if

    @property
    def parent(self):
        """Element parent (or none)"""
        return self._parent

    @property
    def call_list(self):
        "Return the SuiteObject's call_list"
        return self._call_list

    @property
    def parts(self):
        """Return a copy the component parts of this SuiteObject.
        Returning a copy allows for the part list to be changed during
        processing of the return value"""
        return self._parts[:]

    @property
    def needs_vertical(self):
        'Return the vertical dimension this SuiteObject is missing or None'
        return self._needs_vertical

    def __repr__(self):
        'Create a unique readable string for this Object'
        so_repr = super(SuiteObject, self).__repr__()
        olmatch = OBJ_LOC_RE.search(so_repr)
        if olmatch is not None:
            loc = ' at {}'.format(olmatch.group(1))
        else:
            loc = ""
        # End if
        return '<{} {}{}>'.format(self.__class__.__name__, self.name, loc)

    def __format__(self, spec):
        """Return a string representing the SuiteObject, including its children.
        <spec> is used between subitems.
        <ind_level> is the indent level for multi-line output.
        """
        if spec:
            sep = spec[0]
        else:
            sep = '\n'
        # End if
        try:
            ind_level = int(spec[1:])
        except (ValueError, IndexError):
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
        if subout:
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
        self._has_vertical_dimension = False
        self._group = None
        super(Scheme, self).__init__(name, context, parent,
                                     logger, active_call_list=True)

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        # Unused arguments are for consistent analyze interface
        # pylint: disable=unused-argument
        "Analyze the scheme's interface to prepare for writing"
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
            vstdname = var.get_prop_value('standard_name')
            vdims = var.get_dimensions()
            args = self.match_variable(var, vstdname=vstdname, vdims=vdims)
            found, vert_dim, new_dims, missing_vert = args
            if found:
                if not self.has_vertical_dim:
                    self._has_vertical_dimension = vert_dim is not None
                # End if
                # We have a match, make sure var is in call list
                if new_dims == vdims:
                    self.add_call_list_variable(var, exists_ok=True)
                else:
                    subst_dict = {'dimensions':new_dims}
                    clone = var.clone(subst_dict)
                    self.add_call_list_variable(clone, exists_ok=True)
                # End if
            else:
                if missing_vert is not None:
                    # This Scheme needs to be in a VerticalLoop
                    self._needs_vertical = missing_vert
                    break # Deal with this and come back
                elif var.get_prop_value('intent') == 'out':
                    if self._group is None:
                        errmsg = 'Group not defined for {}'.format(self.name)
                        raise ParseInternalError(errmsg)
                    else:
                        # We still need it in our call list
                        self.add_call_list_variable(var)
                        # The Group will manage this variable
                        self._group.manage_variable(var)
                    # End if
                else:
                    errmsg = 'Input argument for {}, {}, not found'
                    raise CCPPError(errmsg.format(self.name, vstdname))
                # End if
            # End if
        # End for
        if self._needs_vertical is not None:
            self.parent.add_part(self, replace=True)
            if isinstance(self.parent, VerticalLoop):
                # Retart the loop analysis
                scheme_mods = self.parent.analyze(phase, group, scheme_library,
                                                  suite_vars, level)
            # End if
        # End if
        return scheme_mods

    def write(self, outfile, logger, indent):
        # Unused arguments are for consistent write interface
        # pylint: disable=unused-argument
        "Write code to call this Scheme to <outfile>"
        my_args = self.call_list.call_string(cldict=self.parent)
        stmt = 'call {}({})'
        outfile.write(stmt.format(self._subroutine_name, my_args), indent)

    def schemes(self):
        'Return self as a list for consistency with subcycle'
        return [self]

    @property
    def has_vertical_dim(self):
        """Return True if at least one of this Scheme's variables has
        a vertical dimension (vertical_layer_dimension or
        vertical_level_dimension)
        """
        return self._has_vertical_dimension

    def __str__(self):
        'Create a readable string for this Scheme'
        return '<Scheme {}: {}>'.format(self.name, self._subroutine_name)

###############################################################################

class VerticalLoop(SuiteObject):
    """Class to call a group of schemes or scheme collections called in a
    loop over a vertical dimension"""

    def __init__(self, dim_name, context, parent, logger, items=None):
        self._dim_name = dim_name
        name = dim_name.lower().replace('dimension', 'index') # Iteration count
        self._loop = dim_name # Number of iterations
        lvar = parent.find_variable(self.loop, any_scope=True)
        if lvar is None:
            emsg = "VerticalLoop, {}, specifies {} iterations but {} not found"
            raise CCPPError(emsg.format(name, self.loop, self.loop))
        # End if
        super(VerticalLoop, self).__init__(name, context, parent, logger)
        # Add any items
        if not isinstance(items, list):
            if items is None:
                items = list()
            else:
                items = [items]
            # End if
        # End if
        for item in items:
            self.add_part(item)
        # End for

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        "Analyze the VerticalLoop's interface to prepare for writing"
        # Create a variable for the loop index
        self.add_variable(Var({'local_name':self.name,
                               'standard_name':'loop_variable',
                               'type':'integer', 'units':'count',
                               'dimensions':'()'}, __api_source__))
        # Handle all the suite objects inside of this subcycle
        scheme_mods = set()
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1)
            for smod in smods:
                scheme_mods.add(smod)
            # End for
        # End for
        return scheme_mods

    def write(self, outfile, logger, indent):
        "Write code for the vertical loop, including contents, to <outfile>"
        outfile.write('do {} = 1, {}'.format(self.name, self.loop), indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for item in self.parts:
            item.write(outfile, logger, indent+1)
        # End for
        outfile.write('end do', 2)

    @property
    def loop(self):
        """Return the loop variable local_name"""
        return self._loop

    @property
    def dimension_name(self):
        'Return the vertical dimension over which this VerticalLoop loops'
        return self._dim_name

###############################################################################

class Subcycle(SuiteObject):
    "Class to represent a subcycled group of schemes or scheme collections"

    def __init__(self, sub_xml, context, parent, logger):
        name = sub_xml.get('name', None) # Iteration count
        loop_extent = sub_xml.get('loop', "1") # Number of iterations
        # See if our loop variable is an interger or a variable
        try:
            loop_int = int(loop_extent) # pylint: disable=unused-variable
            self._loop = loop_extent
            self._loop_var_int = True
        except ValueError:
            self._loop_var_int = False
            lvar = parent.find_variable(self.loop, any_scope=True)
            if lvar is None:
                emsg = "Subcycle, {}, specifies {} iterations but {} not found"
                raise CCPPError(emsg.format(name, self.loop, self.loop))
            else:
                parent.add_call_list_variable(lvar)
            # End if
        # End try
        super(Subcycle, self).__init__(name, context, parent, logger)
        for item in sub_xml:
            new_item = new_suite_object(item, context, self, logger)
            self.add_part(new_item)
        # End for

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        "Analyze the Subcycle's interface to prepare for writing"
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
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1)
            for smod in smods:
                scheme_mods.add(smod)
            # End for
        # End for
        return scheme_mods

    def write(self, outfile, logger, indent):
        "Write code for the subcycle loop, including contents, to <outfile>"
        outfile.write('do {} = 1, {}'.format(self.name, self.loop), indent)
        # Note that 'scheme' may be a sybcycle or other construct
        for item in self.parts:
            item.write(outfile, logger, indent+1)
        # End for
        outfile.write('end do', 2)

    @property
    def loop(self):
        """Return the loop value or variable local_name"""
        lvar = self.find_variable(self.loop, any_scope=True)
        if lvar is None:
            emsg = "Subcycle, {}, specifies {} iterations but {} not found"
            raise CCPPError(emsg.format(self.name, self.loop, self.loop))
        else:
            lname = lvar.get_prop_value('local_name')
        # End if
        return lname

###############################################################################

class TimeSplit(SuiteObject):
    """Class to represent a group of processes to be computed in a time-split
    manner -- each parameterization or other construct is called with an
    state which has been updated from the previous step.
    """

    def __init__(self, sub_xml, context, parent, logger):
        super(TimeSplit, self).__init__('TimeSplit', context, parent, logger)
        for part in sub_xml:
            new_item = new_suite_object(part, context, self, logger)
            self.add_part(new_item)
        # End for

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        # Unused arguments are for consistent analyze interface
        # pylint: disable=unused-argument
        "Analyze the TimeSplit's interface to prepare for writing"
        # Handle all the suite objects inside of this group
        scheme_mods = set()
        for item in self.parts:
            smods = item.analyze(phase, group, scheme_library,
                                 suite_vars, level+1)
            for smod in smods:
                scheme_mods.add(smod)
            # End for
        # End for
        return scheme_mods

    def write(self, outfile, logger, indent):
        """Write code for this TimeSplit section, including contents,
        to <outfile>"""
        for item in self.parts:
            item.write(outfile, logger, indent+1)
        # End for

###############################################################################

class ProcessSplit(SuiteObject):
    """Class to represent a group of processes to be computed in a
    process-split manner -- all parameterizations or other constructs are
    called with the same state.
    NOTE: Currently a stub
    """

    def __init__(self, sub_xml, context, parent, logger):
        # Unused arguments are for consistent __init__ interface
        # pylint: disable=unused-argument
        super(ProcessSplit, self).__init__('ProcessSplit', context,
                                           parent, logger)
        raise CCPPError('ProcessSplit not yet implemented')

    def analyze(self, phase, group, scheme_library, suite_vars, level):
        # Unused arguments are for consistent analyze interface
        # pylint: disable=unused-argument
        "Analyze the ProcessSplit's interface to prepare for writing"
        # Handle all the suite objects inside of this group
        raise CCPPError('ProcessSplit not yet implemented')

    def write(self, outfile, logger, indent):
        """Write code for this ProcessSplit section, including contents,
        to <outfile>"""
        raise CCPPError('ProcessSplit not yet implemented')

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
            errmsg = "Bad transition argument to Group, '{}'"
            raise ParseInternalError(errmsg.format(transition))
        # End if
        self._transition = transition
        # Initialize the dictionary of variables internal to group
        super(Group, self).__init__(name, context, parent,
                                    logger, active_call_list=True)
        # Add the items but first make sure we know the process tpye for
        # the group (e.g., TimeSplit or ProcessSplit).
        if (transition == 'run') and ((len(group_xml) == 0) or
                                      (group_xml[0].tag not in
                                       Group.__process_types__)):
            # Default is TimeSplit
            tsxml = ET.fromstring(Group.__process_xml__['timesplit'])
            time_split = new_suite_object(tsxml, context, self, logger)
            add_to = time_split
            self.add_part(time_split)
        else:
            add_to = self
        # End if
        # Add the sub objects either directly to the Group or to the TimeSplit
        for item in group_xml:
            new_item = new_suite_object(item, context, add_to, logger)
            add_to.add_part(new_item)
        # End for
        self._local_schemes = set()
        self._host_vars = None
        self._host_ddts = None
        self._loop_var_matches = list()
        self._phase = None
        self._phase_check_stmts = list()
        self._set_state = None
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
        """If scheme_name matches the group phase, return the group and
            function ID. Otherwise, return None
        """
        fid, tid, _ = CCPP_STATE_MACH.transition_match(scheme_name,
                                                       transition=self.phase)
        if tid is not None:
            return self, fid
        # End if
        return None, None

    def move_to_call_list(self, standard_name):
        """Move a variable from the group internal dictionary to the call list.
        This is done when the variable, <standard_name>, will be allocated by
        the suite.
        """
        gvar = self.find_variable(standard_name, any_scope=False)
        if gvar is None:
            errmsg = "Group {}, cannot move {}, variable not found"
            raise ParseInternalError(errmsg.format(self.name, standard_name))
        else:
            self.add_call_list_variable(gvar, exists_ok=True)
            self.remove_variable(standard_name)
        # End if

    def register_action(self, vaction):
        """Register any recognized <vaction> type for use during self.write.
        Return True iff <vaction> is handled.
        """
        if isinstance(vaction, VarLoopSubst):
            self._loop_var_matches = vaction.add_to_list(self._loop_var_matches)
            # Add the missing dim
            vaction.add_local(self, __api_local__)
            return True
        # End if
        return False

    def manage_variable(self, newvar):
        """Add <newvar> to our local dictionary making necessary
        modifications to the variable properties so that it is
        allocated appropriately"""
        # Need new dictionary to eliminate unwanted properties (e.g., intent)
        vdims = newvar.get_dimensions()
        # Look for dimensions where we have a loop substitution and replace
        # with the correct size
        hdims = [x.missing_stdname for x in self._loop_var_matches]
        for index, dim in enumerate(vdims):
            newdim = None
            vert_index = None
            for subdim in dim.split(':'):
                if subdim in hdims:
                    # We have a loop substitution, find and replace
                    hindex = hdims.index(subdim)
                    names = self._loop_var_matches[hindex].required_stdnames
                    newdim = ':'.join(names)
                    break
                elif ('vertical' in subdim) and ('index' in subdim):
                    # We have a vertical index, replace with correct dimension
                    raise ParseInternalError("vertical index replace not implemented")
                # End if
            # End for
            if newdim is not None:
                vdims[index] = newdim
            # End if
        # End for
        prop_dict = {'standard_name':newvar.get_prop_value('standard_name'),
                     'local_name':newvar.get_prop_value('local_name'),
                     'type':newvar.get_prop_value('type'),
                     'kind':newvar.get_prop_value('kind'),
                     'units':newvar.get_prop_value('units'),
                     'dimensions':vdims}
        local_var = Var(prop_dict, __api_local__)
# XXgoldyXX: v debug only
        print('XXG {}: Adding {}({})'.format(self.name, newvar.get_prop_value('local_name'), vdims))
# XXgoldyXX: ^ debug only
        self.add_variable(local_var, exists_ok=True)


    def analyze(self, phase, suite_vars, scheme_library):
        "Analyze the Group's interface to prepare for writing"
        parent = self.parent
        self._phase = phase
        for item in self.parts:
            # Items can be schemes, subcycles or other objects
            # All have the same interface and return a set of module use
            # statements (lschemes)
            lschemes = item.analyze(phase, self, scheme_library,
                                    suite_vars, 1)
            for lscheme in lschemes:
                self._local_schemes.add(lscheme)
            # End for
        # End for
# XXgoldyXX: v debug only
#        # Add each scheme's variables either to our call list (field
#        # comes from the suite or the host model) or to our internal
#        # dictionary (field is defined in the group subroutine).
#        # Variable dimensions also count since they need a source.
#        for scheme in self.schemes():
#            # Sort the call list to handle scalars first (to capture
#            # dimension loop substitutions before dealing with them as
#            # vector dimensions
#            var_list = scheme.call_list.variable_list()
#            var_list.sort(key=lambda x: len(x.get_dimensions()))
#            has_vdim = scheme.has_vertical_dim
#            for cvar in var_list:
#                cstdname = cvar.get_prop_value('standard_name')
#                cdims = cvar.get_dimensions()
#                if not cdims:
#                    vmatch = VarDictionary.loop_var_match(cstdname)
#                else:
#                    vmatch = None
#                # End if
#                found_var = False
#                # Do we already have this variable?
#                local_var = self.find_variable(cstdname, any_scope=False)
#                if local_var is None:
#                    local_var = self.find_variable(cstdname, any_scope=True)
#                if local_var is not None:
#                    found_var = True
#                elif cvar.get_prop_value('intent') == 'out':
#                    self.add_variable(cvar, exists_ok=True)
#                elif vmatch is not None:
#                    svars = vmatch.has_subst(self, any_scope=True)
#                    if svars is None:
#                        found_var = False
#                    else:
#                        found_var = True
#                        for var in svars:
#                            self.add_call_list_variable(var, exists_ok=True)
#                        # End for
#                        # This variable should now be available in Group
#                        # But as a local variable
#                        lname = cvar.get_prop_value('local_name')
#                        prop_dict = {'standard_name':cstdname,
#                                     'local_name':lname,
#                                     'type':cvar.get_prop_value('type'),
#                                     'kind':cvar.get_prop_value('kind'),
#                                     'units':cvar.get_prop_value('units'),
#                                     'dimensions':cdims}
#                        local_var = Var(prop_dict, __api_local__)
#                        self.add_variable(local_var, exists_ok=True)
#                    # End if
#                else:
#                    found_var = False
#                # End if
#                if not found_var:
#                    intent = cvar.get_prop_value('intent')
#                    lname = cvar.get_prop_value('local_name')
#                    errmsg = ("{grp} / {sch} intent({int}) variable "
#                              "{lnam} has no source")
#                    raise CCPPError(errmsg.format(grp=self.name,
#                                                  sch=scheme.name,
#                                                  int=intent, lnam=cstdname))
#                # End if
#            # End for
#        # End for
# XXgoldyXX: ^ debug only
        self._phase_check_stmts = Suite.check_suite_state(phase)
        self._set_state = Suite.set_suite_state(phase)

    def write(self, outfile, logger, host_arglist, indent,
              suite_vars=None, allocate=False, deallocate=False):
        """Write code for this subroutine (Group), including contents,
        to <outfile>"""
        # Unused arguments are for consistent write interface
        # pylint: disable=unused-argument
        # group type for (de)allocation
        if 'timestep' in self._phase:
            group_type = 'timestep'
        else:
            group_type = 'run'
        # End if
        # First, write out the subroutine header
        subname = self.name
        call_list = self.call_list.call_string()
        outfile.write(Group.__subhead__.format(subname=subname, args=call_list),
                      indent)
        # Write out any use statements
        # Write out the scheme use statements
        for scheme in self._local_schemes:
            outfile.write(scheme, indent+1)
        # End for
        outfile.write('', 0)
        # Write out dummy arguments
        outfile.write('! Dummy arguments', indent+1)
        msg = 'Variables for {}: ({})'
        logger.debug(msg.format(self.name, self.call_list.variable_list()))
        self.call_list.declare_variables(outfile, indent+1)
        subpart_var_set = {}
        for item in [self] + self.parts:
            for var in item.declarations():
                lname = var.get_prop_value('local_name')
                if lname in subpart_var_set:
                    if subpart_var_set[lname].compatible(var):
                        pass # We already are going to declare this variable
                    else:
                        errmsg = "Duplicate suite part variable, {}"
                        raise ParseInternalError(errmsg.format(lname))
                    # End if
                else:
                    subpart_var_set[lname] = (var, item)
                # End if
            # End for
        # End for
        if subpart_var_set:
            outfile.write('\n! Local Variables', indent+1)
        # Write out local variables
        for key in subpart_var_set:
            var = subpart_var_set[key][0]
            spdict = subpart_var_set[key][1]
            var.write_def(outfile, indent+1, spdict)
        # End for
        outfile.write('', 0)
        # Check state machine
        verrflg = self.find_variable('ccpp_error_flag', any_scope=True)
        if verrflg is not None:
            errflg = verrflg.get_prop_value('local_name')
        else:
            errmsg = "No ccpp_error_flag variable for group, {}"
            raise CCPPError(errmsg.format(self.name))
        # End if
        verrmsg = self.find_variable('ccpp_error_message', any_scope=True)
        if verrmsg is not None:
            errmsg = verrmsg.get_prop_value('local_name')
        else:
            errmsg = "No ccpp_error_message variable for group, {}"
            raise CCPPError(errmsg.format(self.name))
        # End if
        for stmt in self._phase_check_stmts:
            text = stmt[0].format(errflg=errflg, errmsg=errmsg,
                                  funcname=self.name)
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
                                errmsg = "Dimension variable, {} not found{}"
                                ctx = context_string(self._context)
                                raise CCPPError(errmsg.format(dim, ctx))
                            else:
                                lname = dvar.get_prop_value('local_name')
                                rdparts.append(lname)
                            # End if
                        # End for
                        rdims.append(':'.join(rdparts))
                    # End for
                    alloc_str = ', '.join(rdims)
                    lname = svar.get_prop_value('local_name')
                    outfile.write("allocate({}({}))".format(lname, alloc_str),
                                  indent+1)
                # End if
            # End for
        # End if
        # Write any loop match calculations
        for vmatch in self._loop_var_matches:
            action = vmatch.write_action(self, dict2=self.call_list)
            if action:
                outfile.write(action, indent+1)
            # End if
        # End for
        # Write the scheme and subcycle calls
        for item in self.parts:
            item.write(outfile, logger, indent)
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
    """Class to hold, process, and output a CAP for an entire CCPP suite.
    The Suite includes initialization and finalization Group objects as
    well as a Group for every suite part."""

    __state_machine_initial_state__ = 'uninitialized'
    __state_machine_var_name__ = 'ccpp_suite_state'

    __header__ = '''
!>
!! @brief Auto-generated cap module for the CCPP suite
!!
!
module {module}
'''

    __state_machine_init__ = '''
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
        self._host_arg_list_full = None
        self._host_arg_list_noloop = None
        self._module = None
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
        """Get the name of the suite."""
        return self._name

    @property
    def sdf_name(self):
        """Get the name of the suite definition file."""
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
            errmsg_str = "write({errmsg}, '(3a)') "
            errmsg_str += "\"Invalid initial CCPP state, '\", " + css + ', '
            errmsg_str += "\"' in {funcname}\""
            check_stmts.append((errmsg_str, 2))
            check_stmts.append(("return", 2))
            check_stmts.append(("end if", 1))
        else:
            raise ParseInternalError("Unknown stage, '{}'".format(stage))
        # End if
        return check_stmts

    @classmethod
    def set_suite_state(cls, phase):
        "Return the code string to set the current suite state to <phase>"
        final = CCPP_STATE_MACH.final_state(phase)
        return ("ccpp_suite_state = '{}'".format(final), 1)

    def new_group(self, group_string, transition):
        """Create a new Group object from the a XML description"""
        if isinstance(group_string, str):
            gxml = ET.fromstring(group_string)
        else:
            gxml = group_string
        # End if
        group = Group(gxml, transition, self, self._context, self._logger)
        for svar in CCPP_REQUIRED_VARS:
            group.add_call_list_variable(svar)
        # End for
        self._full_groups[group.name] = group
        self._full_phases[group.phase] = group
        return group

    def parse(self):
        """Parse the suite definition file."""
        success = True

        _, suite_xml = read_xml_file(self._sdf_name, self._logger)
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
                self._groups.append(self.new_group(suite_item, 'run'))
            else:
                match_trans = CCPP_STATE_MACH.function_match(item_type)
                if match_trans is None:
                    raise CCPPError("Unknown CCPP suite component tag type, '{}'".format(item_type))
                elif match_trans in self._full_phases:
                    # Parse a suite-wide initialization scheme
                    scheme = Scheme(suite_item, self._context,
                                    self, self._logger)
                    self._full_phases[match_trans].add_item(scheme)
                else:
                    emsg = "Unhandled CCPP suite component tag type, '{}'"
                    raise ParseInternalError(emsg.format(match_trans))
                # End if
        # End for
        self._groups.append(self._timestep_final_group)
        self._groups.append(self._suite_final_group)
        return success

    @property
    def module(self):
        """Get the list of the module generated for this suite."""
        return self._module

    @property
    def groups(self):
        """Get the list of groups in this suite."""
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
                    emsg = ("Group, {}, claimed it had created {} "
                            "but variable was not found")
                    raise CCPPError(emsg.format(group.name, standard_name))
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

    def analyze(self, host_model, scheme_library):
        """Collect all information needed to write a suite file
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
        """
        # Collect all relevant schemes
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
                                                    pgroup, self._logger)
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
            lmsg = "Group {}, schemes = {}"
            self._logger.debug(lmsg.format(item.name,
                                           [x.name for x in item.schemes()]))
            item.analyze(phase, self, scheme_library)
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
        return ((group.name not in self._beg_groups) and
                (group.name not in self._end_groups))

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
        output_file_name = os.path.join(output_dir, filename)
        with FortranWriter(output_file_name, 'w') as outfile:
            # Write suite header
            outfile.write(COPYRIGHT, 0)
            outfile.write(Suite.__header__.format(module=self._module), 0)
            # Write module 'use' statements here
            outfile.write('use {}'.format(KINDS_MODULE), 1)
            outfile.write('implicit none\nprivate\n\n! Suite interfaces', 1)
            line = Suite.__state_machine_init__
            var_name = Suite.__state_machine_var_name__
            var_state = Suite.__state_machine_initial_state__
            outfile.write(line.format(css_var_name=var_name,
                                      state=var_state), 1)
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
    """Class representing the API for the CCPP framework.
    The API class organizes the suites for which CAPS will be generated"""

    __suite_fname__ = 'ccpp_physics_suite_list'
    __part_fname__ = 'ccpp_physics_suite_part_list'

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
        self._module = 'ccpp_physics_api'
        self._host = host_model
        self._suites = list()
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
            func_id, _, match_trans = CCPP_STATE_MACH.function_match(header.title)
            if func_id not in scheme_library:
                scheme_library[func_id] = {}
            # End if
            func_entry = scheme_library[func_id]
            if match_trans in func_entry:
                errmsg = "Duplicate scheme entry, {}"
                raise CCPPError(errmsg.format(header.title))
            else:
                func_entry[match_trans] = header
            # End if
        # End for
        # Turn the SDF files into Suites
        for sdf in sdfs:
            suite = Suite(sdf, self, logger)
            suite.analyze(host_model, scheme_library)
            self._suites.append(suite)
        # End for
        # Find DDTs for use statements
        host_vars = host_model.variable_list()
        self._host_ddt_list_full = ddt_modules(host_vars)
        host_vars = host_model.variable_list(loop_vars=False)
        self._host_ddt_list_noloop = ddt_modules(host_vars)
        # We will need the correct names for errmsg and errflg
        evar = host_model.find_variable('ccpp_error_message')
        subst_dict = {'intent':'out'}
        if evar is None:
            raise CCPPError('Required variable, ccpp_error_message, not found')
        else:
            self._errmsg_var = evar.clone(subst_dict)
        # End if
        evar = host_model.find_variable('ccpp_error_flag')
        if evar is None:
            raise CCPPError('Required variable, ccpp_error_flag, not found')
        else:
            self._errflg_var = evar.clone(subst_dict)
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
        """Get the module name of the API."""
        return self._module

    @property
    def suite_name_var(self):
        "Return the name of the variable specifying the suite to run"
        return self.__class__.__suite_name__

    @property
    def suite_part_var(self):
        "Return the name of the variable specifying the suite group to run"
        return self.__class__.__suite_part__

    @property
    def suites(self):
        "Return the list of this API's suites"
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
        if not self.suites:
            raise CCPPError("No suite specified for generating API")
        # End if
        api_filenames = list()
        # Write out the suite files
        for suite in self.suites:
            out_file_name = suite.write(output_dir, logger)
            api_filenames.append(out_file_name)
        # End for
        return api_filenames

    @classmethod
    def declare_inspection_interfaces(cls, ofile):
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
        emsg = "write({errmsg}, '(3a)')".format(errmsg=errmsg_name)
        emsg += "'No suite named ', trim(suite_name), ' found'"
        ofile.write(emsg, 3)
        ofile.write("{errflg} = 1".format(errflg=errflg_name), 3)
        ofile.write("end if", 2)
        ofile.write("end subroutine {}".format(API.__part_fname__), 1)

###############################################################################
if __name__ == "__main__":
    # pylint: disable=ungrouped-imports
    from parse_tools import init_log, set_log_to_null
    LOGGING = init_log('ccpp_suite')
    set_log_to_null(LOGGING)
    try:
        # First, run doctest
        import doctest
        doctest.testmod()
        FRAME_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        CPF = os.path.dirname(FRAME_ROOT)
        KESSLER = os.path.join(CPF, 'cam_driver', 'suites',
                               'suite_cam_kessler_test_simple1.xml')
        if os.path.exists(KESSLER):
            _ = Suite(KESSLER, VarDictionary('Kessler'), LOGGING)
        else:
            print("Cannot find test file, '{}', skipping test".format(KESSLER))
    except CCPPError as suite_error:
        print("{}".format(suite_error))
# End if (no else)
