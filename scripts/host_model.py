#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
import sys
import os
import os.path
import re
import subprocess
import xml.etree.ElementTree as ET
# CCPP framework imports
from metavar import Var, VarDDT, VarDictionary
from parse_tools import ParseSource, ParseContext, CCPPError, ParseInternalError
from parse_tools import read_xml_file, validate_xml_file, find_schema_version
from parse_tools import context_string, check_fortran_intrinsic, FORTRAN_ID

###############################################################################
class HostModel(VarDictionary):
    "Class to hold the data from a host model"

    def __init__(self, meta_headers, name_in, logger):
        self._name = name_in
        self._ddt_defs = {}      # DDT definition headers
        self._ddt_vars = {}      # DDT variable to DDT map
        self._ddt_fields = {}    # DDT field to DDT access map
        self._var_locations = {} # Local name to module map
        # Process the headers by type
        varlist = list()
        while len(meta_headers) > 0:
            header = meta_headers.pop()
            title = header.title
            if logger is not None:
                msg = 'Adding {} {} to host model'
                logger.debug(msg.format(header.header_type, title))
            # End if
            if header.header_type == 'ddt':
                if title in self._ddt_defs:
                    errmsg = "Duplicate DDT, {}, found{}, original{}"
                    ctx = context_string(header.source.context)
                    octx = context_string(self._ddt_defs[title].source.context)
                    raise CCPPError(errmsg.format(title, ctx, octx))
                else:
                    self._ddt_defs[title] = header
                # End if
            elif header.header_type == 'module':
                varlist.extend(header.variable_list())
                # Set the variable modules
                modname = header.title
                for var in header.variable_list():
                    lname = var.get_prop_value('local_name')
                    self._var_locations[lname] = modname
            elif header.header_type == 'host':
                if self._name is None:
                    # Grab the first host name we see
                    self._name = header.name
                varlist.extend(header.variable_list())
            else:
                errmsg = "Invalid host model metadata header, {} ({})"
                raise CCPPError(errmsg.format(header.title, header.header_type))
            # End if
        # End while
        if self.name is None:
            errmsg = 'No name found for host model, add a host metadata entry'
            raise CCPPError(errmsg)
        # Initialize variable dictionary
        super(HostModel, self).__init__(self.name, variables=varlist, logger=logger)
        # Make sure we have a DDT definition for every DDT variable
        self.check_ddt_vars(logger)
        self.collect_ddt_fields(logger)

    @property
    def name(self):
        'Return the host model name'
        return self._name

    def argument_list(self, loop_vars=True):
        'Return a string representing the host model variable arg list'
        args = self.prop_list('local_name', loop_vars=loop_vars)
        return ', '.join(args)

    def add_ddt_defs(new_ddt_defs, logger=None):
        "Add new DDT metadata definitions to model"
        if new_ddt_defs is not None:
            for header in new_ddt_defs:
                if header.title in self._ddt_defs:
                    raise CCPPError("Duplicate DDT, {}, passed to add_ddt_defs".format(header.title))
                else:
                    if logger is not None:
                        logger.debug('Adding ddt, {}'.format(header.title))
                    # End if
                    self._ddt_defs[header.title] = header
                # End if
            # End for
            # Make sure we have a DDT definition for every DDT variable
            self.check_ddt_vars()
            self.collect_ddt_fields()
        # End if

    def add_variables(new_variables, logger=None):
        "Add new variables definitions to model"
        if new_variables is not None:
            self.merge(new_variables)
        # End if

    def check_ddt_vars(self, logger=None):
        "Check that we have a DDT definition for every DDT variable"
        for vkey in self.keys():
            var = self[vkey]
            vtype = var.get_prop_value('type')
            if not check_fortran_intrinsic(vtype):
                vkind = var.get_prop_value('kind')
                stdname = var.get_prop_value('standard_name')
                if stdname in self._ddt_vars:
                    # Make sure this is not a duplicate
                    if self._ddt_vars[stdname] != self._ddt_defs[vkind]:
                        raise CCPPError("Duplicate DDT definition for {}".format(vkind))
                    # End if
                else:
                    self._ddt_vars[stdname] = self._ddt_defs[vkind]
                # End if
            # End if (no else, intrinsic types)
        # End for

    def collect_fields_from_ddt(self, ddt, source, logger=None):
        "Collect all the reachable fields from one DDT definition"
        for var in ddt.variable_list():
            vtype = var.get_prop_value('type')
            stdname = var.get_prop_value('standard_name')
            if stdname in self._ddt_fields:
                src = [x.get_prop_name('kind') for x in source]
                raise CCPPError("Duplicate DDT standard name, {}, from {}".format(stdname), src)
            else:
                # Record this intrinsic variable
                self._ddt_fields[stdname] = VarDDT(stdname, source + [var], logger)
            # End if
        # End for

    def collect_ddt_fields(self, logger=None):
        "Make sure we know the standard names of all reachable fields"
        for stdname in self._ddt_vars.keys():
            # The source for the fields in this DDT is the variable
            ddt = self._ddt_vars[stdname]
            svar = super(HostModel, self).find_variable(stdname)
            if svar is None:
                raise ParseInternalError('No host variable for DDT {} in {}'.format(stdname, self.name))
            # End if
            self.collect_fields_from_ddt(ddt, [svar], logger)
        # End for

    def host_variable_module(self, local_name):
        "Return the module name for a host variable"
        if local_name in self._var_locations:
            return self._var_locations[local_name]
        else:
            return None
        # End if

    def variable_locations(self):
        """Return a set of module-variable and module-type pairs.
        These represent the locations of all host model data with a listed
        source location (variables with no <module> source are omitted)."""
        varset = set()
        lnames = self.prop_list('local_name')
        for name in lnames:
            module = self.host_variable_module(name)
            if (module is not None) and (len(module) > 0):
                varset.add((module, name))
            # No else, either no module or a zero-length module name
            # End if
        # End for
        return varset

    def find_variable(self, standard_name, any_scope=False, loop_subst=False):
        """Return the host model variable matching <standard_name> or None
        If loop_subst is True, substitute a begin:end range for an extent.
        """
        my_var = super(HostModel, self).find_variable(standard_name,
                                                      any_scope=any_scope)
        if (my_var is None) and (standard_name in self._ddt_fields):
            # Found variable in a DDT element
            my_var = self._ddt_fields[standard_name]
        # End if
        if loop_subst:
            if my_var is None:
                my_var = self.find_loop_subst(standard_name)
            # End if
            if my_var is not None:
                # If we get here, the host does not have the requested
                # variable but does have a replacement set. Create a new
                # variable to use to send to suites.
                new_name = self.new_internal_variable_name(prefix=self.name)
                new_var = my_var.clone(new_name, source_name=self.name,
                                       source_type="HOST",
                                       context=ParseContext(filename='host_model.py'))
                self.add_variable(new_var)
                my_var = new_var
# XXgoldyXX: v debug only
                raise ValueError('New host variable, {}'.format(my_var))
# XXgoldyXX: ^ debug only
            # End if
        # End if
        return my_var

    def add_host_variable_module(self, local_name, module, logger=None):
        "Add a module name location for a host variable"
        if local_name in self._var_locations:
            raise CCPPError("Host variable, {}, already located in module".format(self._var_locations[local_name]))
        else:
            if logger is not None:
                logger.debug('Adding variable, {}, from module, {}'.format(local_name, module))
            # End if
            self._var_locations[local_name] = module
        # End if

    def call_list(self, phase):
        "Return the list of variables passed by the host model to the host cap"
        hdvars = list()
        loop_vars = phase == 'run'
        for hvar in self.variable_list(loop_vars=loop_vars):
            lname = hvar.get_prop_value('local_name')
            if self.host_variable_module(lname) is None:
                hdvars.append(hvar)
            # End if
        # End for
        return hdvars

###############################################################################

if __name__ == "__main__":
    from parse_tools import init_log, set_log_to_null
    logger = init_log('host_registry')
    set_log_to_null(logger)
    # First, run doctest
    import doctest
    doctest.testmod()
# No else:
