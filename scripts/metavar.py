#!/usr/bin/env python
#
# Class to hold all information on a CCPP metadata variable
#

# Python library imports
from __future__ import print_function
import re
import xml.etree.ElementTree as ET
from collections import OrderedDict
# CCPP framework imports
from parse_tools import check_fortran_ref, check_fortran_type, context_string
from parse_tools import FORTRAN_DP_RE, FORTRAN_ID
from parse_tools import registered_fortran_ddt_name
from parse_tools import check_dimensions, check_cf_standard_name
from parse_tools import ParseContext, ParseSource
from parse_tools import ParseInternalError, ParseSyntaxError, CCPPError

###############################################################################
real_subst_re = re.compile(r"(.*\d)p(\d.*)")
list_re = re.compile(r"[(]([^)]*)[)]\s*$")

########################################################################
def standard_name_to_long_name(prop_dict, context=None):
########################################################################
    """Translate a standard_name to its default long_name
    >>> standard_name_to_long_name({'standard_name':'cloud_optical_depth_layers_from_0p55mu_to_0p99mu'})
    'Cloud optical depth layers from 0.55mu to 0.99mu'
    >>> standard_name_to_long_name({'local_name':'foo'}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No standard name to convert foo to long name
    >>> standard_name_to_long_name({}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No standard name to convert to long name
    >>> standard_name_to_long_name({'local_name':'foo'}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No standard name to convert foo to long name at foo.F90:3
    >>> standard_name_to_long_name({}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No standard name to convert to long name at foo.F90:3
    """
    # We assume that standar_name has been checked for validity
    # Make the first char uppercase and replace each underscore with a space
    if 'standard_name' in prop_dict:
        standard_name = prop_dict['standard_name']
        long_name = standard_name[0].upper() + re.sub("_", " ", standard_name[1:])
        # Next, substitute a decimal point for the p in [:digit]p[:digit]
        match = real_subst_re.match(long_name)
        while match is not None:
            long_name = match.group(1) + '.' + match.group(2)
            match = real_subst_re.match(long_name)
        # End while
    else:
        long_name = ''
        if 'local_name' in prop_dict:
            lname = ' {}'.format(prop_dict['local_name'])
        else:
            lname = ''
        # End if
        ctxt = context_string(context)
        raise CCPPError('No standard name to convert{} to long name{}'.format(lname, ctxt))
    # End if
    return long_name

########################################################################
def default_kind_val(prop_dict, context=None):
########################################################################
    """Choose a default kind based on a variable's type
    >>> default_kind_val({'type':'REAL'})
    'kind_phys'
    >>> default_kind_val({'type':'complex'})
    'kind_phys'
    >>> default_kind_val({'type':'double precision'})
    'kind_phys'
    >>> default_kind_val({'type':'integer'})
    ''
    >>> default_kind_val({'type':'character'})
    ''
    >>> default_kind_val({'type':'logical'})
    ''
    >>> default_kind_val({'local_name':'foo'}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No type to find default kind for foo
    >>> default_kind_val({}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No type to find default kind
    >>> default_kind_val({'local_name':'foo'}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No type to find default kind for foo at foo.F90:3
    >>> default_kind_val({}, context=ParseContext(linenum=3, filename='foo.F90')) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: No type to find default kind at foo.F90:3
    """
    if 'type' in prop_dict:
        vtype = prop_dict['type'].lower()
        if vtype == 'real':
            kind = 'kind_phys'
        elif vtype == 'complex':
            kind = 'kind_phys'
        elif FORTRAN_DP_RE.match(vtype) is not None:
            kind = 'kind_phys'
        else:
            kind = ''
        # End if
    else:
        kind = ''
        if 'local_name' in prop_dict:
            lname = ' {}'.format(prop_dict['local_name'])
        else:
            lname = ''
        # End if
        ctxt = context_string(context)
        raise CCPPError('No type to find default kind for {}{}'.format(lname, ctxt))
    # End if
    return kind

########################################################################

class VariableProperty(object):
    """Class to represent a single property of a metadata header entry
    >>> VariableProperty('local_name', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('standard_name', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('long_name', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('units', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('dimensions', list) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('type', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('kind', str) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('state_variable', str, valid_values_in=['True',   'False', '.true.', '.false.' ], optional_in=True, default_in=False) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('intent', str, valid_values_in=['in', 'out', 'inout']) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('optional', str, valid_values_in=['True',   'False', '.true.', '.false.' ], optional_in=True, default_in=False) #doctest: +ELLIPSIS
    <__main__.VariableProperty object at ...>
    >>> VariableProperty('local_name', str).name
    'local_name'
    >>> VariableProperty('standard_name', str).type
    <type 'str'>
    >>> VariableProperty('units', str).is_match('units')
    True
    >>> VariableProperty('units', str).is_match('UNITS')
    True
    >>> VariableProperty('units', str).is_match('type')
    False
    >>> VariableProperty('value', int, valid_values_in=[1, 2 ]).valid_value('2')
    2
    >>> VariableProperty('value', int, valid_values_in=[1, 2 ]).valid_value('3')

    >>> VariableProperty('value', int, valid_values_in=[1, 2 ]).valid_value('3', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Invalid value variable property, '3'
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('()')
    []
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x)')
    ['x']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('x')

    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x:y)')
    ['x:y']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,y:z)')
    ['w:x', 'y:z']
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,x:y:z)', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'x:y:z' is an invalid dimension range
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(x:3y)', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: '3y' is not a valid Fortran identifier
    """

    def __init__(self, name_in, type_in, valid_values_in=None, optional_in=False, default_in=None, default_fn_in=None, check_fn_in=None):
        self._name = name_in
        self._type = type_in
        if self._type not in [ bool, int, list, str ]:
            raise CCPPError("{} has invalid VariableProperty type, '{}'".format(name_in, type_in))
        # End if
        self._valid_values = valid_values_in
        self._optional = optional_in
        if self.optional:
            if (default_in is None) and (default_fn_in is None):
                raise CCPPError('default_in or default_fn_in is a required property for {} because it is optional'.format(name_in))
            if (default_in is not None) and (default_fn_in is not None):
                raise CCPPError('default_in and default_fn_in cannot both be provided')
            self._default = default_in
            self._default_fn = default_fn_in
        elif default_in is not None:
            raise CCPPError('default_in is not a valid property for {} because it is not optional'.format(name_in))
        elif default_in is not None:
            raise CCPPError('default_fn_in is not a valid property for {} because it is not optional'.format(name_in))
        self._check_fn = check_fn_in

    @property
    def name(self):
        'Return the name of the property'
        return self._name

    @property
    def type(self):
        'Return the type of the property'
        return self._type

    def get_default_val(self, prop_dict, context=None):
        if self._default_fn is not None:
            return self._default_fn(prop_dict, context)
        elif self._default is not None:
            return self._default
        else:
            ctxt = context_string(context)
            raise CCPPError('No default for variable property {}{}'.format(self.name, ctxt))
        # End if

    @property
    def optional(self):
        return self._optional

    def is_match(self, test_name):
        "Return True iff <test_name> is the name of this property"
        return self.name.lower() == test_name.lower()

    def valid_value(self, test_value, error=False):
        'Return True iff test_value is valid'
        valid_val = None
        if self.type is int:
            try:
                tv = int(test_value)
                if self._valid_values is not None:
                    if tv in self._valid_values:
                        valid_val = tv
                    else:
                        valid_val = None # i.e. pass
                else:
                    valid_val = tv
            except CCPPError:
                valid_val = None # Redundant but more expressive than pass
        elif self.type is list:
            if isinstance(test_value, str):
                match = list_re.match(test_value)
                if match is None:
                    tv = None
                else:
                    tv = [x.strip() for x in match.group(1).split(',')]
                    if (len(tv) == 1) and (len(tv[0]) == 0):
                        # Scalar
                        tv = list()
                    # End if
                # End if
            else:
                tv = test_value
            # End if
            if isinstance(tv, list):
                valid_val = tv
            elif isinstance(tv, tuple):
                valid_val = list(tv)
            else:
                valid_val = None
            # End if
            if (valid_val is not None) and (self._valid_values is not None):
                # Special case for lists, _valid_values applies to elements
                for item in valid_val:
                    if item not in self._valid_values:
                        valid_val = None
                        break
                    # End if
                # End for
            else:
                pass
        elif self.type is bool:
            if isinstance(test_value, str):
                valid_val = (test_value in ['True', 'False']) or (test_value.lower() in ['t', 'f', '.true.', '.false.'])
            else:
                valid_val = not not test_value
        elif self.type is str:
            if isinstance(test_value, str):
                if self._valid_values is not None:
                    if test_value in self._valid_values:
                        valid_val = test_value
                    else:
                        valid_val = None # i.e., pass
                else:
                    valid_val = test_value
                # End if
            # End if
        # End if
        # Call a check function?
        if valid_val and (self._check_fn is not None):
            valid_val = self._check_fn(valid_val, error=error)
        elif (valid_val is None) and error:
            raise CCPPError("Invalid {} variable property, '{}'".format(self.name, test_value))
        # End if
        return valid_val

###############################################################################

class Var(object):
    """ A class to hold a metadata variable
    >>> Var.get_prop('standard_name') #doctest: +ELLIPSIS
    <__main__.VariableProperty object at 0x...>
    >>> Var.get_prop('standard')

    >>> Var.get_prop('type').is_match('type')
    True
    >>> Var.get_prop('type').is_match('long_name')
    False
    >>> Var.get_prop('type').valid_value('character')
    'character'
    >>> Var.get_prop('type').valid_value('char')

    >>> Var.get_prop('long_name').valid_value('hi mom')
    'hi mom'
    >>> Var.get_prop('dimensions').valid_value('hi mom')

    >>> Var.get_prop('dimensions').valid_value(['Bob', 'Ray'])
    ['Bob', 'Ray']
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())).get_prop_value('long_name')
    'Hi mom'
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())).get_prop_value('intent')
    'in'
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'ttype' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))
    Traceback (most recent call last):
    ParseSyntaxError: Invalid metadata variable property, 'ttype', at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))
    Traceback (most recent call last):
    ParseSyntaxError: Required property, 'units', missing, at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'inout', 'constant' : '.true.'}, ParseSource('vname', 'SCHEME', ParseContext())) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: foo is marked constant but is intent inout, at <standard input>:1
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'ino'}, ParseSource('vname', 'SCHEME', ParseContext())) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid intent variable property, 'ino', at <standard input>:1
    """

    # __spec_props are for variables defined in a specification
    __spec_props = [VariableProperty('local_name', str,
                                     check_fn_in=check_fortran_ref),
                    VariableProperty('standard_name', str,
                                     check_fn_in=check_cf_standard_name),
                    VariableProperty('long_name', str, optional_in=True,
                                     default_fn_in=standard_name_to_long_name),
                    VariableProperty('units', str),
                    VariableProperty('dimensions', list,
                                     check_fn_in=check_dimensions),
                    VariableProperty('type', str,
                                     check_fn_in=check_fortran_type),
                    VariableProperty('kind', str,
                                     optional_in=True, default_fn_in=default_kind_val),
                    VariableProperty('state_variable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('optional', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('module', str,
                                     optional_in=True, default_in=''),
                    VariableProperty('constant', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('allocatable', bool,
                                     optional_in=True, default_in=False)]

    # __var_props contains properties which are not in __spec_props
    __var_props = [VariableProperty('intent', str,
                                    valid_values_in=['in', 'out', 'inout'])]


    __spec_propdict = {}
    __var_propdict = {}
    __required_spec_props = list()
    __required_var_props = list()
    for p in __spec_props:
        __spec_propdict[p.name] = p
        __var_propdict[p.name] = p
        if not p.optional:
            __required_spec_props.append(p.name)
            __required_var_props.append(p.name)
        # End if
    # End for
    for p in __var_props:
        __var_propdict[p.name] = p
        if not p.optional:
            __required_var_props.append(p.name)
        # End if
    # End for

    def __init__(self, prop_dict, source):
        if source.type is 'SCHEME':
            required_props = Var.__required_var_props
            master_propdict = Var.__var_propdict
        else:
            required_props = Var.__required_spec_props
            master_propdict = Var.__spec_propdict
        # End if
        self._source = source
        # Grab a frozen copy of the context
        self._context = ParseContext(context=source.context)
        # First, check the input
        if 'ddt_type' in prop_dict:
            # Special case to bypass normal type rules
            if 'type' not in prop_dict:
                prop_dict['type'] = prop_dict['ddt_type']
            # End if
            if 'units' not in prop_dict:
                prop_dict['units'] = ""
            # End if
            prop_dict['kind'] = prop_dict['ddt_type']
            del prop_dict['ddt_type']
        # End if
        for key in prop_dict:
            if Var.get_prop(key) is None:
                raise ParseSyntaxError("Invalid metadata variable property, '{}'".format(key), context=self.context)
            # End if
        # End for
        # Make sure required properties are present
        for propname in required_props:
            if propname not in prop_dict:
                raise ParseSyntaxError("Required property, '{}', missing".format(propname), context=self.context)
            # End if
        # End for
        # Check for any mismatch
        if ('constant' in prop_dict) and ('intent' in prop_dict):
            if prop_dict['intent'].lower() != 'in':
                raise ParseSyntaxError("{} is marked constant but is intent {}".format(prop_dict['local_name'], prop_dict['intent']), context=self.context)
            # End if
        # End if
        # Steal dict from caller
        self._prop_dict = prop_dict
        # Fill in default values for missing properties
        for propname in master_propdict:
            if (propname not in prop_dict) and master_propdict[propname].optional:
                self._prop_dict[propname] = master_propdict[propname].get_default_val(self._prop_dict, context=self.context)
            # End if
        # End for
        # Make sure all the variable values are valid
        try:
            for prop in self._prop_dict.keys():
                check = Var.get_prop(prop).valid_value(self._prop_dict[prop],
                                                       error=True)
            # End for
        except CCPPError as cp:
            raise ParseSyntaxError("{}: {}".format(self._prop_dict['local_name'], cp),
                                   context=self.context)
        # End try

    def compatible(self, other, logger=None):
        # We accept character(len=*) as compatible with character(len=INTEGER_VALUE)
        stype =     self.get_prop_value('type')
        skind =     self.get_prop_value('kind')
        sunits =    self.get_prop_value('units')
        srank=      self.get_prop_value('tank')
        sstd_name = self.get_prop_value('standard_name')
        otype =     other.get_prop_value('type')
        okind =     other.get_prop_value('kind')
        ounits =    other.get_prop_value('units')
        orank=      other.get_prop_value('tank')
        ostd_name = other.get_prop_value('standard_name')
        if stype == 'character':
            kind_eq = ((skind == okind) or
                       (skind == 'len=*' and okind.startswith('len=')) or
                       (skind.startswith('len=') and okind == 'len=*'))
        else:
            kind_eq = skind == okind
        # End if
        if ((sstd_name == ostd_name) and kind_eq and
            (sunits == ounits) and (stype == otype) and (srank == orank)):
            return True
        elif logger is not None:
            if sstd_name != ostd_name:
                logger.info("standard_name: '{}' != '{}'".format(sstd_name, ostd_name))
            elif not kind_eq:
                logger.info("kind: '{}' != '{}'".format(skind, okind))
            elif sunits != ounits:
                logger.info("units: '{}' != '{}'".format(sunits, ounits))
            elif stype != otype:
                logger.info("type: '{}' != '{}'".format(stype, otype))
            elif srank != orank:
                logger.info("rank: '{}' != '{}'".format(srank, orank))
            else:
                logger.error('Why are these variables not compatible?')
            # End if
            return False
        else:
            return False
        # End if

    @classmethod
    def get_prop(cls, name, spec_type=None):
        if (spec_type is None) and (name in Var.__var_propdict):
            return Var.__var_propdict[name]
        elif (spec_type is not None) and (name in Var.__spec_propdict):
            return Var.__spec_propdict[name]
        else:
            return None

    def get_prop_value(self, name):
        if name in self._prop_dict:
            return self._prop_dict[name]
        else:
            return None

    @property
    def context(self):
        return self._context

    @property
    def source(self):
        return self._source

    def get_dimensions(self):
        "Return the variable's dimension string"
        dimval = self.get_prop_value('dimensions')
        dims = Var.get_prop('dimensions').valid_value(dimval)
        return dims

    def write_def(self, outfile, indent, allocatable=False):
        '''Write the definition line for the variable.'''
        vtype = self.get_prop_value('type')
        kind = self.get_prop_value('kind')
        name = self.get_prop_value('local_name')
        dims = self.get_dimensions()
        if (dims is not None) and (len(dims) > 0):
            if allocatable:
                dimstr = '(:' + ',:'*(len(dims) - 1) + ')'
            else:
                dimstr = '({})'.format(', '.join(dims))
            # End if
        else:
            dimstr = ''
        # End if
        constant = self.get_prop_value('constant')
        intent = self.get_prop_value('intent')
        if constant and allocatable:
            raise CCPPError('Cannot create allocatable variable from constant, {}'.format(name))
        # End if
        if constant:
            intent_str = 'intent(in)   '
        elif allocatable:
            if len(dimstr) > 0:
                intent_str = 'allocatable  '
            else:
                intent_str = ' '*13
            # End if
        elif intent is not None:
            intent_str = 'intent({}){}'.format(intent, ' '*(5 - len(intent)))
        else:
            intent_str = ' '*13
        # End if
        if registered_fortran_ddt_name(vtype):
            str = "type({kind}){cspc}{intent} :: {name}{dims}"
            cspc = ',' + ' '*(13 - len(kind))
        else:
            if (kind is not None) and (len(kind) > 0):
                str = "{type}({kind}){cspc}{intent} :: {name}{dims}"
                cspc = ',' + ' '*(17 - len(vtype) - len(kind))
            else:
                str = "{type}{cspc}{intent} :: {name}{dims}"
                cspc = ',' + ' '*(19 - len(vtype))
            # End if
        # End if
        outfile.write(str.format(type=vtype, kind=kind, intent=intent_str,
                                 name=name, dims=dimstr, cspc=cspc), indent)

    def host_arg_str(self, hvar, host_model, header, ddt):
        '''Create the proper statement of a piece of a host-model variable.
        If ddt is True, we can only have a single element selected
        '''
        hstr = hvar.get_prop_value('local_name')
        # Turn the dimensions string into a proper list and take the correct one
        hdims = hvar.get_dimensions()
        dimsep = ''
        # Does the local name have any extra indices?
        match = array_ref_re.match(hstr.strip())
        if match is not None:
            hstr = match.group(1)
            # Find real names for all the indices
            tokens = [x.strip() for x in match.group(2).strip().split(',')]
            for token in tokens:
                hsdim = self.find_host_model_var(token, host_model)
                dimstr = dimstr + dimsep + hsdim
            # End for
        # End if
        if len(hdims) > 0:
            dimstr = '('
        else:
            dimstr = ''
        # End if
        for hdim in hdims:
            if ddt and (':' in hdim):
                raise CCPPError("Invalid DDT dimension spec {}({})".format(hstr, hdimval))
            else:
                # Find the host model variable for each dim
                hsdims = self.find_host_model_var(hdim, host_model)
                dimstr = dimstr + dimsep + hsdims
                dimsep = ', '
            # End if
        # End for
        if len(hdims) > 0:
            dimstr = dimstr + ')'
        # End if
        return hstr + dimstr

    def print_debug(self):
        '''Print the data retrieval line for the variable.'''
        str='''Contents of {local_name} (* = mandatory for compatibility):
        standard_name = {standard_name} *
        long_name     = {long_name}
        units         = {units} *
        local_name    = {local_name}
        type          = {type} *
        dimensions    = {dimensions} *
        kind          = {kind} *
        intent        = {intent}
        optional      = {optional}
        '''
        if self._context is not None:
            str = str + '\n        context       = {}'.format(self._context)
        # End if
        return str.format(**self._prop_dict)

###############################################################################

class VarDictionary(OrderedDict):
    """
    A class to store and cross-check variables from one or more metadata
    headers. The class also serves as a scoping construct so that a variable
    can be found in an innermost available scope.
    The dictionary is organized by standard_name. It is an error to try
    to add a variable if its standard name is already in the dictionary.
    Scoping is a tree of VarDictionary objects.
    >>> VarDictionary('foo')
    VarDictionary(foo)
    >>> VarDictionary('bar', variables={})
    VarDictionary(bar)
    >>> VarDictionary('baz', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))) #doctest: +ELLIPSIS
    VarDictionary(baz, [('hi_mom', <__main__.Var object at 0x...>)])
    >>> print("{}".format(VarDictionary('baz', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())))))
    VarDictionary(baz, ['hi_mom'])
    >>> VarDictionary('qux', [Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))]) #doctest: +ELLIPSIS
    VarDictionary(qux, [('hi_mom', <__main__.Var object at 0x...>)])
    >>> VarDictionary('boo').add_variable(Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext())))

    >>> VarDictionary('who', variables=[Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))]).prop_list('local_name')
    ['foo']
    >>> VarDictionary('glitch', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))).add_variable(Var({'local_name' : 'bar', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname2', 'DDT', ParseContext()))) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid Duplicate standard name, 'hi_mom', at <standard input>:
    """

    # Regular expression matching <name>_extent, <name>_begin, or <name>_end
    __ebe_re  = re.compile(FORTRAN_ID+r"_((?i)(?:extent)|(?:begin)|(?:end))$")
    # Regular expression matching <name>_loop_extent
    __extent_re = re.compile(FORTRAN_ID+r"((?i)_loop_extent)$")

    # Variable representing the constant integer, 1
    __var_one = Var({'local_name' : 'ccpp_one', 'constant' : 'True',
                     'standard_name' : 'ccpp_constant_one',
                     'units' : '1', 'dimensions' : '()', 'type' : 'integer'},
                    ParseSource('VarDictionary', 'REGISTRY', ParseContext()))

    def __init__(self, name, variables=None, parent_dict=None, logger=None):
        "Unlike dict, VarDictionary only takes a Var or Var list"
        super(VarDictionary, self).__init__()
        self._name = name
        self._logger = logger
        self._parent_dict = parent_dict
        if parent_dict is not None:
            parent_dict.add_sub_scope(self)
        # End if
        self._sub_dicts = list()
        if isinstance(variables, Var):
            self.add_variable(variables)
        elif isinstance(variables, list):
            for var in variables:
                self.add_variable(var)
            # End for
        elif isinstance(variables, VarDictionary):
            for stdname in variables.keys():
                self[stdname] = variables[stdname]
            # End for
        elif isinstance(variables, dict):
            # variables will not be in 'order', but we accept them anyway
            for stdname in variables.keys():
                self[stdname] = variables[stdname]
            # End for
        elif variables is not None:
            raise ParseInternalError('Illegal type for variables, {} in {}'.format(type(variables), self.name))
        # End if

    @property
    def name(self):
        return self._name

    @property
    def parent(self):
        return self._parent_dict

    def variable_list(self, recursive=False):
        "Return a list of all variables"
        if recursive and (self._parent_dict is not None):
            vlist = self._parent_dict.variable_list(recursive=recursive)
        else:
            vlist = list()
        # End if
        for sn in self.keys():
            vlist.append(self[sn])
        # End for
        return vlist

    def add_variable(self, newvar, exists_ok=False):
        """Add a variable if it does not conflict with existing entries"""
        standard_name = newvar.get_prop_value('standard_name')
        if (standard_name in self) and (not exists_ok):
            # We already have a matching variable, error!
            if self._logger is not None:
                self._logger.error("Attempt to add duplicate variable, {} from {}".format(standard_name, newvar.source.name))
            # End if
            raise ParseSyntaxError("Duplicate standard name in {}".format(self.name),
                                   token=standard_name, context=newvar._context)
        # End if
        cvar = self.find_variable(standard_name)
        if (cvar is not None) and (not cvar.compatible(newvar, self._logger)):
            if self._logger is not None:
                self._logger.error("Attempt to add incompatible variable, {} from {}".format(standard_name, newvar.source.name))
            # End if
            errstr = "Standard name incompatible with {}"
            raise ParseSyntaxError(errstr.format(cvar.context),
                                   token=standard_name,
                                   context=newvar.source.context)
        # End if
        # If we make it to here without an exception, add the variable
        self[standard_name] = newvar

    def find_variable(self, standard_name, any_scope=True, loop_subst=False):
        """Return the variable matching <standard_name> or None
        If any_scope is True, search parent scopes if not in current scope.
        VarDictionary is a base class, loop_subst does nothing at this level.
        """
        if standard_name in self:
            var = self[standard_name]
        elif any_scope and (self._parent_dict is not None):
            var = self._parent_dict.find_variable(standard_name, any_scope)
        else:
            var = None
        # End if
        if (var is None) and loop_subst:
            var = self.find_loop_subst(standard_name, any_scope=any_scope)
        # End if
        return var

    def add_sub_scope(self, sub_dict):
        'Add a child dictionary to enable traversal'
        self._sub_dicts.append(sub_dict)

    def prop_list(self, prop_name):
        'Return a list of the <prop_name> property for each variable.'
        plist = list()
        for standard_name in self.keys():
            plist.append(self[standard_name].get_prop_value(prop_name))
        # End for
        return plist

    def declare_variables(self, outfile, indent):
        "Write out the declarations for this dictionary's variables"
        for standard_name in self.keys():
            self[standard_name].write_def(outfile, indent)
        # End for

    def merge(self, other_dict):
        "Add new entries from <other_dict>"
        for ovar in other_dict.variable_list():
            self.add_variable(ovar)
        # End for

    def __str__(self):
        return "VarDictionary({}, {})".format(self.name, self.keys())

    def __repr__(self):
        srepr = super(VarDictionary, self).__repr__()
        vstart = len("VarDictionary") + 1
        if len(srepr) > vstart + 1:
            comma = ", "
        else:
            comma = ""
        # End if
        return "VarDictionary({}{}{}".format(self.name, comma, srepr[vstart:])

    def find_loop_subst(self, standard_name, any_scope=True, context=None):
        """If <standard_name> is of the form <standard_name>_extent and that
        variable is not in the dictionary, substitute a tuple of variables,
        (<standard_name>_begin, <standard_name>_end), if those variables are
        in the dictionary.
        If <standard_name>_extent *is* present, return that variable as a
        range, (__var_one, <standard_name>_extent)
        In other cases, return None
        """
        loop_var = VarDictionary.__ebe_re.match(standard_name)
        dict_var = self.find_variable(standard_name,
                                      any_scope=any_scope, loop_subst=False)
        logger_str = None
        if loop_var is not None:
            # Let us see if we can fix a loop variable
            # First up, we have an extent but host has begin and end
            if loop_var.group(2).lower() == 'extent':
                if dict_var is not None:
                    my_var = (VarDictionary.__var_one, dict_var)
                    if self._logger is not None:
                        logger_str = "loop_subst: found {}{}".format(standard_name, context_string(context))
                    # End if
                else:
                    beg_name = loop_var.group(1) + '_begin'
                    end_name = loop_var.group(1) + '_end'
                    beg_var = self.find_variable(beg_name)
                    end_var = self.find_variable(end_name)
                    if (beg_var is not None) and (end_var is not None):
                        my_var = (beg_var, end_var)
                        if self._logger is not None:
                            logger_str = "loop_subst: {} ==> ({}, {}){}".format(standard_name, beg_name, end_name, context_string(context))
                        # End if
                    else:
                        if self._logger is not None:
                            logger_str = "loop_subst: {} ==> ({}, {}) FAILED{}".format(standard_name, beg_name, end_name, context_string(context))
                        # End if
                        my_var = None
                    # End if
                # End if
            else:
                if self._logger is not None:
                    logger_str = "loop_subst: no known substitution for {}{}, not a loop variable?".format(standard_name, context_string(context))
                # End if
                my_var = None # Nothing to do here?
            # End if
        else:
            if self._logger is not None:
                logger_str = "loop_subst: {} is not a loop variable{}".format(standard_name, context_string(context))
            # End if
            my_var = None
        # End if
        if logger_str is not None:
            self._logger.debug(logger_str)
        # End if
        return my_var

    def find_dimension_subst(self, standard_name, any_scope=True, context=None):
        """If <standard_name> is of the form <standard_name>_loop_extent
        attempt to find a variable of the form <standard_name>_dimension
        and return that. If such a variable is not found, raise an exception.
        If <standard_name> is not of the form <standard_name>_extent, return
        None.
        """
        loop_var = VarDictionary.__extent_re.match(standard_name)
        logger_str = None
        if loop_var is not None:
            # Let us see if we can replace the variable
            dim_name = loop_var.group(1) + '_dimension'
            my_var = self.find_variable(dim_name, any_scope=any_scope)
            if my_var is None:
                raise CCPPError("Dimension variable, {} not found{}".format(dim_name, context_string(context)))
            # End if
        else:
            my_var = None
        # End if
        return my_var

###############################################################################
if __name__ == "__main__":
    import doctest
    doctest.testmod()
