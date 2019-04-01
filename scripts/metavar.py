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
        if len(standard_name) > 0:
            long_name = standard_name[0].upper() + re.sub("_", " ", standard_name[1:])
        else:
            long_name = ''
        # End if
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
def ddt_modules(variable_list):
########################################################################
    ddt_mods = set()
    for var in variable_list:
        if var.is_ddt():
            module = var.get_prop_value('module')
            if len(module) > 0:
                ddt_mods.add((module, var.get_prop_value('type')))
            # End if
        # End if
    # End for
    return ddt_mods

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
    >>> VariableProperty('dimensions', list, check_fn_in=check_dimensions).valid_value('(w:x,x:y:z:q)', error=True) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: 'x:y:z:q' is an invalid dimension range
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
    """ A class to hold a metadata or code variable.
    Var objects should be treated as immutable.
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
    ParseSyntaxError: Invalid metadata variable property, 'ttype', in <standard input>
    >>> Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'SCHEME', ParseContext()))
    Traceback (most recent call last):
    ParseSyntaxError: Required property, 'units', missing, in <standard input>
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
                                     optional_in=True,
                                     default_fn_in=default_kind_val),
                    VariableProperty('state_variable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('optional', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('constant', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('allocatable', bool,
                                     optional_in=True, default_in=False),
                    VariableProperty('vertical_coord', str, optional_in=True,
                                     valid_values_in=['vertical_index',
                                                      'vertical_level_dimension',
                                                      'vertical_layer_dimension'],
                                     default_in='vertical_index'),
                    VariableProperty('persistence', str, optional_in=True,
                                     valid_values_in=['timestep', 'run'],
                                     default_in='timestep')]

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

    def __init__(self, prop_dict, source, invalid_ok=False, logger=None):
        """NB: invalid_ok=True is dangerous because it allows creation
        of a Var object with invalid properties.
        In order to prevent silent failures, invalid_ok requires a logger
        in order to take effect."""
        if source.type is 'scheme':
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
                if invalid_ok and (logger is not None):
                    ctx = context_string(self.context)
                    logger.warning("Required property, '{}', missing{}".format(propname, ctx))
                else:
                    raise ParseSyntaxError("Required property, '{}', missing".format(propname), context=self.context)
                # End if
            # End if
        # End for
        # Check for any mismatch
        if ('constant' in prop_dict) and ('intent' in prop_dict):
            if prop_dict['intent'].lower() != 'in':
                if invalid_ok and (logger is not None):
                    ctx = context_string(self.context)
                    logger.warning("{} is marked constant but is intent {}{}".format(prop_dict['local_name'], prop_dict['intent'], ctx))
                else:
                    raise ParseSyntaxError("{} is marked constant but is intent {}".format(prop_dict['local_name'], prop_dict['intent']), context=self.context)
                # End if
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
            if invalid_ok and (logger is not None):
                ctx = context_string(self.context)
                logger.warning("{}: {}{}".format(self._prop_dict['local_name'], cp, ctx))
            else:
                raise ParseSyntaxError("{}: {}".format(self._prop_dict['local_name'], cp),
                                       context=self.context)
            # End if
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
            return True, None
        else:
            logger_str = None
            error_str = None
            if sstd_name != ostd_name:
                logger_str = "standard_name: '{}' != '{}'".format(sstd_name, ostd_name)
                reason = 'standard_name'
            elif not kind_eq:
                logger_str = "kind: '{}' != '{}'".format(skind, okind)
                reason = 'kind'
            elif sunits != ounits:
                logger_str = "units: '{}' != '{}'".format(sunits, ounits)
                reason = 'units'
            elif stype != otype:
                logger_str = "type: '{}' != '{}'".format(stype, otype)
                reason = 'type'
            elif srank != orank:
                logger_str = "rank: '{}' != '{}'".format(srank, orank)
                reason = 'rank'
            else:
                error_str = 'Why are these variables not compatible?'
                reason = 'UNKNOWN'
            # End if
            if logger is not None:
                if error_str is not None:
                    logger.error('{}'.format(error_str))
                elif logger_str is not None:
                    logger.info('{}'.format(logger_str))
                # End if (no else)
            # End if
            return False, reason
        # End if

    @classmethod
    def get_prop(cls, name, spec_type=None):
        if (spec_type is None) and (name in Var.__var_propdict):
            return Var.__var_propdict[name]
        elif (spec_type is not None) and (name in Var.__spec_propdict):
            return Var.__spec_propdict[name]
        else:
            return None

    def clone(self, name, source_name=None, source_type=None, context=None, internal=True):
        """Create a clone of this Var object with local_name, <name>.
        The optional <source_name>, <source_type>, and <context> inputs
        allow the clone to appear to be coming from a designated source,
        by default, the source and type are the same as this Var (self).
        <internal> is an indication that the clone is generated and owned by
        the CCPP framework, not by the host model or any scheme.
        """
        cprop_dict = {}
        for prop in self._prop_dict.keys():
            if prop == 'local_name':
                cprop_dict[prop] = name
            else:
                cprop_dict[prop] = self._prop_dict[prop]
            # End if
        # End for
        if source_name is None:
            source_name = self.source.name
        # End if
        if source_type is None:
            source_type = self.source.type
        # End if
        if context is None:
            context = self._context
        # End if
        psource = ParseSource(source_name, source_type, context)
        return Var(cprop_dict, psource, internal=internal)

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

    @property
    def host_interface_var(self):
        'True iff self is included in the host model interface calls'
        return self.source.type == 'host'

    @classmethod
    def loop_subst_dims(cls, dims):
        newdims = list()
        for dim in dims:
            # loop_subst_match swallows an entire dim string, even ranges
            ldim = VarDictionary.loop_subst_match(dim)
            if ldim is None:
                newdims.append(dim)
            else:
                newdims.append(ldim)
            # End if
        # End for
        return newdims

    def get_dimensions(self, loop_subst=False):
        "Return the variable's dimension string"
        dimval = self.get_prop_value('dimensions')
        dims = Var.get_prop('dimensions').valid_value(dimval)
        if loop_subst:
            newdims = loop_subst_dims(dims)
        else:
            newdims = dims
        # End if
        return newdims

    def get_dim_stdnames(self):
        "Return a set of all the dimension standard names for this Var"
        dimset = set()
        for dim in self.get_dimensions():
            for name in dim.split(':'):
                # Weed out the integers
                try:
                    ival = int(name)
                except ValueError as ve:
                    # Not an integer, add it
                    dimset.add(name)
                # End try
            # End for
        # End for
        return dimset

    def get_rank(self):
        "Return the variable's rank (zero for scalar)"
        dims = self.get_dimensions(loop_subst=False)
        return len(dims)

    def write_def(self, outfile, indent, dict, allocatable=False, loop_subst=False):
        '''Write the definition line for the variable.'''
        vtype = self.get_prop_value('type')
        kind = self.get_prop_value('kind')
        name = self.get_prop_value('local_name')
        dims = self.get_dimensions(loop_subst=loop_subst)
        if (dims is not None) and (len(dims) > 0):
            if allocatable:
                dimstr = '(:' + ',:'*(len(dims) - 1) + ')'
            else:
                dimstr = '('
                comma = ''
                for dim in dims:
                    # Only ranges or sizes go into declaration
                    if VarDictionary.loop_var_match(dim):
                        continue
                    else:
                        dstdnames = dim.split(':')
                        dvars = [dict.find_variable(x) for x in dstdnames]
                        if None in dvars:
                            for dim in dstdnames:
                                if dict.find_variable(dim) is None:
                                    raise CCPPError("No variable found for dimension '{}' in {}".format(dim, name))
                                # End if
                            # End for
                        # End if
                        dnames = [x.get_prop_value('local_name') for x in dvars]
                        dimstr = dimstr + comma + ':'.join(dnames)
                        comma = ', '
                    # End if
                # End for
                dimstr = dimstr + ')'
                if dimstr == '()':
                    dimstr = '' # It ends up being a scalar reference
                # End if
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
        if self.is_ddt():
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

    def is_ddt(self):
        '''Return True iff <self> is a DDT type.'''
        vtype = self.get_prop_value('type')
        return registered_fortran_ddt_name(vtype) is not None

    def host_arg_str(self, hvar, host_model, ddt):
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

    def __str__(self):
        '''Print representation or string for Var objects'''
        return "<Var {standard_name}: {local_name}>".format(**self._prop_dict)

    def __repr__(self):
        '''Object representation for Var objects'''
        base = super(Var, self).__repr__()
        pind = base.find(' object ')
        if pind >= 0:
            pre = base[0:pind]
        else:
            pre = '<Var'
        # End if
        bind = base.find('at 0x')
        if bind >= 0:
            post = base[bind:]
        else:
            post = '>'
        # End if
        return '{} {}: {} {}'.format(pre, self._prop_dict['standard_name'], self._prop_dict['local_name'], post)

###############################################################################

class VarSpec(object):
    """A class to hold a standard_name description of a variable.
    A scalar variable is just a standard name while an array also
    contains a comma-separated list of dimension standard names in parentheses.
    """

    def __init__(self, var, loop_subst=False):
        self._name = var.get_prop_value('standard_name')
        self._dims = var.get_dimensions(loop_subst=loop_subst)
        if len(self._dims) == 0:
            self._dims = None
        # End if

    @property
    def name(self):
        return self._name

    def get_dimensions(self, loop_subst=False):
        if loop_subst:
            rdims = Var.loop_subst_dims(dims)
        else:
            rdims = dims
        # End if
        return rdims

    def __repr__(self):
        if self._dims is not None:
            return "{}({})".format(self._name, ', '.join(self._dims))
        else:
            return self._name
        # End if

###############################################################################

class VarSpec(object):
    """A class to hold a standard_name description of a variable.
    A scalar variable is just a standard name while an array also
    contains a comma-separated list of dimension standard names in parentheses.
    """

    def __init__(self, var, loop_subst=False):
        self._name = var.get_prop_value('standard_name')
        self._dims = var.get_dimensions(loop_subst=loop_subst)
        if len(self._dims) == 0:
            self._dims = None
        # End if

    @property
    def name(self):
        return self._name

    def get_dimensions(self, loop_subst=False):
        if loop_subst:
            rdims = Var.loop_subst_dims(self._dims)
        else:
            rdims = self._dims
        # End if
        return rdims

    def __repr__(self):
        if self._dims is not None:
            return "{}({})".format(self._name, ', '.join(self._dims))
        else:
            return self._name
        # End if

###############################################################################

class VarDDT(Var):
    """A class to store a variable that is a component of a DDT (at any
    DDT nesting level).
    """

    def __init__(self, standard_name, var_ref_list, logger=None):
        self._standard_name = standard_name
        self._var_ref_list = list()
        for var in var_ref_list:
            self._var_ref_list.append(var)
        # End for
        self._vlen = len(self._var_ref_list)
        if logger is not None:
            lnames = [x.get_prop_value('local_name') for x in self._var_ref_list]
            logger.debug('Adding DDT field, {}, {}'.format(standard_name, lnames))
        # End if

    def compatible(self, other, logger=None):
        "Compare <other> to the intrinsic variable the end of the DDT chain"
        self._var_ref_list[-1].compare(other)

    def get_prop_value(self, name, index=0):
        "Return the indicated property value, defauling to the top-level DDT"
        if abs(index) >= self._vlen:
            raise ParseInternalError("VarDDT.get_prop_value index ({}) out of range".format(index))
        # End if
        return self._var_ref_list[index].get_prop_value(name)

    @property
    def context(self):
        "Return the context of the variable source (DDT root)"
        return self._var_ref_list[0].context

    @property
    def source(self):
        "Return the source of the variable source (DDT root)"
        return self._var_ref_list[0].source

    def get_dimensions(self, loop_subst=False, index=0):
        "Return the dimensions of the indicated var, defauling to the top-level DDT"
        if abs(index) >= self._vlen:
            raise ParseInternalError("VarDDT.get_prop_value index ({}) out of range".format(index))
        # End if
        return self._var_ref_list[index].get_dimensions(loop_subst)

    def write_def(self, outfile, indent, dict, allocatable=False, loop_subst=False):
        '''Write the definition line for the variable.'''
        pass

    def is_ddt(self):
        '''Return True iff <self> is a DDT type.'''
        return True

    def host_arg_str(self, hvar, host_model, ddt):
        '''Create the proper statement of a piece of a host-model variable.
        If ddt is True, we can only have a single element selected
        '''
        pass

    def __repr__(self):
        '''Print representation or string for VarDDT objects'''
        return "<{}>".format('%'.join([x.__repr__() for x in self._var_ref_list]))

###############################################################################

__ccpp_registry_parse_source__ = ParseSource('VarDictionary', 'REGISTRY',
                                             ParseContext())

# Dictionary of standard CCPP variables
CCPP_STANDARD_VARS = {
    # Variable representing the constant integer, 1
    'ccpp_constant_one' :
    Var({'local_name' : 'ccpp_one', 'constant' : 'True',
         'standard_name' : 'ccpp_constant_one',
         'units' : '1', 'dimensions' : '()', 'type' : 'integer'},
        __ccpp_registry_parse_source__),
    'ccpp_error_flag' :
    Var({'local_name' : 'errflg', 'standard_name' : 'ccpp_error_flag',
         'units' : '1', 'dimensions' : '()', 'type' : 'integer'},
        __ccpp_registry_parse_source__),
    'ccpp_error_message' :
    Var({'local_name' : 'errmsg', 'standard_name' : 'ccpp_error_message',
         'units' : '1', 'dimensions' : '()', 'type' : 'integer'},
        __ccpp_registry_parse_source__),
    'horizontal_loop_extent' :
    Var({'local_name' : 'horz_loop_ext',
         'standard_name' : 'horizontal_loop_extent', 'units' : '1',
         'dimensions' : '()', 'type' : 'integer'},
        __ccpp_registry_parse_source__),
    'horizontal_loop_begin' :
    Var({'local_name' : 'horz_col_beg',
         'standard_name' : 'horizontal_loop_begin', 'units' : '1',
         'dimensions' : '()', 'type' : 'integer'},
        __ccpp_registry_parse_source__),
    'horizontal_loop_end' :
    Var({'local_name' : 'horz_col_end',
         'standard_name' : 'horizontal_loop_end', 'units' : '1',
         'dimensions' : '()', 'type' : 'integer'},
        __ccpp_registry_parse_source__)
}

# List of constant variables which are universally available
CCPP_CONSTANT_VARS = {'ccpp_constant_one' :
                      CCPP_STANDARD_VARS['ccpp_constant_one']}

###############################################################################

class VarLoopSubst(object):
    """A class to handle required loop substitutions where the host model
    (or a suite part) does not provide a loop-like variable used by a
    suite part or scheme"""

    def __init__(self, missing_stdname, required_stdnames, set_action):
        self._missing_stdname = missing_stdname
        if isinstance(required_stdnames, Var):
            self._required_stdnames = (required_stdnames,)
        else:
            # Make sure required_stdnames is iterable
            try:
                _ = (v for v in required_stdnames)
                self._required_stdnames = required_stdnames
            except TypeError as te:
                raise ParseInternalError("required_stdnames must be a tuple or a list")
            # End try
        # End if
        self._set_action = set_action

    def find_subst(self, dict, any_scope=False):
        """Attempt to find or create a Var for <_missing_stdname> and add it
        to <dict>.
        If any_scope is True, consult parent dictionaries.
        Also, verify that the required variable(s) is(are) in <dict>
        If successful, return Var, otherwise, throw a CCPPError.
        """
        # A template for 'missing' should be in the standard variable list
        if self._missing_stdname in CCPP_STANDARD_VARS:
            var = CCPP_STANDARD_VARS[self._missing_stdname]
            dict.add_variable(var, gen_unique=True)
        else:
            var = None
        # End if
        if var is None:
            raise CCPPError('Unable to find or create missing loop variable, {}'.format(self._missing_stdname))
        # End if
        for stdname in self._required_stdnames:
            if dict.find_variable(stdname, any_scope=any_scope) is None:
                raise CCPPError("{} is required to set value for {} but is not in dictionary".format(stdname, self._missing_stdname))
            # End if
        # End for
        return var

    def write_action(self, dict, any_scope=False):
        """Return a string setting the correct values for our
        replacement variable"""
        action_dict = {}
        for stdname in self._required_stdnames:
            var = dict.find_variable(stdname, any_scope=any_scope)
            if var is None:
                raise CCPPError("Required variable, {}, not found".format(stdname))
            # End if
            action_dict[stdname] = var.get_prop_value('local_name')
        # End for
        var = dict.find_variable(self._missing_stdname)
        if var is None:
            raise CCPPError("Required variable, {}, not found".format(self._missing_stdname))
        # End if
        action_dict[self._missing_stdname] = var.get_prop_value('local_name')
        return self._set_action.format(**action_dict)

CCPP_VAR_LOOP_SUBSTS = { 'horizontal_loop_extent' :
                         VarLoopSubst('horizontal_loop_extent',
                                      ('horizontal_loop_begin',
                                       'horizontal_loop_end'),
                                      '{horizontal_loop_extent} = {horizontal_loop_end} - {horizontal_loop_begin} + 1'),
                         'horizontal_loop_begin' :
                         VarLoopSubst('horizontal_loop_begin',
                                      ('ccpp_constant_one',),
                                      '{horizontal_loop_begin} = 1'),
                         'horizontal_loop_end' :
                         VarLoopSubst('horizontal_loop_end',
                                      ('horizontal_loop_extent',),
                                      '{horizontal_loop_end} = {horizontal_loop_extent}')
}

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
    >>> VarDictionary('baz', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))) #doctest: +ELLIPSIS
    VarDictionary(baz, [('hi_mom', <__main__.Var hi_mom: foo at 0x...>)])
    >>> print("{}".format(VarDictionary('baz', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext())))))
    VarDictionary(baz, ['hi_mom'])
    >>> VarDictionary('qux', [Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))]) #doctest: +ELLIPSIS
    VarDictionary(qux, [('hi_mom', <__main__.Var hi_mom: foo at 0x...>)])
    >>> VarDictionary('boo').add_variable(Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext())))

    >>> VarDictionary('who', variables=[Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))]).prop_list('local_name')
    ['foo']
    >>> VarDictionary('who', variables=[Var({'local_name' : 'who_var1', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))]).new_internal_variable_name()
    'who_var2'
    >>> VarDictionary('who', variables=[Var({'local_name' : 'who_var1', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))]).new_internal_variable_name('bar')
    'bar_who_var1'
    >>> VarDictionary('glitch', Var({'local_name' : 'foo', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname', 'scheme', ParseContext()))).add_variable(Var({'local_name' : 'bar', 'standard_name' : 'hi_mom', 'units' : 'm/s', 'dimensions' : '()', 'type' : 'real', 'intent' : 'in'}, ParseSource('vname2', 'DDT', ParseContext()))) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ParseSyntaxError: Invalid Duplicate standard name, 'hi_mom', at <standard input>:
    """

    # Loop variables
    __ccpp_loop_vars__ = ['horizontal_loop_begin', 'horizontal_loop_end',
                          'thread_block_number', 'horizontal_loop_extent']

    # Dimension substitutions
    __ccpp_dim_subst__ = {'horizontal_loop_extent' : 'horizontal_dimension'}

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
            # variables may not be in 'order', but we accept them anyway
            for key in variables.keys():
                var = variables[key]
                stdname = var.get_prop_value('standard_name')
                self[stdname] = variables[key]
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

    def include_var_in_list(self, var, std_vars, loop_vars, consts):
        '''Return True iff <var> is of a type allowed by the logicals,
        <std_vars> (not constants or loop_vars),
        <loop_vars> a variable ending in "_extent", "_begin", "_end", or
        <consts> a variable with the "constant" property.
        '''
        const_val = var.get_prop_value('constant')
        const_var = Var.get_prop('constant').valid_value(const_val)
        include_var = consts and const_var
        if not include_var:
            standard_name = var.get_prop_value('standard_name')
            loop_var = VarDictionary.loop_var_match(standard_name)
            include_var = loop_var and loop_vars
            if not include_var:
                std_var = not (loop_var or const_var)
                include_var = std_vars and std_var
            # End if
        # End if
        return include_var

    def variable_list(self, recursive=False,
                      std_vars=True, loop_vars=True, consts=True):
        "Return a list of all variables"
        if recursive and (self._parent_dict is not None):
            vlist = self._parent_dict.variable_list(recursive=recursive,
                                                    std_vars=std_vars,
                                                    loop_vars=loop_vars,
                                                    consts=consts)
        else:
            vlist = list()
        # End if
        for sn in self.keys():
            var = self[sn]
            if self.include_var_in_list(var, std_vars=std_vars,
                                        loop_vars=loop_vars, consts=consts):
                vlist.append(var)
            # End if
        # End for
        return vlist

    def add_variable(self, newvar, exists_ok=False, gen_unique=False):
        """Add a variable if it does not conflict with existing entries
        If exists_ok is True, attempting to add an identical copy is okay.
        If gen_unique is True, a new local_name will be created if a
        local_name collision is detected."""
        standard_name = newvar.get_prop_value('standard_name')
        if (standard_name in self) and (not exists_ok):
            # We already have a matching variable, error!
            if self._logger is not None:
                self._logger.error("Attempt to add duplicate variable, {} from {}".format(standard_name, newvar.source.name))
            # End if
            raise ParseSyntaxError("(duplicate) standard name in {}".format(self.name),
                                   token=standard_name, context=newvar._context)
        # End if
        cvar = self.find_variable(standard_name, any_scope=False)
        if cvar is not None:
            compat, reason = cvar.compatible(newvar, self._logger)
            if not compat:
                if self._logger is not None:
                    self._logger.error("Attempt to add incompatible variable, {} from {}".format(standard_name, newvar.source.name))
                # End if
                nlname = newvar.get_prop_value('local_name')
                clname = cvar.get_prop_value('local_name')
                cstr = context_string(cvar.context, with_comma=True)
                errstr = "new variable, {}, incompatible {} between {}{} and"
                raise ParseSyntaxError(errstr.format(nlname, reason, clname, cstr),
                                       token=standard_name,
                                       context=newvar.source.context)
            # End if
        # End if
        lname = newvar.get_prop_value('local_name')
        lvar = self.find_local_name(lname)
        if lvar is not None:
            if gen_unique:
                new_lname = self.new_internal_variable_name(lname)
                newvar = newvar.clone(new_lname)
            else:
                errstr = ' {}: already registered{}'
                cstr = context_string(lvar.source.context, with_comma=True)
                raise ParseSyntaxError(errstr.format(lname, cstr),
                                       token='local_name',
                                       context=newvar.source.context)
        # If we make it to here without an exception, add the variable
        self[standard_name] = newvar

    def remove_variable(self, standard_name):
        """Remove <standard_name> from the dictionary.
        Ignore if <standard_name> is not in dict
        """
        if standard_name in self:
            del self[standard_name]
        # End if

    def find_variable(self, standard_name, any_scope=True, clone=None):
        """Attempt to return the variable matching <standard_name>.
        If <any_scope> is True, search parent scopes if not in current scope.
        If the variable is not found and <clone> is not None, add a clone of
        <clone> to this dictionary.
        If the variable is not found and <clone> is None, return None.
        """
        if standard_name in CCPP_CONSTANT_VARS:
            var = CCPP_CONSTANT_VARS[standard_name]
        elif standard_name in self:
            var = self[standard_name]
        elif any_scope and (self._parent_dict is not None):
            var = self._parent_dict.find_variable(standard_name, any_scope)
        else:
            var = None
        # End if
        if (var is None) and (clone is not None):
            lname = clone.get_prop_value['local_name']
            new_name = self.new_internal_variable_name(lname)
            new_var = clone.clone(new_name)
        # End if
        return var

    def add_sub_scope(self, sub_dict):
        'Add a child dictionary to enable traversal'
        self._sub_dicts.append(sub_dict)

    def prop_list(self, prop_name, std_vars=True, loop_vars=True, consts=True):
        '''Return a list of the <prop_name> property for each variable.
        std_vars are variables which are neither constants nor loop variables.
        '''
        plist = list()
        for standard_name in self.keys():
            var = self.find_variable(standard_name, any_scope=False)
            if self.include_var_in_list(var, std_vars=std_vars, loop_vars=loop_vars, consts=consts):
                plist.append(self[standard_name].get_prop_value(prop_name))
            # End if
        # End for
        return plist

    def declare_variables(self, outfile, indent,
                          std_vars=True, loop_vars=True, consts=True):
        "Write out the declarations for this dictionary's variables"
        for standard_name in self.keys():
            var = self.find_variable(standard_name, any_scope=False)
            if self.include_var_in_list(var, std_vars=std_vars, loop_vars=loop_vars, consts=consts):
                self[standard_name].write_def(outfile, indent, self)
            # End if
        # End for

    def set_subst_value(self, local_name, stdname):
        "Return code to set <local_name> value"
        subst_str = None
        if stdname == 'horizontal_loop_extent':
            bvar = self.find_variable('horizontal_loop_begin', any_scope=False)
            evar = self.find_variable('horizontal_loop_end', any_scope=False)
            if bvar and evar:
                bname = bvar.get_prop_value('local_name')
                ename = evar.get_prop_value('local_name')
                subst_str = '{} = {} - {} + 1'.format(local_name, ename, bname)
            else:
                raise ParseInternalError("Unable to create value for {}".format(local_name))
            # End if
        elif stdname == 'horizontal_loop_begin':
            subst_str = '{} = 1'.format(local_name)
        elif stdname == 'horizontal_loop_end':
            evar = self.find_variable('horizontal_loop_extent', any_scope=False)
            if evar:
                ename = evar.get_prop_value('local_name')
                subst_str = '{} = {}'.format(local_name, ename)
            else:
                raise ParseInternalError("Unable to create value for {}".format(local_name))
            # End if
        else:
            raise ParseInternalError("Unknown loop subst standard name, '{}'".format(stdname))
        # End if
        return subst_str

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

    def __del__(self):
        try:
            for key in self.keys():
                del self[key]
            # End for
        except Exception as e:
            pass # python does not guarantee object state during finalization
        # End try

    @classmethod
    def loop_var_match(cls, standard_name):
        'Return True iff <standard_name> is a loop variable'
        return standard_name in cls.__ccpp_loop_vars__

    @classmethod
    def loop_subst_match(cls, standard_name):
        'Return a loop substitution match, if any, for <standard_name>'
        if standard_name in cls.__ccpp_loop_subst__:
            return cls.__ccpp_loop_subst__[standard_name]
        else:
            return None
        # End if

    def find_loop_subst(self, standard_name, any_scope=True, context=None):
        """If <standard_name> is of the form <standard_name>_extent and that
        variable is not in the dictionary, substitute a tuple of variables,
        (<standard_name>_begin, <standard_name>_end), if those variables are
        in the dictionary.
        If <standard_name>_extent *is* present, return that variable as a
        range, ('ccpp_constant_one', <standard_name>_extent)
        In other cases, return None
        """
        loop_var = VarDictionary.loop_subst_match(standard_name)
        logger_str = None
        if loop_var is not None:
            # Let us see if we can fix a loop variable
            dict_var = self.find_variable(standard_name,
                                          any_scope=any_scope, loop_subst=False)
            if dict_var is not None:
                var_one = CCPP_STANDARD_VARS['ccpp_constant_one']
                my_var = (var_one, dict_var)
                if self._logger is not None:
                    logger_str = "loop_subst: found {}{}".format(standard_name, context_string(context))
                # End if
            else:
                my_vars = [self.find_variable(x) for x in loop_var]
                if None not in my_vars:
                    my_var = tuple(my_vars)
                    if self._logger is not None:
                        names = [x.get_prop_value('local_name') for x in my_vars]
                        logger_str = "loop_subst: {} ==> (){}".format(standard_name, ', '.join(names), context_string(context))
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
        loop_var = standard_name in VarDictionary.__ccpp_dim_subst__
        logger_str = None
        if loop_var:
            # Let us see if we can replace the variable
            dim_name = VarDictionary.__ccpp_dim_subst__[standard_name]
            my_var = self.find_variable(dim_name, any_scope=any_scope)
            if my_var is None:
                raise CCPPError("Dimension variable, {} not found{}".format(dim_name, context_string(context)))
            # End if
        else:
            my_var = None
        # End if
        return my_var

    def find_local_name(self, local_name):
        """Return the variable in this dictionary with local_name,
        <local_name>, or None"""
        lvar = None
        for var in self.variable_list():
            tname = var.get_prop_value('local_name')
            if tname == local_name:
                lvar
                break
            # End if
        # End for
        return lvar

    def new_internal_variable_name(self, prefix=None, max_len=63):
        """Find a new local variable name for this dictionary.
        The new name begins with <prefix>_<self.name> or with <self.name>
        (where <self.name> is this VarDictionary's name) if <prefix> is None.
        The new variable name is kept to a maximum length of <max_len>.
        """
        index = 0
        if prefix is None:
            var_prefix = '{}_var'.format(self.name)
        else:
            var_prefix = '{}_{}_var'.format(prefix, self.name)
        # End if
        varlist = [x for x in self.prop_list('local_name') if var_prefix in x]
        newvar = None
        while newvar is None:
            index = index + 1
            newvar = '{}{}'.format(var_prefix, index)
            if len(newvar) > max_len:
                var_prefix = var_prefix[:-1]
                newvar = None
            elif newvar in varlist:
                newvar = None
            # End if
        # End while
        return newvar

###############################################################################
if __name__ == "__main__":
    import doctest
    doctest.testmod()
